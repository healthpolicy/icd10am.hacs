# Internal utilities for identify_hacs()

# ---------------------------------------------------------------------------
# Sub-category \u2192 cat-column name
# ---------------------------------------------------------------------------

# Convert a hac_table_a sub_category string to its cat* column name.
# E.g. "2.1 Intracranial injury" \u2192 "cat02p1"
#      "14.2 Arrhythmias"        \u2192 "cat14p2"
#      "15.1.1 ..."              \u2192 "cat15p1p1"  (if such a string existed)
.hac_sub_to_cat <- function(sc) {
  nums  <- regmatches(sc, regexpr("^[0-9]+(?:[.][0-9]+)*", sc, perl = TRUE))
  parts <- strsplit(nums, ".", fixed = TRUE)
  vapply(parts, function(p)
    paste0("cat", sprintf("%02d", as.integer(p[1L])),
           paste0("p", p[-1L], collapse = "")),
    character(1L))
}

# ---------------------------------------------------------------------------
# Wildcard-aware code matching
# ---------------------------------------------------------------------------

# Match character vector x against a criteria data frame (cols: code, is_wildcard).
# Wildcard codes are prefix-matched with startsWith(); others use %in%.
.hac_match_codes <- function(x, criteria_df) {
  exact  <- criteria_df$code[!criteria_df$is_wildcard]
  wilds  <- criteria_df$code[criteria_df$is_wildcard]
  result <- x %in% exact
  for (p in wilds) result <- result | startsWith(x, p)
  result
}

# ---------------------------------------------------------------------------
# Shared stage helpers
# ---------------------------------------------------------------------------

# Returns a character vector of episode IDs where any proc in proc_vect is present.
.hac_proc_search <- function(proc_df, proc_vect) {
  unique(proc_df[["fn_id"]][proc_df[["proc_var"]] %in% proc_vect])
}

# Returns a tibble(fn_id, <hac_cat> = 1L) for IDs in proc_ids that have a
# matching diagnosis satisfying onset_mult and denom_mult.
.hac_diag_by_proc <- function(diag_df, proc_ids, hac_cat, cats_list) {
  sub  <- diag_df[diag_df[["fn_id"]] %in% proc_ids, ]
  ok   <- (sub[["diag_var"]] %in% cats_list[[hac_cat]]) &
    as.logical(sub[["onset_mult"]]) & as.logical(sub[["denom_mult"]])
  ids  <- sort(unique(sub[["fn_id"]][ok]))
  dout <- tibble::tibble(fn_id = ids)
  dout[[hac_cat]] <- 1L
  dout
}

# Build a parse()-able OR expression for cat* columns matching a regex.
# E.g. .hac_or_expr("01p.$", df) \u2192 expression(cat01p1 | cat01p2 | ...)
.hac_or_expr <- function(rgx, df) {
  vars <- sort(grep(paste0("^cat", rgx), names(df), value = TRUE))
  parse(text = paste0(vars, collapse = "|"))
}

# ---------------------------------------------------------------------------
# Stage 0: normalise episode and diagnosis/procedure data
# ---------------------------------------------------------------------------

# Returns list(din, diag_long, proc_long).  All downstream stage functions
# consume these three objects.
.hac_normalise <- function(df, id_str, format,
                            df_diag, df_proc,
                            diag_col, diagno_col, cof_col, proc_col,
                            drg, qualified_days, caretype,
                            admmode, los, sameday) {

  # Episode-level fields
  din <- df |>
    dplyr::rename(
      fn_id          = !!rlang::sym(id_str),
      drg_ra         = !!rlang::sym(drg),
      caretype       = !!rlang::sym(caretype),
      qualified_days = !!rlang::sym(qualified_days),
      samedayf       = !!rlang::sym(sameday),
      admmode        = !!rlang::sym(admmode),
      los            = !!rlang::sym(los)
    ) |>
    dplyr::mutate(
      dplyr::across(c(admmode, los, samedayf, qualified_days), as.integer),
      caretype_int = as.integer(caretype),
      caretype     = as.character(caretype),
      caretype     = dplyr::case_when(
        caretype != "7"        ~ caretype,
        is.na(qualified_days)  ~ "7.3",
        qualified_days == 0L   ~ "7.3",
        qualified_days >= los  ~ "7.1",
        qualified_days < los   ~ "7.2",
        TRUE                   ~ caretype
      ),
      denom_mult = dplyr::case_when(
        caretype %in% c("7.3", "9", "10")                ~ 0L,
        drg_ra %in% c("R63Z", "L61Z") & samedayf == 1L  ~ 0L,
        TRUE                                              ~ 1L
      )
    ) |>
    dplyr::select(fn_id, drg_ra, admmode, caretype, caretype_int,
                  qualified_days, los, samedayf, denom_mult)

  # Wide-to-long conversion when input is wide-format
  if (format == "wide") {
    long       <- icd10am.utils::.to_long(df, !!rlang::sym(id_str),
                                          diag_col, proc_col, cof_col)
    df_diag    <- long$df_diag
    df_proc    <- long$df_proc
    diag_col   <- "diag"
    diagno_col <- "diagno"
    cof_col    <- if (!is.null(cof_col)) "cof" else NULL
    proc_col   <- "proc"
  }

  # Long-format diagnoses with COF and episode fields attached
  diag_long <- df_diag |>
    dplyr::rename(
      fn_id    = !!rlang::sym(id_str),
      diag_var = !!rlang::sym(diag_col),
      diagno   = !!rlang::sym(diagno_col)
    ) |>
    dplyr::mutate(
      diag_var = gsub("\\.", "", .data$diag_var),
      diagno   = as.integer(.data$diagno)
    ) |>
    dplyr::filter(!is.na(.data$diag_var), .data$diag_var != "")

  if (is.null(cof_col)) {
    diag_long <- diag_long |>
      dplyr::mutate(
        cof      = as.integer(stringr::str_sub(.data$diag_var, 1L, 1L)),
        diag_var = stringr::str_sub(.data$diag_var, 2L, -1L)
      )
  } else {
    diag_long <- diag_long |>
      dplyr::rename(cof = !!rlang::sym(cof_col)) |>
      dplyr::mutate(cof = as.integer(.data$cof))
  }

  diag_long <- diag_long |>
    dplyr::left_join(
      dplyr::select(din, fn_id, caretype, caretype_int, admmode, denom_mult),
      by = "fn_id"
    ) |>
    dplyr::mutate(
      onset_mult = dplyr::case_when(
        diagno == 1L & !caretype %in% c("7.1", "7.2") ~ 0L,
        cof == 1L                                      ~ 1L,
        TRUE                                           ~ 0L
      )
    )

  # Long-format procedures
  proc_long <- df_proc |>
    dplyr::rename(
      fn_id    = !!rlang::sym(id_str),
      proc_var = !!rlang::sym(proc_col)
    ) |>
    dplyr::filter(!is.na(.data$proc_var), .data$proc_var != "") |>
    dplyr::mutate(proc_var = stringr::str_replace(.data$proc_var, "-", ""))

  list(din = din, diag_long = diag_long, proc_long = proc_long)
}

# ---------------------------------------------------------------------------
# Finalise: hac1-hac16 flags, hac_array, join to original df
# ---------------------------------------------------------------------------

.hac_finalise <- function(hac_cats, din, df, id_str, verbose = FALSE) {

  hac_output <- dplyr::full_join(
      hac_cats,
      dplyr::select(din, fn_id, drg_ra),
      by = "fn_id"
    ) |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("cat"), as.integer)) |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("cat"),
                                ~ tidyr::replace_na(.x, 0L)))

  hac_output <- hac_output |>
    dplyr::mutate(
      hac1  = eval(.hac_or_expr("01p.$",      hac_output)),
      hac2  = eval(.hac_or_expr("02p.$",      hac_output)),
      hac3  = eval(.hac_or_expr("03p.$",      hac_output)),
      hac4  = eval(.hac_or_expr("04p.$",      hac_output)),
      hac6  = eval(.hac_or_expr("06p.$",      hac_output)),
      hac7  = eval(.hac_or_expr("07p.$",      hac_output)),
      hac8  = eval(.hac_or_expr("08p.$",      hac_output)),
      hac9  = eval(.hac_or_expr("09p.$",      hac_output)),
      hac10 = eval(.hac_or_expr("10p.$",      hac_output)),
      hac11 = eval(.hac_or_expr("11p.$",      hac_output)),
      hac12 = eval(.hac_or_expr("12p.$",      hac_output)),
      hac13 = eval(.hac_or_expr("13p.$",      hac_output)),
      hac14 = eval(.hac_or_expr("14p.(p.)?$", hac_output)),
      hac15 = eval(.hac_or_expr("15p.(p.)?$", hac_output)),
      hac15p1 = cat15p1p1,
      hac15p2 = cat15p1p2,
      hac16 = eval(.hac_or_expr("16p.$",      hac_output)),
      dplyr::across(dplyr::matches("hac[0-9]{1,2}$"), as.integer)
    )

  # hac_array: comma-separated string of all detected HAC categories per episode
  hac_array <- hac_output |>
    dplyr::select(fn_id, hac1:hac16) |>
    tidyr::pivot_longer(
      -fn_id,
      names_to      = c(NA, "hac"),
      names_pattern = "(hac)(.+)"
    ) |>
    dplyr::filter(value == 1L) |>
    dplyr::group_by(fn_id) |>
    dplyr::summarise(hac_array = paste0(hac, collapse = ","), .groups = "drop")

  hac_output <- hac_output |>
    dplyr::left_join(hac_array, by = "fn_id")

  if (verbose) {
    any_hac <- rowSums(
      hac_output[grep("^hac[0-9]{1,2}$", names(hac_output))], na.rm = TRUE
    ) > 0L
    n_hac <- sum(any_hac, na.rm = TRUE)
    n_tot <- nrow(hac_output)
    message(sprintf(
      "  -- %d / %d episodes with any HAC (%.1f%%)",
      n_hac, n_tot, 100 * n_hac / max(n_tot, 1L)
    ))
  }

  names(hac_output)[names(hac_output) == "fn_id"] <- id_str
  df |> dplyr::left_join(hac_output, by = id_str)
}

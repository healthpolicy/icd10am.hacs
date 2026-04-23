# Internal implementation of HAC classification for specification versions 3.x.
# Called by identify_hacs() after normalisation.
#
# Returns a tibble(fn_id, cat*) with one column per HAC sub-category flag.
# Conditional categories (those requiring additional criteria beyond the base
# diagnosis map) are computed by the numbered stages and override Stage 1.

.hac_run_v3 <- function(diag_long, proc_long, din,
                         hac_version, icd10am_version, verbose = FALSE) {

  # -- Setup ------------------------------------------------------------------

  icd_ed_col   <- sprintf("icd10am_%02d", icd10am_version)
  hac_flag_col <- paste0("hac_", hac_version)
  ver_str      <- paste0("v", gsub("_", ".", sub("^v", "", hac_version)))

  # Codes active for this HAC version and ICD edition
  dx_active <- hac_table_a |>
    dplyr::filter(
      icd_edition == icd_ed_col,
      .data[[hac_flag_col]] == TRUE
    ) |>
    dplyr::mutate(cat_name = .hac_sub_to_cat(sub_category))

  # Wide binary diagnosis map: DDX column + one 0/1 column per cat_name
  diagnosis_map_df <- dx_active |>
    dplyr::select(code_nodot, cat_name) |>
    dplyr::distinct() |>
    dplyr::mutate(value = 1L) |>
    tidyr::pivot_wider(
      names_from  = cat_name,
      values_from = value,
      values_fill = 0L
    ) |>
    dplyr::rename(DDX = code_nodot)

  conditions <- setdiff(names(diagnosis_map_df), "DDX")

  # Named list: cat_name \u2192 code vector (used in .hac_diag_by_proc and stage lookups)
  cats_list <- dx_active |>
    dplyr::group_by(cat_name) |>
    dplyr::summarise(codes = list(code_nodot), .groups = "drop") |>
    tibble::deframe()

  # Conditional cat names: removed from Stage 1 output, computed by later stages.
  # cat14p2 is handled by Stage 14; cat15p1 by Stage 9; cat16p1/p2 by Stage 10.
  COND_CATS <- c(
    "cat02p1", "cat02p2", "cat02p3",      # falls - stage 2
    "cat04p1",                             # haematoma - stage 3
    "cat04p3",                             # anastomotic leak - stage 4
    "cat06p1",                             # ventilation - stage 6
    "cat08p1",                             # dialysis - stage 7
    "cat10p1", "cat10p4",                 # drug reactions - stage 8
    "cat14p2",                             # arrhythmias - stage 14
    "cat15p1", "cat15p1p1", "cat15p1p2",  # perineal - stage 9
    "cat16p1", "cat16p2"                  # neonatal - stage 10
  )
  cond_cats <- intersect(COND_CATS, conditions)

  # Stage criteria filtered to this HAC version
  crit_ver <- hac_stage_criteria |> dplyr::filter(version_id == ver_str)

  # Shorthand: pull codes matching a sub_category prefix and role
  .crit <- function(sub_prefix, role_val) {
    crit_ver |>
      dplyr::filter(
        grepl(sub_prefix, sub_category, fixed = TRUE),
        role == role_val
      )
  }

  if (verbose) {
    edition_ord <- as.integer(sub("icd10am_", "", icd_ed_col))
    message(sprintf(
      "  identify_hacs: %s | ICD-10-AM %dth ed | N = %d",
      ver_str, edition_ord, nrow(din)
    ))
  }

  ####
  ####  STAGE 1: Map diagnoses to HAC categories (no additional conditions)
  ####

  s1 <- dplyr::left_join(
      diag_long,
      diagnosis_map_df,
      by = c(diag_var = "DDX")
    ) |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(conditions), ~ tidyr::replace_na(.x, 0L))
    ) |>
    dplyr::filter(
      dplyr::if_any(dplyr::all_of(conditions), ~ .x > 0L)
    ) |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(conditions), ~ .x * onset_mult * denom_mult)
    ) |>
    dplyr::group_by(fn_id) |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(conditions), ~ max(.x, na.rm = TRUE)),
      .groups = "drop"
    )

  if (verbose) {
    message(sprintf(
      "  Stage  1 diagnosis map          : %d episode-dx matches",
      nrow(s1)
    ))
  }

  ####
  ####  STAGE 2: Falls (HAC 2 - cat02p1/p2/p3)
  ####  Additional condition: a fall external cause code must appear AFTER
  ####  the fracture/intracranial diagnosis (higher diagno position).
  ####  Falls criteria apply to all 2.x sub-categories; the spec attaches
  ####  them to sub_category "2.1" but they are shared across 2.1-2.3.
  ####

  falls_crit <- .crit("2.", "falls_diag")

  s2_before <- c(
    cat02p1 = sum(s1$cat02p1 > 0L, na.rm = TRUE),
    cat02p2 = sum(s1$cat02p2 > 0L, na.rm = TRUE),
    cat02p3 = sum(s1$cat02p3 > 0L, na.rm = TRUE)
  )

  s2_ep <- s1$fn_id[
    (if ("cat02p1" %in% names(s1)) s1$cat02p1 == 1L else FALSE) |
    (if ("cat02p2" %in% names(s1)) s1$cat02p2 == 1L else FALSE) |
    (if ("cat02p3" %in% names(s1)) s1$cat02p3 == 1L else FALSE)
  ]

  s2_sub <- diag_long |> dplyr::filter(fn_id %in% s2_ep, cof == 1L)

  if (nrow(s2_sub) > 0L && nrow(falls_crit) > 0L) {
    s2 <- s2_sub |>
      dplyr::select(fn_id, diag_var, diagno, onset_mult, denom_mult) |>
      dplyr::mutate(
        cat02p1  = ifelse(diag_var %in% (cats_list[["cat02p1"]] %||% character(0L)),
                          diagno * onset_mult * denom_mult, 0L),
        cat02p2  = ifelse(diag_var %in% (cats_list[["cat02p2"]] %||% character(0L)),
                          diagno * onset_mult * denom_mult, 0L),
        cat02p3  = ifelse(diag_var %in% (cats_list[["cat02p3"]] %||% character(0L)),
                          diagno * onset_mult * denom_mult, 0L),
        place_pos = 99L,  # place-of-occurrence check removed in v3.x
        fall_pos  = diagno * .hac_match_codes(diag_var, falls_crit)
      ) |>
      dplyr::group_by(fn_id) |>
      dplyr::summarise(
        place_pos = max(place_pos),
        cat02p1   = max(cat02p1),
        cat02p2   = max(cat02p2),
        cat02p3   = max(cat02p3),
        fall_pos  = max(fall_pos),
        .groups   = "drop"
      ) |>
      dplyr::mutate(
        dplyr::across(cat02p1:cat02p3,
                      ~ as.integer((.x > 1L) & (.x < fall_pos) & (.x < place_pos)))
      ) |>
      dplyr::select(fn_id, cat02p1, cat02p2, cat02p3)
  } else {
    s2 <- tibble::tibble(
      fn_id   = character(0L),
      cat02p1 = integer(0L),
      cat02p2 = integer(0L),
      cat02p3 = integer(0L)
    )
  }

  if (verbose) {
    s2_after <- c(
      cat02p1 = sum(s2$cat02p1 > 0L, na.rm = TRUE),
      cat02p2 = sum(s2$cat02p2 > 0L, na.rm = TRUE),
      cat02p3 = sum(s2$cat02p3 > 0L, na.rm = TRUE)
    )
    message(sprintf(
      "  Stage  2 falls (HAC 2)          : cat02p1 %d\u2192%d, cat02p2 %d\u2192%d, cat02p3 %d\u2192%d",
      s2_before["cat02p1"], s2_after["cat02p1"],
      s2_before["cat02p2"], s2_after["cat02p2"],
      s2_before["cat02p3"], s2_after["cat02p3"]
    ))
  }

  ####
  ####  STAGE 3: Postoperative haemorrhage/haematoma (cat04p1)
  ####  Additional condition: haematoma-related procedure code present
  ####

  s3_procs <- .crit("4.1", "inclusion_proc") |> dplyr::pull(code)
  s3_pin   <- .hac_proc_search(proc_long, s3_procs)
  s3       <- .hac_diag_by_proc(diag_long, s3_pin, "cat04p1", cats_list)

  if (verbose) {
    message(sprintf(
      "  Stage  3 haematoma (HAC 4.1)    : cat04p1 %d  (%d with proc)",
      nrow(s3), length(s3_pin)
    ))
  }

  ####
  ####  STAGE 4: Anastomotic leak (cat04p3)
  ####  Additional condition: Y832 must appear AFTER the HAC diagnosis
  ####

  s4_ep <- if ("cat04p3" %in% names(s1)) s1$fn_id[s1$cat04p3 == 1L] else character(0L)

  s4 <- diag_long |>
    dplyr::filter(fn_id %in% s4_ep) |>
    dplyr::select(fn_id, onset_mult, denom_mult, diag_var, diagno) |>
    dplyr::mutate(
      cat04p3            = ifelse(diag_var %in% (cats_list[["cat04p3"]] %||% character(0L)) &
                                    onset_mult == 1L & denom_mult == 1L, 1L, 0L),
      location           = ifelse(cat04p3 == 1L, diagno, 9999L),
      cat04p3_mult_final = 0L
    ) |>
    dplyr::group_by(fn_id) |>
    dplyr::mutate(location = min(location, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      cat04p3_mult_final = ifelse(diag_var == "Y832" & diagno > location, 1L, 0L)
    ) |>
    dplyr::group_by(fn_id) |>
    dplyr::summarise(
      cat04p3 = max(cat04p3_mult_final, 0L, na.rm = TRUE),
      .groups = "drop"
    )

  if (verbose) {
    message(sprintf(
      "  Stage  4 anastomotic leak (4.3) : cat04p3 %d",
      sum(s4$cat04p3 > 0L, na.rm = TRUE)
    ))
  }

  ####
  ####  STAGE 5: Unplanned return to theatre - not implemented (no NMDS data item)
  ####

  ####
  ####  STAGE 6: Respiratory failure (cat06p1)
  ####  Additional condition: mechanical ventilation procedure present
  ####

  s6_procs <- .crit("6.1", "inclusion_proc") |> dplyr::pull(code)
  s6_pin   <- .hac_proc_search(proc_long, s6_procs)
  s6       <- .hac_diag_by_proc(diag_long, s6_pin, "cat06p1", cats_list)

  if (verbose) {
    message(sprintf(
      "  Stage  6 ventilation (HAC 6.1)  : cat06p1 %d  (%d with proc)",
      nrow(s6), length(s6_pin)
    ))
  }

  ####
  ####  STAGE 7: Renal failure (cat08p1)
  ####  Additional condition: haemodialysis procedure; excludes pre-existing CKD 4/5
  ####

  s7_procs <- .crit("8.1", "inclusion_proc") |> dplyr::pull(code)
  s7_pin   <- .hac_proc_search(proc_long, s7_procs)
  s7_excl  <- .crit("8.1", "exclusion_diag") |> dplyr::pull(code)

  s7_ep <- if ("cat08p1" %in% names(s1)) s1$fn_id[s1$cat08p1 == 1L] else character(0L)

  s7 <- diag_long |>
    dplyr::filter(
      fn_id %in% s7_ep,
      fn_id %in% s7_pin
    ) |>
    dplyr::group_by(fn_id) |>
    dplyr::summarise(
      cat08p1 = as.integer(!any(diag_var %in% s7_excl)),
      .groups = "drop"
    )

  if (verbose) {
    n_excl <- length(s7_pin) - nrow(s7)
    message(sprintf(
      "  Stage  7 dialysis (HAC 8.1)     : cat08p1 %d  (%d with proc, %d excl CKD)",
      sum(s7$cat08p1 > 0L, na.rm = TRUE), length(s7_pin), max(n_excl, 0L)
    ))
  }

  ####
  ####  STAGE 8: Drug-related complications (cat10p1, cat10p4)
  ####  Additional condition: matching external cause code must appear AFTER
  ####  the HAC diagnosis (higher diagno position).
  ####  cat10p5 is unconditional in v3.x and flows directly from Stage 1.
  ####

  ext_crit_10p1 <- .crit("10.1", "external_diag")
  ext_crit_10p4 <- .crit("10.4", "external_diag")

  s8_diags <- c(
    cats_list[["cat10p1"]] %||% character(0L),
    cats_list[["cat10p4"]] %||% character(0L)
  )

  s8_base <- diag_long[
    diag_long$denom_mult == 1L & diag_long$cof == 1L &
      diag_long$diag_var %in% s8_diags, ]
  s8_base <- diag_long[diag_long$fn_id %in% s8_base$fn_id, ]

  .s8_match_ext <- function(diag_vec, crit_df) {
    .hac_match_codes(diag_vec, dplyr::select(crit_df, code, is_wildcard))
  }

  c10p1_ext <- s8_base[.s8_match_ext(s8_base$diag_var, ext_crit_10p1),
                        c("fn_id", "diagno")]
  names(c10p1_ext)[2L] <- "diagno_ext"

  c10p4_ext <- s8_base[.s8_match_ext(s8_base$diag_var, ext_crit_10p4),
                        c("fn_id", "diagno")]
  names(c10p4_ext)[2L] <- "diagno_ext"

  .s8_flagged <- function(base, ext, cat_codes) {
    if (nrow(ext) == 0L) return(character(0L))
    joined <- dplyr::inner_join(base, ext, by = "fn_id")
    unique(dplyr::filter(
      joined,
      cof == 1L,
      diagno < diagno_ext,
      diag_var %in% cat_codes,
      diagno != 1L | caretype %in% c("7.1", "7.2")
    )$fn_id)
  }

  c10p1_ids <- .s8_flagged(s8_base, c10p1_ext, cats_list[["cat10p1"]] %||% character(0L))
  c10p4_ids <- .s8_flagged(s8_base, c10p4_ext, cats_list[["cat10p4"]] %||% character(0L))

  s8 <- tibble::tibble(fn_id = c10p1_ids, cat10p1 = 1L) |>
    dplyr::full_join(tibble::tibble(fn_id = c10p4_ids, cat10p4 = 1L), by = "fn_id") |>
    dplyr::distinct()
  s8[is.na(s8)] <- 0L

  if (verbose) {
    message(sprintf(
      "  Stage  8 drug reactions (HAC 10): cat10p1 %d, cat10p4 %d",
      length(c10p1_ids), length(c10p4_ids)
    ))
  }

  ####
  ####  STAGE 9: Perineal laceration during delivery (cat15p1p1, cat15p1p2)
  ####  Vaginal births only; excludes admissions from another facility
  ####  and certain care types; caesarean episodes excluded by proc code.
  ####
  ####  HAC 15 codes from hac_table_b: O702 = 3rd degree (cat15p1p1),
  ####                                   O703 = 4th degree (cat15p1p2)
  ####  Denominator (vaginal birth): hac_table_c
  ####  Caesarean exclusion (ACHI):  hac_table_d
  ####

  b_codes <- hac_table_b |>
    dplyr::filter(icd_edition == icd_ed_col, .data[[hac_flag_col]] == TRUE) |>
    dplyr::pull(code_nodot) |> sort()

  perineal_p1p1 <- if (length(b_codes) >= 1L) b_codes[1L] else character(0L)  # O702
  perineal_p1p2 <- if (length(b_codes) >= 2L) b_codes[2L] else character(0L)  # O703

  vaginal_codes <- hac_table_c |>
    dplyr::filter(icd_edition == icd_ed_col, .data[[hac_flag_col]] == TRUE) |>
    dplyr::pull(code_nodot)

  caesarean_procs <- gsub(
    "-", "",
    hac_table_d |>
      dplyr::filter(icd_edition == icd_ed_col, .data[[hac_flag_col]] == TRUE) |>
      dplyr::pull(achi_code)
  )

  s9_numerator <- c(perineal_p1p1, perineal_p1p2)
  s9_ep <- unique(diag_long$fn_id[
    diag_long$diag_var %in% s9_numerator & diag_long$diagno != 1L
  ])

  s9_sub <- diag_long[
    diag_long$fn_id %in% s9_ep &
      diag_long$admmode != 1L &
      !(diag_long$caretype %in% c("10", "9", "7.3")), ]

  vaginal_ids   <- unique(s9_sub$fn_id[s9_sub$diag_var %in% vaginal_codes])
  caesarean_ids <- .hac_proc_search(proc_long, caesarean_procs)

  s9_keep <- s9_sub[
    s9_sub$fn_id %in% vaginal_ids & !(s9_sub$fn_id %in% caesarean_ids), ]

  s9 <- s9_keep |>
    dplyr::group_by(fn_id) |>
    dplyr::summarise(
      cat15p1p1 = as.integer(any(diag_var %in% perineal_p1p1)),
      cat15p1p2 = as.integer(any(diag_var %in% perineal_p1p2)),
      .groups   = "drop"
    )

  # Ensure columns exist even with 0 rows
  if (nrow(s9) == 0L) {
    s9 <- tibble::tibble(
      fn_id     = character(0L),
      cat15p1p1 = integer(0L),
      cat15p1p2 = integer(0L)
    )
  }

  if (verbose) {
    message(sprintf(
      "  Stage  9 perineal (HAC 15)      : cat15p1p1 %d, cat15p1p2 %d",
      sum(s9$cat15p1p1 > 0L, na.rm = TRUE),
      sum(s9$cat15p1p2 > 0L, na.rm = TRUE)
    ))
  }

  ####
  ####  STAGE 10: Neonatal birth trauma (cat16p1, cat16p2)
  ####  Newborns only (caretype == 7); excludes transfers and preterm/low-weight
  ####

  neonatal_codes <- hac_table_e |>
    dplyr::filter(icd_edition == icd_ed_col, .data[[hac_flag_col]] == TRUE) |>
    dplyr::mutate(cat_name = .hac_sub_to_cat(sub_category)) |>
    dplyr::select(code_nodot, cat_name)

  exclusion_codes <- hac_table_f |>
    dplyr::filter(icd_edition == icd_ed_col, .data[[hac_flag_col]] == TRUE) |>
    dplyr::pull(code_nodot)

  s10_sub <- diag_long[diag_long$caretype_int == 7L & diag_long$admmode != 1L, ]
  excl_ids <- unique(s10_sub$fn_id[s10_sub$diag_var %in% exclusion_codes])
  s10_sub  <- s10_sub[!s10_sub$fn_id %in% excl_ids, ]

  cat16p1_codes <- neonatal_codes$code_nodot[neonatal_codes$cat_name == "cat16p1"]
  cat16p2_codes <- neonatal_codes$code_nodot[neonatal_codes$cat_name == "cat16p2"]

  c16p1_ids <- unique(s10_sub$fn_id[s10_sub$diag_var %in% cat16p1_codes])
  c16p2_ids <- unique(s10_sub$fn_id[s10_sub$diag_var %in% cat16p2_codes])

  s10 <- tibble::tibble(fn_id = c16p1_ids, cat16p1 = 1L) |>
    dplyr::full_join(tibble::tibble(fn_id = c16p2_ids, cat16p2 = 1L), by = "fn_id") |>
    dplyr::distinct()

  if (nrow(s10) == 0L) {
    s10 <- tibble::tibble(
      fn_id   = character(0L),
      cat16p1 = integer(0L),
      cat16p2 = integer(0L)
    )
  }

  if (verbose) {
    message(sprintf(
      "  Stage 10 neonatal (HAC 16)      : cat16p1 %d, cat16p2 %d",
      length(c16p1_ids), length(c16p2_ids)
    ))
  }

  ####
  ####  STAGE 14: Arrhythmias (cat14p2)
  ####  In v3.x, ALL cat14p2 codes flow from Stage 1 EXCEPT R001 (bradycardia),
  ####  which is split:
  ####    cat14p2p1 = R001 without a pacing procedure
  ####    cat14p2p2 = R001 WITH a pacing procedure
  ####  Other arrhythmia codes (I470, I471, etc.) keep their Stage 1 cat14p2 flag.
  ####

  s14_procs <- .crit("14.2", "inclusion_proc") |>
    dplyr::filter(!is.na(applying_to_code), applying_to_code == "R001") |>
    dplyr::pull(code)

  s14_pin <- .hac_proc_search(proc_long, s14_procs)

  all_14p2_codes  <- cats_list[["cat14p2"]] %||% character(0L)
  brady_code      <- "R001"
  other_14p2      <- setdiff(all_14p2_codes, brady_code)

  # Episodes with R001 (onset, denom-eligible)
  brady_ep <- s1$fn_id[
    if ("cat14p2" %in% names(s1)) s1$cat14p2 == 1L else FALSE
  ]
  brady_sub <- diag_long[
    diag_long$fn_id %in% brady_ep &
      diag_long$diag_var == brady_code &
      diag_long$onset_mult == 1L &
      diag_long$denom_mult == 1L, ]

  brady_ids      <- unique(brady_sub$fn_id)
  brady_with_proc <- intersect(brady_ids, s14_pin)
  brady_no_proc   <- setdiff(brady_ids, s14_pin)

  # Episodes with other arrhythmia codes (unconditional in v3.x)
  other_ep <- s1$fn_id[
    if ("cat14p2" %in% names(s1)) s1$cat14p2 == 1L else FALSE
  ]
  other_14p2_ep <- diag_long |>
    dplyr::filter(
      fn_id %in% other_ep,
      diag_var %in% other_14p2,
      onset_mult == 1L, denom_mult == 1L
    ) |>
    dplyr::pull(fn_id) |>
    unique()

  s14 <- tibble::tibble(fn_id = brady_no_proc,   cat14p2p1 = 1L) |>
    dplyr::full_join(tibble::tibble(fn_id = brady_with_proc, cat14p2p2 = 1L), by = "fn_id") |>
    dplyr::full_join(tibble::tibble(fn_id = other_14p2_ep,  cat14p2   = 1L), by = "fn_id") |>
    dplyr::distinct()
  s14[is.na(s14)] <- 0L

  if (verbose) {
    message(sprintf(
      "  Stage 14 arrhythmias (HAC 14.2) : R001 no-proc (cat14p2p1) %d, R001+proc (cat14p2p2) %d, other (cat14p2) %d",
      length(brady_no_proc), length(brady_with_proc), length(other_14p2_ep)
    ))
  }

  ####
  ####  Join all stages
  ####

  # Remove conditional cats from s1 - owned by their respective stages
  s1 <- s1 |> dplyr::select(-dplyr::any_of(cond_cats))

  s2 |>
    dplyr::full_join(s3,  by = "fn_id") |>
    dplyr::full_join(s4,  by = "fn_id") |>
    dplyr::full_join(s6,  by = "fn_id") |>
    dplyr::full_join(s7,  by = "fn_id") |>
    dplyr::full_join(s8,  by = "fn_id") |>
    dplyr::full_join(s9,  by = "fn_id") |>
    dplyr::full_join(s10, by = "fn_id") |>
    dplyr::full_join(s14, by = "fn_id") |>
    dplyr::full_join(s1,  by = "fn_id")
}

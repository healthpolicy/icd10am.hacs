#' Identify Hospital Acquired Complications (HACs)
#'
#' @description
#' Identifies Hospital Acquired Complications (HACs) in admitted patient
#' episodes coded with ICD-10-AM, using the Australian Commission on Safety
#' and Quality in Health Care (ACSQHC) HAC classification.
#'
#' Both wide-format and long-format inputs are supported via the `format`
#' argument.
#'
#' @param df A data frame with one row per episode.
#' @param id The episode identifier column (unquoted name).
#' @param format Input format: `"long"` (default) or `"wide"`.
#' @param version HAC specification version string (e.g. `"3.2.2"`). Defaults
#'   to `NULL`, which selects the latest available version. Earlier versions
#'   remain available for reproducibility.
#' @param df_diag Long-format diagnosis data frame. Required when
#'   `format = "long"`.
#' @param df_proc Long-format procedure data frame. Required when
#'   `format = "long"`.
#' @param diag Name of the diagnosis code column in `df_diag` (quoted string).
#'   Default: `"diag"`. Used only when `format = "long"`.
#' @param diagno Name of the diagnosis sequence number column in `df_diag`
#'   (quoted string). Default: `"diagno"`. Used only when `format = "long"`.
#' @param cof Name of the condition onset flag column in `df_diag` (quoted
#'   string), or `NULL` if the COF is embedded as the first character of each
#'   diagnosis code. Default: `NULL`. Used only when `format = "long"`.
#' @param proc Name of the procedure code column in `df_proc` (quoted string).
#'   Default: `"proc"`. Used only when `format = "long"`.
#' @param diag_prefix Prefix shared by all diagnosis columns in `df` (quoted
#'   string). Default: `"diag"`. Used only when `format = "wide"`.
#' @param proc_prefix Prefix shared by all procedure columns in `df` (quoted
#'   string). Default: `"proc"`. Used only when `format = "wide"`.
#' @param cof_prefix Prefix shared by condition onset flag columns in `df`
#'   (quoted string), or `NULL` if COF is embedded as the first character of
#'   each diagnosis code. Default: `NULL`. Used only when `format = "wide"`.
#' @param drg Name of the DRG column in `df` (quoted string). Default: `"drg"`.
#' @param icd10am_version ICD-10-AM edition used for coding (integer).
#'   Default: `13L`.
#' @param caretype Name of the care type column in `df` (quoted string).
#'   Default: `"caretype"`.
#' @param qualified_days Name of the qualified days column in `df` (quoted
#'   string). Default: `"qdays"`.
#' @param admmode Name of the admission mode column in `df` (quoted string).
#'   Default: `"admmode"`.
#' @param los Name of the length of stay column in `df` (quoted string).
#'   Default: `"los"`.
#' @param sameday Name of the same-day flag column in `df` (quoted string).
#'   Default: `"samedayf"`.
#' @param verbose Logical. If `TRUE`, print a stage-by-stage processing summary
#'   to the console via `message()`. Default: `FALSE`.
#'
#' @return The input `df` with additional columns:
#'   - One binary flag per HAC sub-category (`cat*`)
#'   - One summary flag per HAC category (`hac1`--`hac16`)
#'   - `hac15p1`, `hac15p2` -- sub-flags for 3rd/4th degree perineal tear
#'   - `hac_array` -- comma-separated string of all detected HAC categories
#'     (e.g. `"2,15p1"`). `NA` for episodes with no HAC.
#'
#' @references
#' Australian Commission on Safety and Quality in Health Care.
#' *Hospital Acquired Complications*. Sydney: ACSQHC.
#' <https://www.safetyandquality.gov.au/our-work/patient-safety/hospital-acquired-complications>
#'
#' @export
#'
#' @importFrom dplyr across all_of any_of case_when distinct filter full_join
#'   group_by if_any inner_join left_join matches mutate pull rename select
#'   starts_with summarise ungroup
#' @importFrom tidyr pivot_longer pivot_wider replace_na
#' @importFrom rlang sym ensym as_name .data abort `%||%`
#' @importFrom stringr str_sub str_replace
#' @importFrom tibble tibble deframe
#'
#' @examples
#' \dontrun{
#' library(icd10am.utils)
#'
#' df_hac <- transform(dfbase,
#'   drg = "A01A", caretype = "1", qdays = 2L,
#'   admmode = 2L, los = 5L, samedayf = 0L
#' )
#'
#' result <- identify_hacs(
#'   df             = df_hac,
#'   id             = recordID,
#'   format         = "long",
#'   df_diag        = dfdiag,
#'   df_proc        = dfproc,
#'   diag           = "diag",
#'   diagno         = "diagno",
#'   cof            = "COF",
#'   proc           = "proc",
#'   icd10am_version = 13L,
#'   verbose        = TRUE
#' )
#' }
identify_hacs <- function(
    df,
    id,
    format          = c("long", "wide"),
    version         = NULL,
    df_diag         = NULL,
    df_proc         = NULL,
    diag            = "diag",
    diagno          = "diagno",
    cof             = NULL,
    proc            = "proc",
    diag_prefix     = "diag",
    proc_prefix     = "proc",
    cof_prefix      = NULL,
    drg             = "drg",
    icd10am_version = 13L,
    caretype        = "caretype",
    qualified_days  = "qdays",
    admmode         = "admmode",
    los             = "los",
    sameday         = "samedayf",
    verbose         = FALSE
) {
  format  <- match.arg(format)
  id_str  <- rlang::as_name(rlang::ensym(id))

  # -- Resolve HAC version ---------------------------------------------------
  available_versions <- gsub(
    "^hac_", "",
    grep("^hac_v", names(hac_table_a), value = TRUE)
  )

  if (is.null(version)) {
    hac_version <- available_versions[1L]   # newest first
  } else {
    hac_version <- paste0("v", gsub("\\.", "_", as.character(version)))
    if (!hac_version %in% available_versions) {
      supported <- paste(
        gsub("^v", "", gsub("_", ".", available_versions)),
        collapse = ", "
      )
      rlang::abort(paste0(
        "HAC version '", version, "' is not available. ",
        "Supported versions: ", supported
      ))
    }
  }

  # -- Normalise column names before passing to .hac_normalise() ------------
  diag_col <- if (format == "wide") diag_prefix else diag
  proc_col <- if (format == "wide") proc_prefix else proc
  cof_col  <- if (format == "wide") cof_prefix  else cof

  # -- Stage 0: normalise inputs ---------------------------------------------
  norm <- .hac_normalise(
    df             = df,
    id_str         = id_str,
    format         = format,
    df_diag        = df_diag,
    df_proc        = df_proc,
    diag_col       = diag_col,
    diagno_col     = diagno,
    cof_col        = cof_col,
    proc_col       = proc_col,
    drg            = drg,
    qualified_days = qualified_days,
    caretype       = caretype,
    admmode        = admmode,
    los            = los,
    sameday        = sameday
  )

  # -- Dispatch to version runner --------------------------------------------
  hac_major <- sub("^v([0-9]+)_.*", "\\1", hac_version)

  hac_cats <- switch(
    hac_major,
    "3" = .hac_run_v3(
      diag_long       = norm$diag_long,
      proc_long       = norm$proc_long,
      din             = norm$din,
      hac_version     = hac_version,
      icd10am_version = icd10am_version,
      verbose         = verbose
    ),
    rlang::abort(paste0(
      "No runner implemented for HAC v", hac_major, ".x. ",
      "Currently supported: v3.x"
    ))
  )

  # -- Finalise: summary flags, hac_array, join to original df --------------
  .hac_finalise(hac_cats, norm$din, df, id_str, verbose = verbose)
}

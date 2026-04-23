#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom magrittr %>%
#' @importFrom rlang .data
## usethis namespace: end
NULL

# Suppress R CMD check notes for column names used via dplyr data masking
# and for package datasets referenced directly in function bodies.
utils::globalVariables(c(
  # Package datasets
  "hac_table_a", "hac_table_b", "hac_table_c", "hac_table_d",
  "hac_table_e", "hac_table_f", "hac_stage_criteria",
  # hac_table_a / hac_table_e columns
  "sub_category", "code_nodot", "icd_edition", "cat_name",
  "code", "is_wildcard", "role", "version_id", "applying_to_code",
  # hac_table_d columns
  "achi_code",
  # diag_long / proc_long columns used in dplyr verbs
  "fn_id", "diag_var", "diagno", "cof", "onset_mult", "denom_mult",
  "caretype", "caretype_int", "admmode", "drg_ra", "samedayf",
  # intermediate mutate variables
  "cat02p1", "cat02p2", "cat02p3", "cat04p3", "cat04p3_mult_final",
  "cat15p1p1", "cat15p1p2",
  "fall_pos", "place_pos", "location",
  "diagno_ext",
  # hac_array pivot columns
  "hac", "hac1", "hac16",
  # hac_stage_criteria columns (already listed above but explicit for clarity)
  "value"
))

#' HAC numerator codes (HAC 1-14)
#'
#' Main code list for Hospital Acquired Complications 1-14, sourced from the
#' ACSQHC HAC specification Excel files. One row per ICD-10-AM code x
#' ICD edition combination.
#'
#' @format A data frame with 15 columns. Fixed columns:
#'   `complication` (HAC category name), `sub_category`, `role`
#'   (`"numerator"` or `"exclusion"`), `code_nodot`, `code_dot`,
#'   `description`, `icd_edition` (e.g. `"icd10am_13"`).
#'   One logical column per HAC version (newest first):
#'   `hac_v3_2_2`, `hac_v3_2_1`, `hac_v3_1_2`, `hac_v3_1_1`,
#'   `hac_v2_0`, `hac_v1_1_2`, `hac_v1_1_1`, `hac_v1_0`;
#'   `TRUE` if the code is active in that version.
#' @source Australian Commission on Safety and Quality in Health Care (ACSQHC).
#'   HAC specification Excel files, v1.0-v3.2.2.
"hac_table_a"

#' HAC 15 perineal laceration numerator codes
#'
#' ICD-10-AM codes for the HAC 15 numerator (third and fourth degree perineal
#' laceration during delivery). One row per code x ICD edition.
#'
#' @format Same column structure as \code{hac_table_a}.
#' @source ACSQHC HAC specification Excel files.
"hac_table_b"

#' HAC 15 vaginal birth denominator codes
#'
#' ICD-10-AM codes defining the denominator cohort for HAC 15 (vaginal birth
#' procedure codes). One row per code x ICD edition.
#'
#' @format Same column structure as \code{hac_table_a}.
#' @source ACSQHC HAC specification Excel files.
"hac_table_c"

#' HAC 15 caesarean section exclusion ACHI codes
#'
#' ACHI procedure codes for elective/emergency caesarean sections, used to
#' exclude episodes from the HAC 15 denominator. One row per code x ACHI
#' edition.
#'
#' @format Same column structure as \code{hac_table_a} but with \code{achi_*}
#'   edition columns instead of \code{icd10am_*}.
#' @source ACSQHC HAC specification Excel files.
"hac_table_d"

#' HAC 16 neonatal birth trauma numerator codes
#'
#' ICD-10-AM codes for the HAC 16 numerator (neonatal birth trauma). One row
#' per code x ICD edition.
#'
#' @format Same column structure as \code{hac_table_a}.
#' @source ACSQHC HAC specification Excel files.
"hac_table_e"

#' HAC 16 neonatal exclusion codes
#'
#' ICD-10-AM codes used to exclude episodes from the HAC 16 denominator.
#' One row per code x ICD edition.
#'
#' @format Same column structure as \code{hac_table_a}.
#' @source ACSQHC HAC specification Excel files.
"hac_table_f"

#' HAC mental health denominator cohort codes (v3.1.1+)
#'
#' ICD-10-AM codes and ACHI codes defining the mental health denominator
#' cohorts introduced in HAC specification v3.1.1. Not used in the core
#' algorithm but retained for reference.
#'
#' @format A data frame with columns `complication`, `sub_category`, `role`,
#'   `code_nodot`, `code_dot`, `description`, `version_id`,
#'   `edition_type` (one of `"icd"` or `"achi"`).
#' @source ACSQHC HAC specification Excel files.
"hac_table_g"

#' Raw "other associated codes" text per sub-category and version
#'
#' Unparsed text cells from the "Other associated codes" columns in the
#' ACSQHC HAC specification Excel files. Retained for auditability.
#'
#' @format A data frame with columns `version_id`, `sub_category`,
#'   `applying_to_code`, `role`, `text`, `table_id`.
#' @source ACSQHC HAC specification Excel files.
"hac_other_assoc"

#' Parsed stage-specific criteria codes
#'
#' Structured representation of the additional criteria codes embedded in the
#' ACSQHC HAC specification -- falls external-cause codes, haematoma/ventilation
#' ACHI codes, anastomotic-leak external-cause codes, dialysis ACHI codes, CKD
#' exclusion codes, drug-related external-cause codes, and bradycardia pacing
#' codes. Used by conditional stages in `.hac_run_v3()`.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{version_id}{HAC version string, e.g. `"v3.2.2"`}
#'   \item{sub_category}{HAC sub-category to which the criterion applies}
#'   \item{applying_to_code}{Specific ICD code the criterion is attached to,
#'     or `NA` for sub-category-level criteria}
#'   \item{role}{One of `"falls_diag"`, `"inclusion_proc"`, `"external_diag"`,
#'     `"exclusion_diag"`}
#'   \item{code}{ICD-10-AM or ACHI code (without wildcard suffix)}
#'   \item{code_type}{`"icd"` or `"achi"`}
#'   \item{is_wildcard}{Logical; `TRUE` if the code should be prefix-matched}
#' }
#' @source ACSQHC HAC specification Excel files.
"hac_stage_criteria"

#' HAC diagnosis numerator codes (simplified view)
#'
#' A simplified view of the ACSQHC HAC numerator diagnosis codes, with one
#' logical column per ICD-10-AM edition and one logical column per HAC version.
#' Derived from the ACSQHC specification Excel files.
#'
#' @format A data frame with columns `complication`, `sub_category`, `role`,
#'   `code_nodot`, `code_dot`, `description`, plus logical columns
#'   `hac_v3_2_sep2025` and `icd10am_07` through `icd10am_13`.
#' @source ACSQHC HAC specification Excel files.
"hac_dx_codes"

#' HAC procedure exclusion codes (simplified view)
#'
#' A simplified view of the ACSQHC HAC procedure exclusion codes (HAC 15
#' caesarean section codes), with one logical column per ACHI edition and one
#' logical column per HAC version.
#'
#' @format A data frame with columns `complication`, `sub_category`, `role`,
#'   `code_nodot`, `code_dot`, `description`, plus logical columns
#'   `hac_v3_2_sep2025` and `achi_07` through `achi_13`.
#' @source ACSQHC HAC specification Excel files.
"hac_proc_codes"

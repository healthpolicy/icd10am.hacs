library(icd10am.utils)

# Synthetic episode data with the additional fields required by identify_hacs()
make_test_df <- function(base = icd10am.utils::dfbase) {
  transform(base,
    drg      = "A01A",
    caretype = "1",
    qdays    = 2L,
    admmode  = 2L,
    los      = 5L,
    samedayf = 0L
  )
}

test_that("identify_hacs() returns a data frame with correct structure", {
  df     <- make_test_df()
  result <- identify_hacs(
    df      = df,
    id      = recordID,
    format  = "long",
    df_diag = icd10am.utils::dfdiag,
    df_proc = icd10am.utils::dfproc
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), nrow(df))

  # Summary HAC flags present
  expect_true(all(paste0("hac", c(1:4, 6:16)) %in% names(result)))
  expect_true("hac15p1" %in% names(result))
  expect_true("hac15p2" %in% names(result))
  expect_true("hac_array" %in% names(result))
})

test_that("HAC summary flags are binary integers", {
  result <- identify_hacs(
    df      = make_test_df(),
    id      = recordID,
    format  = "long",
    df_diag = icd10am.utils::dfdiag,
    df_proc = icd10am.utils::dfproc
  )

  hac_cols <- grep("^hac[0-9]{1,2}$", names(result), value = TRUE)
  for (col in hac_cols) {
    expect_true(
      all(result[[col]] %in% c(0L, 1L)),
      label = paste0(col, " contains only 0/1")
    )
  }
})

test_that("hac_array is NA iff no HAC category is flagged", {
  result <- identify_hacs(
    df      = make_test_df(),
    id      = recordID,
    format  = "long",
    df_diag = icd10am.utils::dfdiag,
    df_proc = icd10am.utils::dfproc
  )

  hac_cols <- grep("^hac[0-9]{1,2}$", names(result), value = TRUE)
  any_hac  <- rowSums(result[hac_cols]) > 0L
  expect_equal(is.na(result$hac_array), !any_hac)
})

test_that("verbose = TRUE produces messages without changing results", {
  df <- make_test_df()

  result_silent <- identify_hacs(df, recordID, "long",
                                  df_diag = icd10am.utils::dfdiag,
                                  df_proc = icd10am.utils::dfproc,
                                  verbose = FALSE)

  # Check messages are emitted
  expect_message(
    identify_hacs(df, recordID, "long",
                  df_diag = icd10am.utils::dfdiag,
                  df_proc = icd10am.utils::dfproc,
                  verbose = TRUE),
    regexp = "identify_hacs"
  )

  # Results identical regardless of verbose flag
  result_verbose <- suppressMessages(
    identify_hacs(df, recordID, "long",
                  df_diag = icd10am.utils::dfdiag,
                  df_proc = icd10am.utils::dfproc,
                  verbose = TRUE)
  )
  expect_equal(result_silent$hac_array, result_verbose$hac_array)
})

test_that("unsupported HAC version gives informative error", {
  expect_error(
    identify_hacs(make_test_df(), recordID, "long",
                  df_diag = icd10am.utils::dfdiag,
                  df_proc = icd10am.utils::dfproc,
                  version = "99.0.0"),
    regexp = "not available"
  )
})

test_that("cat* sub-category flags are binary integers", {
  result <- identify_hacs(
    df      = make_test_df(),
    id      = recordID,
    format  = "long",
    df_diag = icd10am.utils::dfdiag,
    df_proc = icd10am.utils::dfproc
  )

  cat_cols <- grep("^cat", names(result), value = TRUE)
  expect_true(length(cat_cols) > 0L)
  for (col in cat_cols) {
    expect_true(
      all(result[[col]] %in% c(0L, 1L)),
      label = paste0(col, " contains only 0/1")
    )
  }
})

test_that(".hac_sub_to_cat() converts sub_category strings correctly", {
  expect_equal(.hac_sub_to_cat("1.1 Stage I pressure injury"), "cat01p1")
  expect_equal(.hac_sub_to_cat("2.3 Other fractures"),         "cat02p3")
  expect_equal(.hac_sub_to_cat("10.4 Movement disorders"),     "cat10p4")
  expect_equal(.hac_sub_to_cat("14.2 Arrhythmias"),            "cat14p2")
  expect_equal(.hac_sub_to_cat("15.1 Perineal laceration"),    "cat15p1")
  expect_equal(.hac_sub_to_cat("16.2 Hypoxic ischaemic"),      "cat16p2")
})

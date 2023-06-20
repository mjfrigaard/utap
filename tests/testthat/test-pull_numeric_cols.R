testthat::test_that("pull_numeric_cols() app_inputs_test", {

  app_inputs_test <- readRDS(testthat::test_path("fixtures", "pull_cols_test.rds"))

  # test numeric columns
  testthat::expect_equal(
    object = pull_numeric_cols(app_inputs_test),
    expected =
      c(
        dbl_var_na = "dbl_var_na",
        dbl_var = "dbl_var",
        int_var_na = "int_var_na",
        int_var = "int_var"
      )
  )
})

testthat::test_that("pull_numeric_cols() helpers data", {

  null_tbl <- col_maker(col_type = c("log", "chr", "fct",  "ord"), size = 10,
                        missing = FALSE)

  # test logical columns
  testthat::expect_null(
    object = pull_numeric_cols(null_tbl)
  )
  # test character columns
  testthat::expect_null(
    object = pull_numeric_cols(null_tbl)
  )
  # test factor columns
  testthat::expect_null(
    object = pull_numeric_cols(null_tbl)
  )
  testthat::expect_null(
    object = pull_numeric_cols(null_tbl)
  )
})

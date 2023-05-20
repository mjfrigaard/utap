testthat::test_that("pull_numeric_cols() works", {
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

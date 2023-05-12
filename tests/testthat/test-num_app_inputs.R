testthat::test_that("num_app_inputs() works", {
  app_inputs_test <- readRDS(testthat::test_path("fixtures", "app_inputs_test.rds"))
  # test numeric columns
  testthat::expect_equal(
    object = num_app_inputs(app_inputs_test),
    expected =
      c(
        dbl_var_na = "dbl_var_na",
        dbl_var = "dbl_var",
        int_var_na = "int_var_na",
        int_var = "int_var"
      )
  )
})

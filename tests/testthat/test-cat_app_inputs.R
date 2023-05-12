testthat::test_that("cat_app_inputs() works", {
  app_inputs_test <- readRDS(testthat::test_path("fixtures", "app_inputs_test.rds"))
  testthat::expect_equal(
    object = cat_app_inputs(app_inputs_test),
    expected =
      c(
        chr6_var = "chr6_var",
        chr7_var_na = "chr7_var_na",
        fct6_var_na = "fct6_var_na",
        fct7_var = "fct7_var",
        ord6_var_na = "ord6_var_na",
        ord7_var = "ord7_var"
      )
  )
})

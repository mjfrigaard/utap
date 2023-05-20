testthat::test_that("pull_cat_cols() works", {
  app_inputs_test <- readRDS(testthat::test_path("fixtures", "pull_cols_test.rds"))
  testthat::expect_equal(
    object = pull_cat_cols(app_inputs_test),
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

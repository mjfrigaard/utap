testthat::test_that("pull_cat_cols() pull_cols_test data", {

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

testthat::test_that("pull_cat_cols() nulls", {
  null_tbl <- col_maker(col_type = c("log", "int", "dbl"),
                       size = 10,
                       missing = FALSE)
  # test logical columns
  testthat::expect_null(
    object = pull_cat_cols(null_tbl)
  )
  # test integer columns
  testthat::expect_null(
    object = pull_cat_cols(null_tbl)
  )
  # test double columns
  testthat::expect_null(
    object = pull_cat_cols(null_tbl)
  )
})

testthat::test_that("pull_cat_cols() pull_cols_test data", {

  app_inputs_test <- readRDS(testthat::test_path("fixtures",
                                                      "pull_cols_test.rds"))

  testthat::expect_equal(
    object = pull_cat_cols(app_inputs_test),
    expected =
      dplyr::select(app_inputs_test,
        dplyr::where(is.factor),
        dplyr::where(is.character)) |>
      colnames() |>
      purrr::set_names() |>
      sort()
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

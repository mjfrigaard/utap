testthat::test_that("pull_facet_cols() works", {
  app_inputs_test <- readRDS(testthat::test_path("fixtures", "pull_cols_test.rds"))
  # remove binary columns
  expect_equal(
    object = dplyr::select(
      app_inputs_test,
      !dplyr::contains("bin")
    ) |> pull_facet_cols(),
    expected =
      c(
        chr_facet5 = "chr_facet5",
        chr_facet5_na = "chr_facet5_na",
        fct_facet5 = "fct_facet5",
        fct_facet5_na = "fct_facet5_na",
        ord_facet5 = "ord_facet5",
        ord_facet5_na = "ord_facet5_na"
      )
  )
})

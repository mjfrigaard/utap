testthat::test_that("facet_app_inputs() works", {
  app_inputs_test <- readRDS(testthat::test_path("fixtures", "app_inputs_test.rds"))
  # remove binary columns
  expect_equal(
    object = dplyr::select(
      app_inputs_test,
      !dplyr::contains("bin")
    ) |> facet_app_inputs(),
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

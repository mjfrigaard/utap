library(vdiffr)
library(testPkgApp)
testthat::test_that("gg_base works", {
  # define inputs
  set.seed(1234)
  df_test <- data.frame(
    "x" = rnorm(n = 100, mean = 5, sd = 2),
    "y" = rnorm(n = 100, mean = 10, sd = 5)
  )
  # create base
  ggp_base <- gg_base(
    df = df_test,
    x_var = "x",
    y_var = "y"
  )
  vdiffr::expect_doppelganger("default base plot", ggp_base)
})

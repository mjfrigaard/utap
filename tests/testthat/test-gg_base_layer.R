library(vdiffr)
library(testPkgApp)
testthat::test_that("gg_base works", {
  x <- "mpg"
  y <- 'cyl'
  ggp_base <- gg_base(df = mtcars,
    x_var = x,
    y_var = y)
  vdiffr::expect_doppelganger("default histogram", ggp_base)
})

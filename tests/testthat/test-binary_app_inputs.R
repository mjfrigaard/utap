testthat::test_that("binary_app_inputs() works", {
  app_inputs_test <- readRDS(testthat::test_path("fixtures", "app_inputs_test.rds"))
  expect_equal(
    object = binary_app_inputs(app_inputs_test),
    expected =
      c(
        log_bin_na = "log_bin_na",
        log_bin = "log_bin",
        int_bin_na = "int_bin_na",
        int_bin = "int_bin",
        chr_bin_na = "chr_bin_na",
        chr_bin = "chr_bin"
      )
  )
})

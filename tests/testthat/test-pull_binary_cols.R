testthat::test_that("pull_binary_cols() works", {
  app_inputs_test <- readRDS(testthat::test_path("fixtures", "pull_cols_test.rds"))
  expect_equal(
    object = pull_binary_cols(app_inputs_test),
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

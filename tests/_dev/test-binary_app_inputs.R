test_that("binary_app_inputs works", {
  binary_app_inputs <- function(df) {
    log_df <- dplyr::select(
      tibble::as_tibble(df),
      dplyr::where(is.logical)
    )
    if (ncol(log_df) > 0) {
      # get names
      nms <- names(log_df)
      # set names in names
      df_nms <- purrr::set_names(nms)
      check_log_binary <- function(x) {
        all(na.omit(x) %in% TRUE:FALSE)
      }
      bins <- sapply(log_df, check_log_binary)
      return(df_nms[bins])
    }
  }
  # test with logical data ----------------------------
  log_bin_df <- tibble::tibble(
    bin_na = c(TRUE, FALSE, NA, TRUE),
    bin = c(TRUE, FALSE, FALSE, TRUE),
    # non binary columns
    no_bin_chr = c("A", "B", NA_character_, "D"),
    no_bin_int = c(0L, 1L, NA_integer_, 2L),
    no_bin_dbl = c(0.1, 1.1, NA_real_, 0.2)
  )
  # create object
  bins <- binary_app_inputs(log_bin_df)
  # test
  testthat::expect_equal(
    object = bins,
    expected = c(
      bin_na = "bin_na",
      bin = "bin"
    )
  )
})

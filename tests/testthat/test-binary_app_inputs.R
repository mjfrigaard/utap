test_that("binary_app_inputs works", {
    binary_app_inputs <- function(df) {
      log_df <- dplyr::select(tibble::as_tibble(df),
                              dplyr::where(is.logical))
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
  set.seed(1234)
  log_bin_df <- tibble::tibble(
                      bin_na = log_maker(size = 10, missing = TRUE),
                      bin = log_maker(size = 10),
                      # non binary columns
                      chr_na = chr_maker(10, TRUE),
                      chr = chr_maker(10))

    # create object
    bins <- binary_app_inputs(log_bin_df)
    # test
    testthat::expect_equal(
      object = bins,
      expected = c(bin_na = "bin_na",
                   bin = "bin"))

})

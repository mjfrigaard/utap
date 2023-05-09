library(testthat)
library(testPkgApp)
testthat::test_that("binary_vars() works", {

  set.seed(1234)
  # test with logical data ----------------------------
  log_bin_df <- tibble::tibble(bin_na = c(TRUE, FALSE, NA, TRUE),
                         bin = c(TRUE, FALSE, FALSE, TRUE),
                         # non binary columns
                         no_bin_chr = c("A", "B", NA_character_, "D"),
                         no_bin_int = c(0L, 1L, NA_integer_, 2L),
                         no_bin_dbl = c(0.1, 1.1, NA_real_, 0.2))

    binary_vars <- function(df) {
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

    # create object
    bins <- binary_vars(log_bin_df)
    # test
    testthat::expect_equal(
      object = bins,
      expected = c(bin_na = "bin_na",
                   bin = "bin"))

})

# # test with character data ----------------------------
  # chr_vars_test <- tibble::tibble(
  #       # character 2 unique (with missing values)
  #       chr_02 = sample(x = c("YES", "NO",
  #                                 "YES", "NO", NA_character_),
  #                         size = 100, replace = TRUE),
  #       # character 3 unique (with missing values)
  #       chr_03 = sample(c("YES", "NO",
  #                           "MAYBE", NA_character_),
  #                           size = 100, replace = TRUE))
  #     # create object
  #     bins <- binary_vars(chr_vars_test)
  #     # test
  #     expect_equal(object = bins,
  #       expected = c(chr_02 = "chr_02"))
  # # test with ordered factor data ----------------------------
  # ord_vars_test <- tibble::tibble(
  #     # tests for two level ordered factor (with missing)
  #     ord_02 = factor(sample(x = c("high", "low",
  #                                    "high", "low",
  #                                     NA_character_),
  #                              size = 100, replace = TRUE),
  #                         levels = c("low", "high"),
  #                         ordered = TRUE),
  #     # tests for three level ordered factor (with missing)
  #     ord_03 = factor(sample(x = c("small", "medium",
  #                                      "large", NA_character_),
  #                              size = 100, replace = TRUE),
  #                       levels = c("small", "medium", "large"),
  #                       ordered = TRUE))
  #     # create object
  #     bins <- binary_vars(ord_vars_test)
  #     # test
  #     expect_equal(object = bins,
  #       expected = c(ord_02 = "ord_02"))
  # # test with factor data ----------------------------
  #       fct_vars_test <- tibble::tibble(
  #       # two level factor (with missing)
  #       fct_02 = factor(sample(x = c("group a", "group b",
  #                                      "group a", "group b",
  #                                       NA_character_),
  #                                size = 100, replace = TRUE),
  #                         levels = c("group a", "group b")),
  #       # three level factor (with missing)
  #       fct_03 = factor(x = sample(c("group 1", "group 2", "group 3",
  #                                      NA_character_),
  #                                      size = 100, replace = TRUE),
  #                         levels = c("group 1", "group 2", "group 3")),
  #       # three level factor, with only two levels represented (with missing)
  #       fct_04 = factor(x = sample(c("group x", "group y", NA_character_),
  #                                      size = 100, replace = TRUE),
  #                         levels = c("group x", "group y",
  #                                    "group z")))
  #     # create object
  #     bins <- binary_vars(fct_vars_test)
  #     # test
  #     expect_equal(object = bins,
  #       expected = c(fct_02 = "fct_02"))
  # # test with list-col data ----------------------------
  # list_cols_test <- tibble::tibble(
  #       # single list column
  #       lst_01 = list(
  #         # three level factor (with missing)
  #         fct_03 = factor(x = sample(c("group 1", "group 2", "group 3",
  #                                      NA_character_),
  #                                      size = 100, replace = TRUE),
  #                         levels = c("group 1", "group 2", "group 3")),
  #         # three level factor, with only two levels represented (with missing)
  #         fct_04 = factor(x = sample(c("group x", "group y", NA_character_),
  #                                      size = 100, replace = TRUE),
  #                         levels = c("group x", "group y",
  #                                    "group z"))))
  #     # test
  #     testthat::expect_equal(
  #       object = binary_vars(list_cols_test),
  #       expected = NULL)

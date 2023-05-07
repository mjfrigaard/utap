library(testthat)

testthat::test_that("get_col_type_df() works", {

    binary_df <- tibble::tibble(
      log_na_bin = c(TRUE, FALSE, NA, TRUE),
      log_bin = c(TRUE, FALSE, FALSE, TRUE),
      int_bin = c(0L, 1L, NA_integer_, 1L),
      chr_bin = c("A", "B", NA_character_, "B"),
      bin_ord = factor(x = c("high", "low",
                             NA_character_, "high"),
                       levels = c("low", "high"),
                       ordered = TRUE),
      bin_fct = factor(x = c("hot", "cold",
                             NA_character_, "cold"),
                       levels = c("hot", "cold")),
      no_bin_chr = c("A", "B", NA_character_, "D"),
      no_bin_int = c(0L, 1L, NA_integer_, 2L),
      no_bin_dbl = c(0.5, 1.2, NA_real_, 0.001),
      no_bin_ord = factor(x = c("high", "low",
                                NA_character_, "med"),
                          levels = c("low", "high",
                                     "med"),
                          ordered = TRUE),
      no_bin_fct = factor(x = c("left", "right",
                                NA_character_, "center"),
                          levels = c("left", "right",
                                     "center")))

    get_col_type_df <- function(df, type) {
      df_cols <- switch(type,
        log = dplyr::select(tibble::as_tibble(df),
                            dplyr::where(is.logical)),
        int = dplyr::select(tibble::as_tibble(df),
                            dplyr::where(is.integer)),
        chr = dplyr::select(tibble::as_tibble(df),
                            dplyr::where(is.character)),
        fct = dplyr::select(tibble::as_tibble(df),
                            dplyr::where(is.factor)),
        list = dplyr::select(tibble::as_tibble(df),
                            dplyr::where(is.list))
        )
      if (ncol(df_cols) < 1 ) {
        df_cols <- structure(list(),
          class = c("tbl_df", "tbl", "data.frame"),
          row.names = integer(0),
          names = character(0))
        cli::cli_alert_info("No columns of that type...")
        return(df_cols)
      } else {
        return(df_cols)
      }
    }

    # test logical class
    col_types_class <- class(get_col_type_df(binary_df, type = "log"))
    testthat::expect_equal(
      object = col_types_class,
      expected = c("tbl_df", "tbl", "data.frame"))
    # test logical names
    col_types_names <- names(get_col_type_df(binary_df, type = "log"))
      testthat::expect_equal(
        object = col_types_names,
        expected = c("log_na_bin", "log_bin"))

    # test integer class
    col_types_class <- class(get_col_type_df(binary_df, type = "int"))
    testthat::expect_equal(
      object = col_types_class,
      expected = c("tbl_df", "tbl", "data.frame"))
      # test integer names
    col_types_names <- names(get_col_type_df(binary_df, type = "int"))
    testthat::expect_equal(
      object = col_types_names,
      expected = c("int_bin", "no_bin_int"))

    # test character class
    col_types_class <- class(get_col_type_df(binary_df, type = "chr"))
    testthat::expect_equal(
      object = col_types_class,
      expected = c("tbl_df", "tbl", "data.frame"))
    # test character names
    col_types_names <- names(get_col_type_df(binary_df, type = "chr"))
    testthat::expect_equal(
      object = col_types_names,
      expected = c("chr_bin", "no_bin_chr"))

    # test factor class
    col_types_class <- class(get_col_type_df(binary_df, type = "fct"))
    testthat::expect_equal(
      object = col_types_class,
      expected = c("tbl_df", "tbl", "data.frame"))
    # test character names
    col_types_names <- names(get_col_type_df(binary_df, type = "fct"))
    testthat::expect_equal(
      object = col_types_names,
      expected = c("bin_ord", "bin_fct", "no_bin_ord", "no_bin_fct"))

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
})

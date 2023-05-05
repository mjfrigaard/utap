library(testthat)

testthat::test_that("binary_vars() works", {
  # define inputs
  set.seed(1234)
  # test with logical data ----------------------------
  log_vars_test <- tibble::tibble(
    # logical (no missing)
    log_02 = sample(c(TRUE, FALSE),
                   size = 100, replace = TRUE),
    # logical (with missing)
    log_03 = sample(c(TRUE, FALSE, NA),
                   size = 100, replace = TRUE))
    # create object
    bins <- binary_vars(log_vars_test)
    # test
    testthat::expect_equal(
      object = bins,
      expected = c(log_02 = "log_02",
                   log_03 = "log_03"))
  # test with character data ----------------------------
  chr_vars_test <- tibble::tibble(
        # character 2 unique (with missing values)
        chr_02 = sample(x = c("YES", "NO",
                                  "YES", "NO", NA_character_),
                          size = 100, replace = TRUE),
        # character 3 unique (with missing values)
        chr_03 = sample(c("YES", "NO",
                            "MAYBE", NA_character_),
                            size = 100, replace = TRUE))
      # create object
      bins <- binary_vars(chr_vars_test)
      # test
      expect_equal(object = bins,
        expected = c(chr_02 = "chr_02"))
  # test with ordered factor data ----------------------------
  ord_vars_test <- tibble::tibble(
      # tests for two level ordered factor (with missing)
      ord_02 = factor(sample(x = c("high", "low",
                                     "high", "low",
                                      NA_character_),
                               size = 100, replace = TRUE),
                          levels = c("low", "high"),
                          ordered = TRUE),
      # tests for three level ordered factor (with missing)
      ord_03 = factor(sample(x = c("small", "medium",
                                       "large", NA_character_),
                               size = 100, replace = TRUE),
                        levels = c("small", "medium", "large"),
                        ordered = TRUE))
      # create object
      bins <- binary_vars(ord_vars_test)
      # test
      expect_equal(object = bins,
        expected = c(ord_02 = "ord_02"))
  # test with factor data ----------------------------
        fct_vars_test <- tibble::tibble(
        # two level factor (with missing)
        fct_02 = factor(sample(x = c("group a", "group b",
                                       "group a", "group b",
                                        NA_character_),
                                 size = 100, replace = TRUE),
                          levels = c("group a", "group b")),
        # three level factor (with missing)
        fct_03 = factor(x = sample(c("group 1", "group 2", "group 3",
                                       NA_character_),
                                       size = 100, replace = TRUE),
                          levels = c("group 1", "group 2", "group 3")),
        # three level factor, with only two levels represented (with missing)
        fct_04 = factor(x = sample(c("group x", "group y", NA_character_),
                                       size = 100, replace = TRUE),
                          levels = c("group x", "group y",
                                     "group z")))
      # create object
      bins <- binary_vars(fct_vars_test)
      # test
      expect_equal(object = bins,
        expected = c(fct_02 = "fct_02"))
  # test with list-col data ----------------------------
  list_cols_test <- tibble::tibble(
        # single list column
        lst_01 = list(
          # three level factor (with missing)
          fct_03 = factor(x = sample(c("group 1", "group 2", "group 3",
                                       NA_character_),
                                       size = 100, replace = TRUE),
                          levels = c("group 1", "group 2", "group 3")),
          # three level factor, with only two levels represented (with missing)
          fct_04 = factor(x = sample(c("group x", "group y", NA_character_),
                                       size = 100, replace = TRUE),
                          levels = c("group x", "group y",
                                     "group z"))))
      # test
      testthat::expect_error(object = binary_vars(list_cols_test))
})

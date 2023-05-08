## code to prepare `testdf_app_inputs` dataset goes here
require(tibble)
require(readr)
# test data ----------------------------
testdf_app_inputs <- tibble::tibble(
    # logical binary (with missing)
    log_na_bin = sample(x = c(TRUE, FALSE, NA, TRUE), 10, TRUE),
    # logical binary (no missing)
    log_bin = sample(x = c(TRUE, FALSE, FALSE, TRUE), 10, TRUE),
    # integer binary (with missing)
    int_bin = sample(x = c(0L, 1L, NA_integer_, 1L), 10, TRUE),
    # two-level character facet variable (with missing)
    chr_3l_na_facet = sample(x = c("A", "B", NA_character_, "B"),
                           10, TRUE),
    # two level ordinal binary factor (with missing)
    ord_bin = factor(x = sample(x = c("high", "low",
                                      NA_character_, "high"),
                                10, TRUE),
                            levels = c("low", "high"),
                            ordered = TRUE),
    # two level binary factor (with missing)
    fct_bin = factor(sample(x = c("hot", "cold",
                                  NA_character_, "cold"),
                            10, TRUE),
                            levels = c("hot", "cold")),
    # character facet variable (4 unique values)
    chr_4l_facet = sample(x = c("A", "B", "C", "D"), 10, TRUE),
    # three level integer variable (with missing)
    int_3l_na = sample(x = c(0L, 1L, NA_integer_, 2L), 10, TRUE),
    # three level double variable (with missing)
    dbl_3l_na = sample(x = c(0.5, 1.2, NA_real_, 0.001), 10, TRUE),
    # three level ordinal factor (with missing)
    ord_3l_na = factor(sample(x = c("high", "low",
                      NA_character_, "med"), 10, TRUE),
                      levels = c("low", "high","med"),
                      ordered = TRUE),
    # three level ordinal factor (no missing)
    ord_3l = factor(sample(x = c("high", "low", "med"), 10, TRUE),
                      levels = c("low", "high","med"),
                      ordered = TRUE),
    # three level factor (with missing)
    fct_3l_facet = factor(sample(x = c("left", "right", "center"),
                                  10, TRUE),
                           levels = c("left", "right", "center")),
    # three level factor (no missing)
    fct_3l_na_facet = factor(sample(x = c("left", "right",
                                        NA_character_, "center"),
                                  10, TRUE),
                           levels = c("left", "right", "center")),
    # five level factor, sixth value not included in levels
    fct_5lv6rep_na_facet = factor(sample(x = c("group 1", "group 2",
                                        "group 3", "group 4",
                                        "group 5", "group 6",
                                        NA_character_),
                                  10, TRUE),
                           levels = c("group 1", "group 2",
                                       "group 3", "group 4",
                                       "group 5")),
    # four level factor, three levels represented
    fct_4lv3rep_na_facet = factor(sample(x = c("group 1", "group 2",
                                        "group 3", NA_character_),
                                  10, TRUE),
                           levels = c("group 1", "group 2",
                                       "group 3", "group 4")),
    # six level factor
    fct_6l_na = factor(sample(x = c("group 1", "group 2",
                                        "group 3", "group 4",
                                        "group 5", "group 6",
                                        NA_character_),
                                  10, TRUE),
                           levels = c("group 1", "group 2",
                                       "group 3", "group 4",
                                       "group 5", "group 6")),
    # ten level character variable (no missing)
    chr_10l = c(LETTERS[1:10]),
    # ten level character variable (with missing)
    chr_10l_na = sample(c(LETTERS[1:10], NA_character_), 10, TRUE),
    # ten level factor variable (no missing)
    fct_10l = factor(x = sample(c(LETTERS[1:10]), 10, TRUE),
                          levels = c(LETTERS[1:10])),
    # ten level factor variable (with missing)
    fct_10l_na = factor(sample(c(LETTERS[1:10], NA_character_), 10, TRUE),
                          levels = c(LETTERS[1:10])))

usethis::use_data(testdf_app_inputs, overwrite = TRUE)
# export to tests/testthat/fixtures/
saveRDS(testdf_app_inputs,
  file = "tests/testthat/fixtures/testdf_app_inputs.rds")


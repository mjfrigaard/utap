## code to prepare `testdf_app_inputs` dataset goes here
require(tibble)
# test data ----------------------------
  testdf_app_inputs <- tibble::tibble(
    log_na_bin = sample(x = c(TRUE, FALSE, NA, TRUE), 10, TRUE),
    log_bin = sample(x = c(TRUE, FALSE, FALSE, TRUE), 10, TRUE),
    int_bin = sample(x = c(0L, 1L, NA_integer_, 1L), 10, TRUE),
    # two-level character facet variable (with missing)
    chr_facet_3na = sample(x = c("A", "B", NA_character_, "B"),
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
    chr_facet_4 = sample(x = c("A", "B", "C", "D"), 10, TRUE),
    # three level integer variable (with missing)
    int_3na = sample(x = c(0L, 1L, NA_integer_, 2L), 10, TRUE),
    # three level double variable (with missing)
    dbl_3na = sample(x = c(0.5, 1.2, NA_real_, 0.001), 10, TRUE),
    # three level ordinal factor (with missing)
    ord_3na = factor(sample(x = c("high", "low",
                      NA_character_, "med"), 10, TRUE),
                      levels = c("low", "high","med"),
                      ordered = TRUE),
    # three level ordinal factor (no missing)
    ord_3 = factor(sample(x = c("high", "low", "med"), 10, TRUE),
                      levels = c("low", "high","med"),
                      ordered = TRUE),
    # three level factor (with missing)
    fct_facet_3 = factor(sample(x = c("left", "right", "center"),
                                  10, TRUE),
                           levels = c("left", "right", "center")),
    # three level factor (no missing)
    fct_facet_3na = factor(sample(x = c("left", "right",
                                        NA_character_, "center"),
                                  10, TRUE),
                           levels = c("left", "right", "center")),
    # five level factor, sixth value not included in levels
    fct_facet_5naS = factor(sample(x = c("group 1", "group 2",
                                        "group 3", "group 4",
                                        "group 5", "group 6",
                                        NA_character_),
                                  10, TRUE),
                           levels = c("group 1", "group 2",
                                       "group 3", "group 4",
                                       "group 5")),
    # four level factor, three levels represented
    fct_facet_4naS = factor(sample(x = c("group 1", "group 2",
                                        "group 3", NA_character_),
                                  10, TRUE),
                           levels = c("group 1", "group 2",
                                       "group 3", "group 4")),
    # six level factor
    no_facet_fct_6na = factor(sample(x = c("group 1", "group 2",
                                        "group 3", "group 4",
                                        "group 5", "group 6",
                                        NA_character_),
                                  10, TRUE),
                           levels = c("group 1", "group 2",
                                       "group 3", "group 4",
                                       "group 5", "group 6")),
    # ten level character variable (no missing)
    no_facet_chr_10 = c(LETTERS[1:10]),
    # ten level character variable (with missing)
    no_facet_chr_10na = sample(c(LETTERS[1:10], NA_character_), 10, TRUE),
    # ten level factor variable (no missing)
    no_facet_10fct = factor(x = sample(c(LETTERS[1:10]), 10, TRUE),
                          levels = c(LETTERS[1:10])),
    # ten level factor variable (with missing)
    no_facet_fct_10na = factor(sample(c(LETTERS[1:10], NA_character_), 10, TRUE),
                          levels = c(LETTERS[1:10])))
usethis::use_data(testdf_app_inputs, overwrite = TRUE)

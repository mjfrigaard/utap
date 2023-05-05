## code to prepare `bin_vars_test` dataset goes here
require(tibble)
binary_vars_test <- tibble::tibble(
    # logical (no missing)
    log_02 = sample(c(TRUE, FALSE),
                   size = 100, replace = TRUE),
    # logical (with missing)
    log_03 = sample(c(TRUE, FALSE, NA),
                   size = 100, replace = TRUE),
    # character 2 unique (with missing values)
    chr_02 = sample(x = c("YES", "NO",
                              "YES", "NO", NA_character_),
                      size = 100, replace = TRUE),
    # character 3 unique (with missing values)
    chr_03 = sample(c("YES", "NO",
                        "MAYBE", NA_character_),
                        size = 100, replace = TRUE),
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
                        ordered = TRUE),
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
    fct_04 = factor(x = sample(c("group x", "group y",
                                   NA_character_),
                                   size = 100, replace = TRUE),
                      levels = c("group x", "group y",
                                 "group z")))
usethis::use_data(binary_vars_test, overwrite = TRUE)

## code to prepare `binary_df` dataset goes here
require(tibble)
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
usethis::use_data(binary_df, overwrite = TRUE)

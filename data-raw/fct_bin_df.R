## code to prepare `fct_bin_df` dataset goes here
require(tibble)
fct_bin_df <- tibble::tibble(bin_ord = factor(x = c("high", "low",
                                                 NA_character_, "high"),
                                    levels = c("low", "high"),
                                    ordered = TRUE),
                            bin_fct = factor(x = c("hot", "cold",
                                                    NA_character_, "cold"),
                                    levels = c("hot", "cold")),
                            no_bin_dbl = c(0.1, 1.1, 1.2, 0.2),
                            no_bin_int = c(0L, 1L, NA_integer_, 2L),
                            no_bin_chr = c("A", "B", NA_character_, "D"))
usethis::use_data(fct_bin_df, overwrite = TRUE)

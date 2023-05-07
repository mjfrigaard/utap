## code to prepare `log_bin_df` dataset goes here
require(tibble)
log_bin_df <- tibble::tibble(bin_na = c(TRUE, FALSE, NA, TRUE),
                         bin = c(TRUE, FALSE, FALSE, TRUE),
                         # non binary columns
                         no_bin_chr = c("A", "B", NA_character_, "D"),
                         no_bin_int = c(0L, 1L, NA_integer_, 2L),
                         no_bin_dbl = c(0.1, 1.1, NA_real_, 0.2))
usethis::use_data(log_bin_df, overwrite = TRUE)

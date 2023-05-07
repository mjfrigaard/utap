## code to prepare `int_bin_df` dataset goes here
require(tibble)
int_bin_df <- tibble::tibble(bin = c(0L, 1L, NA_integer_, 1L),
                          no_bin_int = c(0L, 1L, NA_integer_, 2L),
                          no_bin_dbl = c(0.1, 1.1, NA_real_, 0.1),
                          no_bin_chr = c("A", "B", NA_character_, "D"))
usethis::use_data(int_bin_df, overwrite = TRUE)

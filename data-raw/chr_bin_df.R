## code to prepare `chr_bin_df` dataset goes here
require(tibble)
chr_bin_df <- tibble::tibble(bin = c("A", "B", NA_character_, "B"),
                         no_bin_chr = c("A", "B", NA_character_, "D"),
                         no_bin_int = c(0L, 1L, NA_integer_, 2L),
                         no_bin_dbl = c(0.1, 1.1, NA_real_, 0.2))
usethis::use_data(chr_bin_df, overwrite = TRUE)

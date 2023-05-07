## code to prepare `facet_df` dataset goes here
require(tibble)
facet_df <- tibble::tibble(
  # logical (missing)
  log_var = sample(c(TRUE, FALSE, NA),
                   size = 10, replace = TRUE),
  # double (missing)
  dbl_var = sample(c(1.5:10.5, NA_real_),
                   size = 10, replace = TRUE),
  # integer (missing)
  int_var = sample(c(1:10, NA_integer_),
                   size = 10, replace = TRUE),
  # character (missing)
  chr_var_02 = sample(x = c(LETTERS[1:2], LETTERS[1:2],
                            NA_character_),
                      size = 10, replace = TRUE),
  # character (no missing)
  chr_var_05 = rep(LETTERS[1:5], times = 2),
  # character (no missing)
  chr_var_10 = LETTERS[1:10],
  # date (missing)
  date_var = sample(c(Sys.Date(), Sys.Date() + 1,
                      NA_real_, Sys.Date() + 3,
                      Sys.Date() + 4),
                    size = 10, replace = TRUE),
  # two group factor (missing)
  fct_var_02 = factor(sample(x = c("group a", "group b", NA_character_),
                             size = 10, replace = TRUE),
                      levels = c("group a", "group b")),
  # five group factor (missing)
  fct_var_05 = factor(sample(x = c("group 1", "group 2", "group 3",
                                   "group 4", "group 5", NA_character_),
                             size = 10, replace = TRUE),
                      levels = c("group 1", "group 2", "group 3",
                                 "group 4", "group 5")),
  # three group factor (no missing)
  fct_var_03 = factor(x = sample(c("group x", "group y", "group z"),
                                   size = 10, replace = TRUE),
                      levels = c("group x", "group y",
                                 "group z"))
  )
usethis::use_data(facet_df, overwrite = TRUE)

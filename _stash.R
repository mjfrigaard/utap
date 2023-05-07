# require(dplyr)
# make_ui_inputs(dplyr::starwars)

# @importFrom ggplot2 ggplot aes vars facet_wrap geom_point labs
# @importFrom rlang .data
penguins <- palmerpenguins::penguins
gg_points(
 df = penguins,
 x_var = "bill_length_mm",
 y_var = "flipper_length_mm",
 col_var = "island",
 facet_var = "species",
 alpha = 1 / 3, size = 2
)

# mpg_base <- function(df, x_var, y_var) {
#     ggplot2::ggplot(
#     data = df,
#     mapping = ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]]))
# }
# mpg_base(df = ggplot2::mpg, x_var = "displ", y_var = "hwy")


gg_points_facet(
 df = palmerpenguins::penguins,
 x_var = "bill_length_mm",
 y_var = "flipper_length_mm",
 col_var = "island",
 facet_var = "species",
 alpha = 1 / 3, size = 2
)

gg_points_facet(
 df = penguins,
 x_var = "bill_length_mm",
 y_var = "flipper_length_mm",
 col_var = "island",
 facet_var = NULL,
 alpha = 1 / 3, size = 2
)

gg_points_facet(
 df = penguins,
 x_var = "bill_length_mm",
 y_var = "flipper_length_mm",
 col_var = NULL,
 facet_var = NULL,
 alpha = 1 / 3, size = 2
)

binary_checker <- function(df, type) {
  if (ncol(df) < 1) {
    return(purrr::set_names(vector(mode = "character")))
  } else {
    nms <- names(df)
    # set names in names
    dm_nms <- purrr::set_names(nms)
    bin_set <- purrr::map_vec(.x = df, .f = binary_checks, type = type)
    if (sum(bin_set) < 1) {
      # cli::cli_alert_info("No values of that type!")
      bins <- purrr::set_names(vector(mode = "character"))
    } else {
      # cli::cli_alert_success("Values of that type!")
      bins <- purrr::set_names(dm_nms[bin_set])
    }
  }
  return(bins)
}

binary_col_list <- function(df) {
  # logical
  log_bins <- get_col_type(df, "log") |>
              binary_checker("log")
  # integer
  int_bins <- get_col_type(df, "int") |>
              binary_checker("int")
  # character
  chr_bins <- get_col_type(df, "chr") |>
              binary_checker("chr")
  # factors
  fct_bins <- get_col_type(df, "fct") |>
              binary_checker("fct")
  # assemble
  all_bins <- list(log_bins, int_bins, chr_bins, fct_bins)
  # reduce
  bins_list <- purrr::compact(all_bins)
  # vector
  bins <- purrr::list_c(bins_list)
  return(bins)
}
binary_col_list(df = palmerpenguins::penguins)





# this also works,
# gg_points <- function(df, x_var, y_var, col_var, ...) {
#
#   ggplot2::ggplot(data = df,
#     mapping = ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]])) +
#
#     ggplot2::geom_point(ggplot2::aes(color = .data[[col_var]]), ...)
#
# }

library(palmerpenguins)
# devtools::install_github("nteetor/zeallot")
library(zeallot)
library(dplyr)
library(lubridate)
library(purrr)
movies <- testPkgApp::movies
penguins <- palmerpenguins::penguins
df_test <- tibble::tibble(
  a = sample(c(TRUE, FALSE), 10, TRUE),
  b = c(1.5:10.5),
  c = c(1:10),
  d1 = rep(LETTERS[1:5], times = 2),
  d2 = LETTERS[1:10],
  e = sample(c(
    Sys.Date(), Sys.Date() + 1,
    Sys.Date() + 2, Sys.Date() + 3,
    Sys.Date() + 4
  ), size = 10, replace = TRUE),
  f1 = factor(rep(letters[1:5], times = 2),
    levels = letters[1:5]
  ),
  f2 = factor(letters[1:10],
    levels = letters[1:10]
  )
)






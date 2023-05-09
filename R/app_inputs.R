#' Numeric app inputs
#'
#' @param df a `data.frame` or `tibble`
#'
#' @return integer and double column names
#' @export num_app_inputs
#'
#' @examples
#' require(palmerpenguins)
#' require(dplyr)
#' num_app_inputs(palmerpenguins::penguins)
#' num_app_inputs(dplyr::starwars)
num_app_inputs <- function(df) {
  dbls <- get_col_types(df = df, type = 'dbl', return_tbl = FALSE)
  ints <- get_col_types(df = df, type = 'int', return_tbl = FALSE)
  nums <- c(dbls, ints)
  return(nums)
}

#' Categorical app inputs
#'
#' @param df a `data.frame` or `tibble`
#'
#' @return character and factor column names
#' @export cat_app_inputs
#'
#' @examples
#' require(palmerpenguins)
#' require(dplyr)
#' cat_app_inputs(palmerpenguins::penguins)
#' cat_app_inputs(dplyr::starwars)
cat_app_inputs <- function(df) {
  chrs <- get_col_types(df = df, type = 'chr', return_tbl = FALSE)
  fcts <- get_col_types(df = df, type = 'fct', return_tbl = FALSE)
  cats <- c(chrs, fcts)
  return(cats)
}

#' Binary app inputs
#'
#' @param df a `data.frame` or `tibble`
#'
#' @return named vector of binary column names
#' @export binary_app_inputs
#'
#' @examples
#' require(palmerpenguins)
#' require(dplyr)
#' binary_app_inputs(palmerpenguins::penguins)
#' binary_app_inputs(dplyr::starwars)
binary_app_inputs <- function(df) {
  # logical
  log_bins <- get_col_types(df, "log") |>
                make_binary_vec("log")
  # integer
  int_bins <- get_col_types(df, "int") |>
                make_binary_vec("int")
  # character
  chr_bins <- get_col_types(df, "chr") |>
                make_binary_vec("chr")
  # factors
  fct_bins <- get_col_types(df, "fct") |>
                make_binary_vec("fct")
  # assemble
  all_bins <- list(log_bins, int_bins, chr_bins, fct_bins)
  # reduce
  bins_list <- purrr::compact(all_bins)
  # vector
  bins <- purrr::list_c(bins_list)
  return(bins)
}

#' Facet app inputs
#'
#' @section Variables to use for facets:
#'
#' This function is designed to quickly determine which variables have an
#' appropriate number of categorical levels for using `ggplot2::facet_wrap()`
#' or `ggplot2::facet_grid()`
#'
#' @param df a `data.frame` or `tibble`
#'
#'
#' @return a vector of factor or character column names with < 6 unique levels
#' @export facet_app_inputs
#'
#' @importFrom purrr compact list_c
#'
#' @examples
#' require(dplyr)
#' require(NHANES)
#' facet_app_inputs(df = dplyr::starwars)
#' facet_app_inputs(df = NHANES::NHANES)
#'
#' str(dplyr::select(dplyr::starwars,
#'   dplyr::all_of(facet_app_inputs(df = dplyr::starwars))))
#' str(dplyr::select(NHANES::NHANES,
#'   dplyr::all_of(facet_app_inputs(df = NHANES::NHANES))))
facet_app_inputs <- function(df) {
  # character
  chr_facets <- get_col_types(df, "chr") |>
                  make_facet_vec("chr")
  # factors
  fct_facets <- get_col_types(df, "fct") |>
                  make_facet_vec("fct")
  # assemble
  all_facets <- list(chr_facets, fct_facets)
  # reduce
  facets_list <- purrr::compact(all_facets)
  # vector
  facets <- purrr::list_c(facets_list)
  return(facets)
}

#' Numeric app inputs
#'
#' @param df a `data.frame` or `tibble`
#'
#' @return integer and double column names
#' @export num_app_inputs
#'
#' @importFrom purrr compact list_c set_names
#'
#' @examples
#' require(palmerpenguins)
#' require(dplyr)
#' num_app_inputs(palmerpenguins::penguins)
#' num_app_inputs(dplyr::starwars)
num_app_inputs <- function(df) {
    bins <- binary_app_inputs(df = df)
    facets <- facet_app_inputs(df = df)
    # assemble
    all_bins_facets_list <- list(bins, facets)
    # reduce
    bins_facets_list <- purrr::compact(all_bins_facets_list)
    # vector
    bins_facets <- purrr::list_c(bins_facets_list)
    # vector of doubles
    dbls <- get_col_types(df = df, type = 'dbl', return_tbl = FALSE)
    # vector of integers
    ints <- get_col_types(df = df, type = 'int', return_tbl = FALSE)
    # assemble
    all_dbls_ints_list <- list(dbls, ints)
    # reduce
    dbls_ints_list <- purrr::compact(all_dbls_ints_list)
    # vector
    dbls_ints <- purrr::list_c(dbls_ints_list)
    # reduce
    nums_nms <- dbls_ints[dbls_ints %nin% bins_facets]
    # name
    nums <- purrr::set_names(nums_nms)
    return(nums)
}

#' Categorical app inputs
#'
#' @param df a `data.frame` or `tibble`
#'
#' @return character and factor column names
#' @export cat_app_inputs
#'
#' @importFrom purrr compact list_c set_names
#'
#' @examples
#' require(palmerpenguins)
#' require(dplyr)
#' cat_app_inputs(palmerpenguins::penguins)
#' cat_app_inputs(dplyr::starwars)
cat_app_inputs <- function(df) {
  bins <- binary_app_inputs(df = df)
  facets <- facet_app_inputs(df = df)
  # assemble
  all_bins_facets_list <- list(bins, facets)
  # reduce
  bins_facets_list <- purrr::compact(all_bins_facets_list)
  # vector
  bins_facets <- purrr::list_c(bins_facets_list)
  # remove these
  # characters
  chrs <- get_col_types(df = df, type = 'chr', return_tbl = FALSE)
  # factors
  fcts <- get_col_types(df = df, type = 'fct', return_tbl = FALSE)
  # assemble
  all_chrs_fcts_list <- list(chrs, fcts)
  # reduce
  chrs_fcts_list <- purrr::compact(all_chrs_fcts_list)
  # vector
  chrs_fcts <- purrr::list_c(chrs_fcts_list)
  # reduce
  cats_nms <- chrs_fcts[chrs_fcts %nin% bins_facets]
  # name
  cats <- purrr::set_names(cats_nms)
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
  # get bins
  bins <- binary_app_inputs(df)
  # character
  chr_facets <- get_col_types(df, "chr") |> make_facet_vec("chr")
  # factors
  fct_facets <- get_col_types(df, "fct") |> make_facet_vec("fct")
  # assemble
  all_facets_list <- list(chr_facets, fct_facets)
  # reduce
  facets_list <- purrr::compact(all_facets_list)
  # vector
  all_facets <- purrr::list_c(facets_list)
  # reduce
  facets <- all_facets[all_facets %nin% bins]
  return(facets)
}

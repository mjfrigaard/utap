#' Numeric app inputs
#'
#' @param df a `data.frame` or `tibble`
#'
#' @return a named character vector of integer and double column names
#' @export pull_numeric_cols
#'
#' @importFrom purrr compact list_c set_names
#'
#' @examples
#' require(palmerpenguins)
#' require(dplyr)
#' pull_numeric_cols(palmerpenguins::penguins)
#' pull_numeric_cols(dplyr::starwars)
pull_numeric_cols <- function(df) {
  bins <- pull_binary_cols(df = df)
  facets <- pull_facet_cols(df = df)
  # assemble
  all_bins_facets_list <- list(bins, facets)
  # reduce
  bins_facets_list <- purrr::compact(all_bins_facets_list)
  # vector
  bins_facets <- purrr::list_c(bins_facets_list)
  # vector of doubles
  dbls <- get_column_class(df = df, class = "dbl", return_tbl = FALSE)
  # vector of integers
  ints <- get_column_class(df = df, class = "int", return_tbl = FALSE)
  # assemble
  all_dbls_ints_list <- list(dbls, ints)
  # # reduce
  dbls_ints_list <- purrr::compact(all_dbls_ints_list)
  # # vector
  dbls_ints <- purrr::list_c(dbls_ints_list)
  # reduce
  nums_nms <- dbls_ints[dbls_ints %nin% bins_facets]
  if (is.null(nums_nms)) {
    return(nums_nms)
  } else {
    # name
    nums <- purrr::set_names(nums_nms)
    return(nums)
  }
}


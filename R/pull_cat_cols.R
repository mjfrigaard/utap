#' Categorical app inputs
#'
#' @param df a `data.frame` or `tibble`
#'
#' @return character and factor column names
#' @export pull_cat_cols
#'
#' @importFrom purrr compact list_c set_names
#'
#' @examples
#' require(palmerpenguins)
#' require(dplyr)
#' pull_cat_cols(palmerpenguins::penguins)
#' pull_cat_cols(dplyr::starwars)
pull_cat_cols <- function(df) {
  bins <- pull_binary_cols(df = df)
  facets <- pull_facet_cols(df = df)
  # assemble
  all_bins_facets_list <- list(bins, facets)
  # reduce
  bins_facets_list <- purrr::compact(all_bins_facets_list)
  # vector
  bins_facets <- purrr::list_c(bins_facets_list)
  # remove these
  # characters
  chrs <- get_column_class(df = df, class = "chr", return_tbl = FALSE)
  # factors
  fcts <- get_column_class(df = df, class = "fct", return_tbl = FALSE)
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

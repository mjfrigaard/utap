#' Facet checks (helper)
#'
#' @param x a vector
#' @param type type of column to return.
#'
#' @return TRUE/FALSE if facet variable (< 6 levels)
#' @export check_facet_vec
#'
#' @examples
#' require(NHANES)
#' check_facet_vec(NHANES::NHANES$Education, type = "fct")
#' levels(NHANES::NHANES$Education)
#' check_facet_vec(NHANES::NHANES$MaritalStatus, type = "fct")
#' levels(NHANES::NHANES$MaritalStatus)
check_facet_vec <- function(x, type) {
  check_chr_facet <- function(x) {
    length(unique(na.omit(x))) <= 5
  }
  check_fct_facet <- function(x) {
    length(unique(na.omit(x))) <= 5
  }
  switch(type,
    chr = check_chr_facet(x),
    ord = check_fct_facet(x),
    fct = check_fct_facet(x)
  )
}

#' Create vector of facet columns by type (helper)
#'
#' @param df  a `data.frame` or `tibble`
#' @param type type of column to return
#'
#' @return vector of facet columns (< 6 levels)
#' @export make_facet_vec
#'
#' @importFrom purrr set_names map_vec
#' @importFrom cli cli_alert_info
#'
#' @examples
#' require(NHANES)
#' require(dplyr)
#' facets <- make_facet_vec(
#'   df = dplyr::select(
#'     NHANES::NHANES,
#'     dplyr::where(is.factor)),
#'   type = "fct")
#' facets
make_facet_vec <- function(df, type) {
  if (ncol(df) < 1) {
    cli::cli_alert_info(glue::glue("No {type} facet columns!"))
    return(purrr::set_names(vector(mode = "character")))
  } else {
    nms <- names(df)
    # set names in names
    dm_nms <- purrr::set_names(nms)
    facet_set <- purrr::map_vec(
      .x = df,
      .f = check_facet_vec,
      type = type
    )
    if (sum(facet_set) < 1) {
      cli::cli_alert_info(glue::glue("No {type} facet values!"))
      facets <- purrr::set_names(vector(mode = "character"))
    } else {
      facets <- purrr::set_names(dm_nms[facet_set])
    }
  }
  return(facets)
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
#' @return a named character vector of factor or character column names with
#' < 6 unique levels
#' @export pull_facet_cols
#'
#' @importFrom purrr compact list_c
#'
#' @examples
#' require(dplyr)
#' require(NHANES)
#' pull_facet_cols(df = dplyr::starwars)
#' pull_facet_cols(df = NHANES::NHANES)
#'
#' str(dplyr::select(
#'   dplyr::starwars,
#'   dplyr::all_of(pull_facet_cols(df = dplyr::starwars))
#' ))
#' str(dplyr::select(
#'   NHANES::NHANES,
#'   dplyr::all_of(pull_facet_cols(df = NHANES::NHANES))
#' ))
pull_facet_cols <- function(df) {
  # get bins
  bins <- pull_binary_cols(df)
  # character
  chr_facets <- get_column_class(df, "chr") |> make_facet_vec("chr")
  # factors
  fct_facets <- get_column_class(df, "fct") |> make_facet_vec("fct")
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

#' Create vector of binary columns by type (helper)
#'
#' @param df  a `data.frame` or `tibble`
#' @param type type of column to return
#'
#' @return vector of binary columns
#' @export make_binary_vec
#'
#' @importFrom purrr set_names map_vec
#' @importFrom cli cli_alert_info
#'
#' @examples
#' require(palmerpenguins)
#' require(dplyr)
#' make_binary_vec(
#'   df = dplyr::select(
#'     palmerpenguins::penguins,
#'     dplyr::where(is.factor)
#'   ),
#'   type = "fct"
#' )
make_binary_vec <- function(df, type) {
  if (ncol(df) < 1) {
    # cli::cli_alert_info(glue::glue("No {type} binary columns!"))
    return(purrr::set_names(vector(mode = "character")))
  } else {
    nms <- names(df)
    # set names in names
    dm_nms <- purrr::set_names(nms)
    bin_set <- purrr::map_vec(
      .x = df,
      .f = check_binary_vec,
      type = type
    )
    if (sum(bin_set) < 1) {
      # cli::cli_alert_info(glue::glue("No {type} binary values!"))
      bins <- purrr::set_names(vector(mode = "character"))
    } else {
      # cli::cli_alert_success(glue::glue("{type} binary values!"))
      bins <- purrr::set_names(dm_nms[bin_set])
    }
  }
  return(bins)
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
    # cli::cli_alert_info(glue::glue("No {type} facet columns!"))
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
      # cli::cli_alert_info(glue::glue("No {type} facet values!"))
      facets <- purrr::set_names(vector(mode = "character"))
    } else {
      # cli::cli_alert_success(glue::glue("{type} facet values!"))
      facets <- purrr::set_names(dm_nms[facet_set])
    }
  }
  return(facets)
}

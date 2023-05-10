#' Get column types as tibble (helper)
#'
#' @description
#' Return column in data by types
#'
#' @param df a `data.frame` or `tibble`
#' @param type type of column to return.
#'  One of:
#'   * `"log"`: logical
#'   * `"dbl"`: double
#'   * `"int"`: integer
#'   * `"chr"`: character
#'   * `"fct"`: factor
#'   * `"list"`: list
#'
#' @return `tibble` of columns matching `type` (empty `tibble` is no columns
#' of specified type
#' exist)
#' @export get_col_type_tbl
#'
#' @importFrom cli cli_abort cli_alert_info
#' @importFrom dplyr select where
#' @importFrom tibble as_tibble tibble
#' @importFrom glue glue
#'
#' @examples
#' require(dplyr)
#' require(tidyr)
#' get_col_type_tbl(dplyr::starwars, type = "chr")
#' get_col_type_tbl(dplyr::starwars, type = "list")
#' get_col_type_tbl(tidyr::fish_encounters, "chr")
get_col_type_tbl <- function(df, type) {

  if (type %nin% c("log", "int", "dbl", "chr", "fct", "list")) {
    cli::cli_abort("Invalid `type` argument. Must be one of:\n
          'log', 'int', 'dbl', 'chr', 'fct', 'list'")
  }

  df_cols <- switch(type,
    log = dplyr::select(tibble::as_tibble(df), dplyr::where(is.logical)),
    int = dplyr::select(tibble::as_tibble(df), dplyr::where(is.integer)),
    dbl = dplyr::select(tibble::as_tibble(df), dplyr::where(is.double)),
    chr = dplyr::select(tibble::as_tibble(df), dplyr::where(is.character)),
    fct = dplyr::select(tibble::as_tibble(df), dplyr::where(is.factor)),
    list = dplyr::select(tibble::as_tibble(df), dplyr::where(is.list)))

  if (ncol(df_cols) < 1 || nrow(df_cols) < 1 ) {
    cli::cli_alert_info(glue::glue("No {type} columns"))
    df_cols <- structure(list(),
                         class = c("tbl_df", "tbl", "data.frame"),
                         row.names = integer(0),
                         names = character(0))
    return(df_cols)
  } else {
    return(df_cols)
  }

}

#' Get column types
#'
#' @description
#' Return column in data by types
#'
#' @param df a `data.frame` or `tibble`
#' @param return_tbl logical, return tibble (`TRUE`) or named vector (`FALSE`)
#' @param type type of column to return
#'  One of:
#'   * `"log"`: logical
#'   * `"dbl"`: double
#'   * `"int"`: integer
#'   * `"chr"`: character
#'   * `"fct"`: factor
#'   * `"list"`: list
#'
#' @return named vector or `tibble` of columns matching `type` (empty vector or
#' `tibble` if no columns of specified type exist)
#'
#' @export get_col_types
#'
#' @importFrom purrr set_names
#'
#' @examples
#' require(dplyr)
#' require(tidyr)
#' get_col_types(dplyr::starwars, type = "chr")
#' get_col_types(dplyr::starwars, type = "chr", return_tbl = FALSE)
#' get_col_types(dplyr::starwars, type = "list")
#' get_col_types(dplyr::starwars, type = "list", return_tbl = FALSE)
#' get_col_types(tidyr::fish_encounters, "chr")
#' get_col_types(tidyr::fish_encounters, "chr", return_tbl = FALSE)
get_col_types <- function(df, type, return_tbl = TRUE) {

  if (isFALSE(return_tbl)) {

    col_types_df <- get_col_type_tbl(df, type = type)
    nms <- names(col_types_df)
    col_types <- purrr::set_names(nms)

  } else {

    col_types <- get_col_type_tbl(df, type = type)

  }

  return(col_types)

}

#' Binary checks (helper)
#'
#' @param x a vector
#' @param type type of column to return.
#'
#' @return TRUE/FALSE if binary
#' @export check_binary_vec
#'
#'
#' @examples
#' require(palmerpenguins)
#' check_binary_vec(palmerpenguins::penguins$sex, type = "fct")
#' check_binary_vec(palmerpenguins::penguins$species, type = "fct")
check_binary_vec <- function(x, type) {
    check_log_binary <- function(x) { all(na.omit(x) %in% TRUE:FALSE) }
    check_int_binary <- function(x) { all(na.omit(x) %in% 0:1) }
    check_chr_binary <- function(x) { length(unique(na.omit(x))) == 2 }
    check_fct_binary <- function(x) { length(levels(na.omit(x))) == 2 }
  switch(type,
    log = check_log_binary(x),
    int = check_int_binary(x),
    chr = check_chr_binary(x),
    fct = check_fct_binary(x))
}


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
      check_chr_facet <- function(x) { length(unique(na.omit(x))) <= 5 }
      check_fct_facet <- function(x) { length(levels(na.omit(x))) <= 5 }
        switch(type,
            chr = check_chr_facet(x),
            fct = check_fct_facet(x))
}

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
#' bins <- make_binary_vec(
#'             df = dplyr::select(palmerpenguins::penguins,
#'                           dplyr::where(is.factor)),
#'             type = "fct")
#' bins
make_binary_vec <- function(df, type) {
  if (ncol(df) < 1) {
    return(purrr::set_names(vector(mode = "character")))
  } else {
    nms <- names(df)
    # set names in names
    dm_nms <- purrr::set_names(nms)
    bin_set <- purrr::map_vec(
                    .x = df,
                    .f = check_binary_vec,
                    type = type)
    if (sum(bin_set) < 1) {
      cli::cli_alert_info(glue::glue("No {type} binary values!"))
      bins <- purrr::set_names(vector(mode = "character"))
    } else {
      cli::cli_alert_success(glue::glue("{type} binary values!"))
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
#'             df = dplyr::select(NHANES::NHANES,
#'                           dplyr::where(is.factor)),
#'             type = "fct")
#' facets
make_facet_vec <- function(df, type) {
  if (ncol(df) < 1) {
    return(purrr::set_names(vector(mode = "character")))
  } else {
    nms <- names(df)
    # set names in names
    dm_nms <- purrr::set_names(nms)
    facet_set <- purrr::map_vec(
                      .x = df,
                      .f = check_facet_vec,
                      type = type)
    if (sum(facet_set) < 1) {
      cli::cli_alert_info(glue::glue("No {type} facet values!"))
      facets <- purrr::set_names(vector(mode = "character"))
    } else {
      cli::cli_alert_success(glue::glue("{type} facet values!"))
      facets <- purrr::set_names(dm_nms[facet_set])
    }
  }
  return(facets)
}


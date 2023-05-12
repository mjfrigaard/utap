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
    list = dplyr::select(tibble::as_tibble(df), dplyr::where(is.list))
  )

  if (ncol(df_cols) < 1 || nrow(df_cols) < 1) {
    # cli::cli_alert_info(glue::glue("No {type} columns"))
    df_cols <- structure(list(),
      class = c("tbl_df", "tbl", "data.frame"),
      row.names = integer(0),
      names = character(0)
    )
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

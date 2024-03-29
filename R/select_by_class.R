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
#'   * `"ord"`: factor (ordered)
#'   * `"list"`: list
#'
#' @return named vector or `tibble` of columns matching the `type` (empty vector or
#' `tibble` if no columns of specified type exist)
#'
#' @export
#'
#'
#' @examples
#' select_by_class(dplyr::starwars, class = "chr")
#' select_by_class(dplyr::starwars, class = "chr", return_tbl = FALSE)
#' select_by_class(dplyr::starwars, class = "list")
#' select_by_class(dplyr::starwars, class = "list", return_tbl = FALSE)
#' select_by_class(tidyr::fish_encounters, "chr")
#' select_by_class(tidyr::fish_encounters, "chr", return_tbl = FALSE)
select_by_class <- function(df, class, return_tbl = TRUE) {

  select_column_class <- function(df, class) {

    col_class <- function(df, class) {
      switch(class,
        log = dplyr::select(tibble::as_tibble(df), dplyr::where(is.logical)),
        int = dplyr::select(tibble::as_tibble(df), dplyr::where(is.integer)),
        dbl = dplyr::select(tibble::as_tibble(df), dplyr::where(is.double)),
        chr = dplyr::select(tibble::as_tibble(df), dplyr::where(is.character)),
        fct = dplyr::select(tibble::as_tibble(df), dplyr::where(is.factor)),
        ord = dplyr::select(tibble::as_tibble(df), dplyr::where(is.ordered)),
        list = dplyr::select(tibble::as_tibble(df), dplyr::where(is.list))
      )
    }

    cl <- unique(class)
    cl_check <- cl %nin% c("log", "int", "dbl", "chr", "fct", "ord", "list")
    if (any(cl_check)) {
      cli::cli_abort("Invalid `class` argument. Must be one of:\n
          'log', 'int', 'dbl', 'chr', 'fct', 'ord', 'list'")
    }

    col_list <- purrr::map(.x = class, .f = col_class, df = df)

    df_cols <- purrr::list_cbind(col_list, size = nrow(df))

    if (ncol(df_cols) < 1 || nrow(df_cols) < 1) {
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

  if (isFALSE(return_tbl)) {

    col_types_df <- select_column_class(df, class = class)
    nms <- names(col_types_df)
    col_types <- purrr::set_names(nms)

  } else {

    col_types <- select_column_class(df, class = class)

  }

  return(col_types)

}

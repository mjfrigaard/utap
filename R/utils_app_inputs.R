#' Get column types (helper)
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
#' @return columns of `type` (empty `tibble` is no columns of specified type
#' exist)
#' @export get_col_type_df
#'
#' @examples
#' require(dplyr)
#' get_col_type_df(dplyr::starwars, type = "chr")
#' get_col_type_df(dplyr::starwars, type = "list")
get_col_type_df <- function(df, type) {

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
    df_cols <- structure(list(),
                         class = c("tbl_df", "tbl", "data.frame"),
                         row.names = integer(0),
                         names = character(0))
    return(df_cols)
    cli::cli_alert_info("No columns of that type...")
  } else {
    return(df_cols)
  }

}

#' Binary checks (vectors)
#'
#' @param x a vector
#' @param type type of column to return.
#'
#' @return TRUE/FALSE if binary
#' @export vec_binary_checks
#'
#' @examples
#' require(palmerpenguins)
#' vec_binary_checks(palmerpenguins::penguins$sex, type = "fct")
vec_binary_checks <- function(x, type) {
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

#' Check for binary columns (helper)
#'
#' @param df  a `data.frame` or `tibble`
#' @param type type of column to return
#'
#' @return vector of binary columns
#' @export df_binary_checks
#'
#' @examples
#' require(palmerpenguins)
#' df_binary_checks(palmerpenguins::penguins, type = "fct")
df_binary_checks <- function(df, type) {
  if (ncol(df) < 1) {
    return(purrr::set_names(vector(mode = "character")))
  } else {
    nms <- names(df)
    # set names in names
    dm_nms <- purrr::set_names(nms)
    bin_set <- purrr::map_vec(.x = df, .f = vec_binary_checks, type = type)
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

#' Get binary columns from data
#'
#' @param df a `data.frame` or `tibble`
#'
#' @return vector of binary column names
#' @export get_binary_cols
#'
#' @examples
#' require(dplyr)
#' get_binary_cols(dplyr::starwars)
get_binary_cols <- function(df) {
  # logical
  log_bins <- get_col_type_df(df, "log") |>
              df_binary_checks("log")
  # integer
  int_bins <- get_col_type_df(df, "int") |>
              df_binary_checks("int")
  # character
  chr_bins <- get_col_type_df(df, "chr") |>
              df_binary_checks("chr")
  # factors
  fct_bins <- get_col_type_df(df, "fct") |>
              df_binary_checks("fct")
  # assemble
  all_bins <- list(log_bins, int_bins, chr_bins, fct_bins)
  # reduce
  bins_list <- purrr::compact(all_bins)
  # vector
  bins <- purrr::list_c(bins_list)
  return(bins)
}

#' Facet variables (as vector)
#'
#' @section Variables to use for facets:
#'
#' This function is designed to quickly determine which variables have an
#' appropriate number of categorical levels for using `ggplot2::facet_wrap()`
#' or `ggplot2::facet_grid()`
#'
#' @param df a `data.frame` or `tibble`
#'
#' @return a vector of factor or character column names with less than six
#'   unique levels
#'
#' @export get_facet_cols
#'
#' @importFrom purrr set_names
#'
#' @examples
#' require(dplyr)
#' require(NHANES)
#' get_facet_cols(df = dplyr::starwars)
#' get_facet_cols(df = NHANES::NHANES)
#'
#' str(dplyr::select(dplyr::starwars,
#'   dplyr::all_of(get_facet_cols(df = dplyr::starwars))))
#' str(dplyr::select(NHANES::NHANES,
#'   dplyr::all_of(get_facet_cols(df = NHANES::NHANES))))
get_facet_cols <- function(df) {
  chr_nms <- names(dplyr::select(df, dplyr::where(is.character)))
  fct_nms <- names(dplyr::select(df, dplyr::where(is.factor)))
  cat_df <- dplyr::select(df, dplyr::all_of(c(chr_nms, fct_nms)))
  if (length(cat_df) > 0) {
    # get names
    nms <- names(cat_df)
    # set names in names
    df_nms <- purrr::set_names(nms)
    # check for facet variables
    check_facet_levels <- function(x) {
      length(unique(na.omit(x))) < 6
    }
    # get TRUE/FALSE facets
    facets <- sapply(cat_df, check_facet_levels)
    # subset names with facet vars
    df_nms[facets]
  }
}

#' Data column type
#'
#' @param df a `data.frame` or `tibble`
#' @param type type of column to return.
#'  One of:
#'   * "log": logical
#'   * "dbl": double
#'   * "int": integer
#'   * "chr": character
#'   * "fct": factor
#'   * "lst": list
#'   * "date": date (from `lubridate::is.Date`)
#'   * "posixct": date (from `lubridate::is.POSIXct`)
#'   * "posixlt": date (from `lubridate::is.POSIXlt`)
#'   * "posix": date (from `lubridate::is.POSIXt`)
#'   * "facet": facet variables (less than six levels)
#'   * "binary": binary variables (two levels)
#'
#' @return vector of names from df matching `type`
#' @export get_col_types
#'
#' @examples
#' require(palmerpenguins)
#' require(dplyr)
#' get_col_types(df = palmerpenguins::penguins, type = 'dbl')
#' get_col_types(df = palmerpenguins::penguins, type = 'int')
get_col_types <- function(df, type) {
  nms <- switch(type,
    log = names(dplyr::select(df, dplyr::where(is.logical))),
    dbl = names(dplyr::select(df, dplyr::where(is.double))),
    int = names(dplyr::select(df, dplyr::where(is.integer))),
    chr = names(dplyr::select(df, dplyr::where(is.character))),
    fct = names(dplyr::select(df, dplyr::where(is.factor))),
    lst = names(dplyr::select(df, dplyr::where(is.list))),
    date = names(dplyr::select(df, dplyr::where(lubridate::is.Date))),
    posixct = names(dplyr::select(df, dplyr::where(lubridate::is.POSIXct))),
    posixlt = names(dplyr::select(df, dplyr::where(lubridate::is.POSIXlt))),
    posix = names(dplyr::select(df, dplyr::where(lubridate::is.POSIXt))),
    facet = get_facet_cols(df = df),
    binary = get_binary_cols(df)
  )
  named_nms <- purrr::set_names(nms)
  return(named_nms)
}

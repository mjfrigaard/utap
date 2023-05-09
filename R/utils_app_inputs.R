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
    cli::cli_abort("No columns of that type...")
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

#' Binary checks (helper)
#'
#' @param x a vector
#' @param type type of column to return.
#'
#' @return TRUE/FALSE if binary
#' @export get_binary_checks_vec
#'
#' @examples
#' require(palmerpenguins)
#' get_binary_checks_vec(palmerpenguins::penguins$sex, type = "fct")
#' get_binary_checks_vec(palmerpenguins::penguins$species, type = "fct")
get_binary_checks_vec <- function(x, type) {
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
#' @export get_facet_checks_vec
#'
#' @examples
#' require(NHANES)
#' get_facet_checks_vec(NHANES::NHANES$Education, type = "fct")
#' levels(NHANES::NHANES$Education)
#' get_facet_checks_vec(NHANES::NHANES$MaritalStatus, type = "fct")
#' levels(NHANES::NHANES$MaritalStatus)
get_facet_checks_vec <- function(x, type) {
      check_chr_facet <- function(x) { length(unique(na.omit(x))) < 6 }
      check_fct_facet <- function(x) { length(levels(na.omit(x))) < 6 }
        switch(type,
            chr = check_chr_facet(x),
            fct = check_fct_facet(x))
}

#' Create vector of binary columns from data.frame or tibble (helper)
#'
#' @param df  a `data.frame` or `tibble`
#' @param type type of column to return
#'
#' @return vector of binary columns
#' @export make_binary_checks_df
#'
#' @examples
#' require(palmerpenguins)
#' require(dplyr)
#' bins <- make_binary_checks_df(
#'             df = dplyr::select(palmerpenguins::penguins,
#'                           dplyr::where(is.factor)),
#'             type = "fct")
#' bins
make_binary_checks_df <- function(df, type) {
  if (ncol(df) < 1) {
    return(purrr::set_names(vector(mode = "character")))
  } else {
    nms <- names(df)
    # set names in names
    dm_nms <- purrr::set_names(nms)
    bin_set <- purrr::map_vec(
                    .x = df,
                    .f = get_binary_checks_vec,
                    type = type)
    if (sum(bin_set) < 1) {
      cli::cli_alert_info("No values of that type!")
      bins <- purrr::set_names(vector(mode = "character"))
    } else {
      cli::cli_alert_success("Values of that type!")
      bins <- purrr::set_names(dm_nms[bin_set])
    }
  }
  return(bins)
}

#' Create vector of facet columns from data.frame or tibble (helper)
#'
#' @param df  a `data.frame` or `tibble`
#' @param type type of column to return
#'
#' @return vector of facet columns (< 6 levels)
#' @export make_facet_checks_df
#'
#' @examples
#' require(NHANES)
#' require(dplyr)
#' facets <- make_facet_checks_df(
#'             df = dplyr::select(NHANES::NHANES,
#'                           dplyr::where(is.factor)),
#'             type = "fct")
#' facets
make_facet_checks_df <- function(df, type) {
  if (ncol(df) < 1) {
    return(purrr::set_names(vector(mode = "character")))
  } else {
    nms <- names(df)
    # set names in names
    dm_nms <- purrr::set_names(nms)
    facet_set <- purrr::map_vec(
                      .x = df,
                      .f = get_facet_checks_vec,
                      type = type)
    if (sum(facet_set) < 1) {
      cli::cli_alert_info("No values of that type!")
      facets <- purrr::set_names(vector(mode = "character"))
    } else {
      cli::cli_alert_success("Values of that type!")
      facets <- purrr::set_names(dm_nms[facet_set])
    }
  }
  return(facets)
}

#' Get binary columns from data
#'
#' @param df a `data.frame` or `tibble`
#'
#' @return vector of binary column names
#' @export make_binary_cols_vec
#'
#' @examples
#' require(dplyr)
#' make_binary_cols_vec(dplyr::starwars)
make_binary_cols_vec <- function(df) {
  # logical
  log_bins <- get_col_type_df(df, "log") |>
              make_binary_checks_df("log")
  # integer
  int_bins <- get_col_type_df(df, "int") |>
              make_binary_checks_df("int")
  # character
  chr_bins <- get_col_type_df(df, "chr") |>
              make_binary_checks_df("chr")
  # factors
  fct_bins <- get_col_type_df(df, "fct") |>
              make_binary_checks_df("fct")
  # assemble
  all_bins <- list(log_bins, int_bins, chr_bins, fct_bins)
  # reduce
  bins_list <- purrr::compact(all_bins)
  # vector
  bins <- purrr::list_c(bins_list)
  return(bins)
}

#' Get facet columns from data
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
#' @export make_facet_cols_vec
#'
#' @examples
#' require(dplyr)
#' require(NHANES)
#' make_facet_cols_vec(df = dplyr::starwars)
#' make_facet_cols_vec(df = NHANES::NHANES)
#'
#' str(dplyr::select(dplyr::starwars,
#'   dplyr::all_of(make_facet_cols_vec(df = dplyr::starwars))))
#' str(dplyr::select(NHANES::NHANES,
#'   dplyr::all_of(make_facet_cols_vec(df = NHANES::NHANES))))
make_facet_cols_vec <- function(df) {
  # character
  chr_facets <- get_col_type_df(df, "chr") |>
              make_facet_checks_df("chr")
  # factors
  fct_facets <- get_col_type_df(df, "fct") |>
              make_facet_checks_df("fct")
  # assemble
  all_facets <- list(chr_facets, fct_facets)
  # reduce
  facets_list <- purrr::compact(all_facets)
  # vector
  facets <- purrr::list_c(facets_list)
  return(facets)
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
#' @export make_col_types_vec
#'
#' @examples
#' require(palmerpenguins)
#' require(dplyr)
#' make_col_types_vec(df = palmerpenguins::penguins, type = 'dbl')
#' make_col_types_vec(df = palmerpenguins::penguins, type = 'int')
make_col_types_vec <- function(df, type) {
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
    facet = make_facet_cols_vec(df = df),
    binary = make_binary_cols_vec(df)
  )
  named_nms <- purrr::set_names(nms)
  return(named_nms)
}

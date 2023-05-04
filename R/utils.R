#' Facet variables (as vector)
#'
#' @param df a data frame or tibble
#'
#' @return vector of factor or character columns with less than six levels
#'
#' @export facet_vars
#'
#' @importFrom purrr map_vec
#' @importFrom dplyr select where
#'
#' @examples
#' require(tibble)
#' df_test <- tibble::tibble(
#'   a = sample(c(TRUE, FALSE), 10, TRUE),
#'   b = c(1.5:10.5),
#'   c = c(1:10),
#'   d1 = rep(LETTERS[1:5], times = 2),
#'   d2 = LETTERS[1:10],
#'   e = sample(c(
#'     Sys.Date(), Sys.Date() + 1,
#'     Sys.Date() + 2, Sys.Date() + 3,
#'     Sys.Date() + 4
#'   ), size = 10, replace = TRUE),
#'   f1 = factor(rep(letters[1:5], times = 2),
#'     levels = letters[1:5]
#'   ),
#'   f2 = factor(letters[1:10],
#'     levels = letters[1:10]
#'   )
#' )
#' facet_vars(df_test)
facet_vars <- function(df) {
  df_names <- names(df)
  levels_check <- function(df) {
    purrr::map_vec(df, function(x) length(unique(x)))
  }
  df_levels <- levels_check(df = df)
  facet_levels_check <- function(x) {
    if (x < 6) {
      x < 6
    } else {
      FALSE
    }
  }
  facet_levels <- map_vec(df_levels, facet_levels_check)
  facet_set <- df_names[facet_levels]

  chr_nms <- names(dplyr::select(df,
                  dplyr::where(is.character)))
  fct_nms <- names(dplyr::select(df,
                  dplyr::where(is.factor)))

  nms <- c(chr_nms, fct_nms)

  facet_vars <- intersect(facet_set, nms)

  return(facet_vars)
}

#' Get data variable types (as list)
#'
#' @param df a data frame or tibble
#'
#' @return list of vectors by type (`is_logical`, `is_double`, `is_integer`,
#'   `is_character`, `is_factor`, `is_facet_var`, `is_date`, `is_POSIXct`,
#'   `is_POSIXlt`)
#'
#' @export get_var_types
#'
#' @examples
#' require(dplyr)
#' get_var_types(dplyr::starwars)
#' get_var_types(dplyr::storms)
#' # great with zealott!
#' require(zeallot)
#' c(dbl_vars, int_vars,
#'   fct_vars, facet_vars) %<-% get_var_types(penguins)
#' dbl_vars
#' int_vars
#' fct_vars
#' facet_vars
get_var_types <- function(df) {
  # atomic
  log_names <- dplyr::select(
    df,
    dplyr::where(is.logical)
  ) |> names()

  dbl_names <- dplyr::select(
    df,
    dplyr::where(is.double)
  ) |> names()

  int_names <- dplyr::select(
    df,
    dplyr::where(is.integer)
  ) |> names()

  chr_names <- dplyr::select(
    df,
    dplyr::where(is.character)
  ) |> names()

  fct_names <- dplyr::select(
    df,
    dplyr::where(is.factor)
  ) |> names()

  facet_vars <- facet_vars(df)

  # s3
  date_names <- dplyr::select(
    df,
    dplyr::where(lubridate::is.Date)
  ) |> names()

  posixct_names <- dplyr::select(
    df,
    dplyr::where(lubridate::is.POSIXct)
  ) |> names()

  posixlt_names <- dplyr::select(
    df,
    dplyr::where(lubridate::is.POSIXlt)
  ) |> names()

  posix_names <- dplyr::select(
    df,
    dplyr::where(lubridate::is.POSIXt)
  ) |> names()

  list_names <- dplyr::select(
    df,
    dplyr::where(is.list)
  ) |> names()

    all_vars_list <- list(
      "is_logical" = log_names,
      "is_double" = dbl_names,
      "is_integer" = int_names,
      "is_character" = chr_names,
      "is_factor" = fct_names,
      "is_facet_var" = facet_vars,
      "is_date" = date_names,
      "is_posixct" = posixct_names,
      "is_posixlt" = posixlt_names,
      "is_posixt" = posix_names,
      "is_list" = list_names)

  vars_list <- purrr::compact(all_vars_list)

  return(vars_list)

}

#' Deconstruct R objects
#'
#' @param x R object passed to `dput()`
#' @param quotes include quotes in the output
#' @param console logical, used in the console? If `FALSE`, then output is printed
#' with `base::noquote()`. If `TRUE`, output is returned with `cat()`
#'
#' @return Deparsed object
#' @export deconstruct
#'
#' @examples
#' deconstruct(names(mtcars), quotes = FALSE)
#' deconstruct(names(mtcars), quotes = FALSE, console = FALSE)
#' deconstruct(x = names(mtcars), quotes = TRUE, console = TRUE)
deconstruct <- function(x, quotes = TRUE, console = TRUE) {
  if (isFALSE(quotes)) {
    raw_obj <- capture.output(dput(x, control = "all"))
    obj <- gsub(pattern = '"', replacement = "", x = raw_obj)
  } else {
    raw_obj <- capture.output(dput(x, control = "all"))
    obj <- gsub(pattern = '"', replacement = "'", x = raw_obj)
  }
  if (isFALSE(console)) {
    return(base::noquote(obj))
  } else {
    cat(base::noquote(obj))
  }
}

#' Get UI inputs
#'
#' @param app_data dataset for app (`data.frame` or `tibble`)
#'
#' @return zeallot assignment (`%<-%`) with input character vector on LHS and
#'    list of names by type on the RHS
#' @export get_ui_inputs
#'
#' @examples
#' require(dplyr)
#' get_ui_inputs(dplyr::starwars)
get_ui_inputs <- function(app_data) {

  ui_inputs <- names(get_var_types(df = app_data))

  input_vector <- deconstruct(x = ui_inputs,
                          quotes = FALSE, console = FALSE)

  var_list <- deconstruct(x = get_var_types(df = app_data),
    quotes = TRUE, console = FALSE)

  cat(input_vector, "%<-%", var_list)

}

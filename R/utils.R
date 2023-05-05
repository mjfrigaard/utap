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
#' @export facet_vars
#'
#' @importFrom purrr set_names
#'
#' @examples
#' require(dplyr)
#' require(NHANES)
#' facet_vars(df = dplyr::starwars)
#' facet_vars(df = NHANES::NHANES)
#'
#' str(dplyr::select(dplyr::starwars,
#'   dplyr::all_of(facet_vars(df = dplyr::starwars))))
#' str(dplyr::select(NHANES::NHANES,
#'   dplyr::all_of(facet_vars(df = NHANES::NHANES))))
facet_vars <- function(df) {
  # remove missing
  complete_df <- sapply(df, na.omit)
  # get names
  nms <- names(complete_df)
  # set names in names
  df_nms <- purrr::set_names(nms)
  # check for binary variables
  check_levels <- function(x) {
    length(unique(x)) < 6
  }
  # get TRUE/FALSE facets
  facet_log <- sapply(complete_df, check_levels)
  # subset names with facet vars
  df_nms[facet_log]
}

#' Non-factor binary variables
#'
#' @param df a `data.frame` or `tibble`
#'
#' @return a vector of character or logical column names with two unique values
#'
#' @importFrom dplyr select where
#' @importFrom purrr set_names
#' @importFrom tidyr drop_na
#' @importFrom tibble as_tibble
#'
#' @export non_fct_binary_vars
#'
non_fct_binary_vars <- function(df) {
  no_fct_df <- dplyr::select(tibble::as_tibble(df),
                  !dplyr::where(is.factor))
  # remove missing
  complete_df <- tidyr::drop_na(no_fct_df)
  # # get names
  nms <- names(complete_df)
  # # # set names in names
  df_nms <- purrr::set_names(nms)
  # check for binary variables
  check_binary <- function(x) {
    length(unique(x)) == 2
  }
  # get TRUE/FALSE binaries
  bin_log <- sapply(complete_df, check_binary)
  # # subset names with binaries
  df_nms[bin_log]
}

#' Factor binary variables
#'
#' @param df a `data.frame` or `tibble`
#'
#' @return a vector of factor column names with two unique levels
#'
#' @importFrom dplyr select where
#' @importFrom purrr set_names
#' @importFrom tibble as_tibble
#' @importFrom tidyr drop_na
#'
#' @export fct_binary_vars
#'
fct_binary_vars <- function(df) {
  fct_df <- dplyr::select(tibble::as_tibble(df), # convert to tibble
                          dplyr::where(is.factor))
  # remove missing
  complete_df <- tidyr::drop_na(fct_df)
  # get names
  nms <- names(complete_df)
  # set names in names
  df_nms <- purrr::set_names(nms)
  # check levels
  check_levels <- function(x) {
    length(levels(x)) == 2
  }
  bin_log <- sapply(complete_df, check_levels)
  # subset factor names with binaries
  df_nms[bin_log]
}

#' Binary variables (as vector)
#'
#' @section Variables with binary (two levels):
#'
#' This function is designed to quickly determine which variables have two
#' categorical levels. This is helpful for using `ggplot2::facet_wrap()`
#' or `ggplot2::facet_grid()`
#'
#' @param df a `data.frame` or `tibble`
#'
#' @return a vector of factor or character column names with two unique levels
#'
#' @export binary_vars
#'
#' @importFrom purrr set_names
#' @importFrom tibble as_tibble
#' @importFrom dplyr select where
#'
#' @examples
#' require(NHANES)
#' require(palmerpenguins)
#' binary_vars(df = NHANES::NHANES)
#' binary_vars(df = palmerpenguins::penguins)
#' # verify with str()
#' str(dplyr::select(NHANES::NHANES,
#'   dplyr::all_of(binary_vars(df = NHANES::NHANES))))
#' str(dplyr::select(palmerpenguins::penguins,
#'   dplyr::all_of(binary_vars(df = palmerpenguins::penguins))))
binary_vars <- function(df) {
  # non factors
  non_fct_cols <- ncol(dplyr::select(tibble::as_tibble(df),
                       !dplyr::where(is.factor)))
  # factors
  fct_cols <- ncol(dplyr::select(tibble::as_tibble(df),
                        dplyr::where(is.factor)))

  # if both column types exist
  if (non_fct_cols > 1 & fct_cols > 1) {
    non_fcts <- non_fct_binary_vars(df = df)
    fcts <- fct_binary_vars(df = df)
    bin_vars <- c(non_fcts, fcts)
    # if only non-factor columns exist
    return(bin_vars)
  } else if (non_fct_cols > 1 & fct_cols == 0) {
    bin_vars <- non_fct_binary_vars(df = df)
    return(bin_vars)
    # if only factor columns exist
  } else if (non_fct_cols == 0 & fct_cols > 1) {
    bin_vars <- fct_binary_vars(df = df)
    return(bin_vars)
    # some other kind of column (list?)
  } else {
    cli::cli_abort("Variables can't be converted to binary:
      (i.e., logical, character, factor)")
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
#' @export col_type
#'
#' @examples
#' require(palmerpenguins)
#' require(dplyr)
#' col_type(df = palmerpenguins::penguins, type = 'dbl')
#' col_type(df = palmerpenguins::penguins, type = 'int')
#' col_type(df = dplyr::starwars, type = 'facet')
col_type <- function(df, type) {
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
    facet = facet_vars(df),
    binary = binary_vars(df),
  )
  named_nms <- purrr::set_names(nms)
  return(named_nms)
}

#' Numeric app inputs
#'
#' @param df a `data.frame` or `tibble`
#'
#' @return integer and double column names
#' @export num_app_inputs
#'
#' @examples
#' require(palmerpenguins)
#' require(dplyr)
#' num_app_inputs(palmerpenguins::penguins)
#' num_app_inputs(dplyr::starwars)
num_app_inputs <- function(df) {
  dbls <- col_type(df = df, type = 'dbl')
  ints <- col_type(df = df, type = 'int')
  nums <- c(dbls, ints)
  return(nums)
}

#' Categorical app inputs
#'
#' @param df a `data.frame` or `tibble`
#'
#' @return character and factor column names
#' @export cat_app_inputs
#'
#' @examples
#' require(palmerpenguins)
#' require(dplyr)
#' cat_app_inputs(palmerpenguins::penguins)
#' cat_app_inputs(dplyr::starwars)
cat_app_inputs <- function(df) {
  chrs <- col_type(df = df, type = 'chr')
  fcts <- col_type(df = df, type = 'fct')
  cats <- c(chrs, fcts)
  return(cats)
}

#' Facet app inputs
#'
#' @param df a `data.frame` or `tibble`
#'
#' @return character and factor column names with less than six levels
#' @export facet_app_inputs
#'
#' @examples
#' require(palmerpenguins)
#' require(dplyr)
#' facet_app_inputs(palmerpenguins::penguins)
#' facet_app_inputs(dplyr::starwars)
facet_app_inputs <- function(df) {
  facets <- col_type(df = df, type = 'facet')
  return(facets)
}

#' Binary app inputs
#'
#' @param df a `data.frame` or `tibble`
#'
#' @return logical, character or factor column names with two levels
#' @export binary_app_inputs
#'
#' @examples
#' require(palmerpenguins)
#' require(dplyr)
#' binary_app_inputs(palmerpenguins::penguins)
#' binary_app_inputs(dplyr::starwars)
binary_app_inputs <- function(df) {
  bins <- col_type(df = df, type = 'binary')
  return(bins)
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

#' Make UI inputs
#'
#' @description
#' This is meant to be used in the console--it generates the code for assigning
#' the elements from a list into a collection of vectors.
#'
#' @param app_data dataset for app (`data.frame` or `tibble`)
#'
#' @return zeallot assignment (`%<-%`) with input character vector on LHS and
#'    list of names by type on the RHS
#'
#' @export make_ui_inputs
#'
#' @examples
#' require(dplyr)
#' make_ui_inputs(dplyr::starwars)
make_ui_inputs <- function(app_data) {
  ui_inputs <- names(col_type_list(df = app_data))
  input_vector <- deconstruct(x = ui_inputs,
                          quotes = FALSE, console = FALSE)
  var_list <- deconstruct(x = col_type_list(df = app_data),
    quotes = TRUE, console = FALSE)
  cat(input_vector, "%<-%", var_list)
}

#' Get data variable types (as list)
#'
#' @param df a data frame or tibble
#'
#' @return list of vectors by type (`is_logical`, `is_double`, `is_integer`,
#'   `is_character`, `is_factor`, `is_facet_var`, `is_date`, `is_POSIXct`,
#'   `is_POSIXlt`)
#'
#' @export col_type_list
#'
#' @examples
#' require(dplyr)
#' col_type_list(dplyr::starwars)
#' col_type_list(dplyr::storms)
#' # great with zealott!
#' require(zeallot)
#' c(dbl_vars, int_vars,
#'   fct_vars, facet_vars) %<-% col_type_list(penguins)
#' dbl_vars
#' int_vars
#' fct_vars
#' facet_vars
col_type_list <- function(df) {
  # atomic
  log_names <- names(dplyr::select(df,
                    dplyr::where(is.logical)))
    dbl_names <- names(dplyr::select(df,
                    dplyr::where(is.double)))
    int_names <- names(dplyr::select(df,
                    dplyr::where(is.integer)))
    chr_names <- names(dplyr::select(df,
                    dplyr::where(is.character)))

    # s3
    fct_names <- names(dplyr::select(df,
                    dplyr::where(is.factor)))
    date_names <- names(dplyr::select(df,
                    dplyr::where(lubridate::is.Date)))
    posixct_names <- names(dplyr::select(df,
                    dplyr::where(lubridate::is.POSIXct)))
    posixlt_names <- names(dplyr::select(df,
                    dplyr::where(lubridate::is.POSIXlt)))
    posix_names <- names(dplyr::select(df,
                    dplyr::where(lubridate::is.POSIXt)))
    list_names <- names(dplyr::select(df,
                    dplyr::where(is.list)))
    # special
    facet_vars <- facet_vars(df)
    binary_vars <- binary_vars(df)
    # assemble
    all_vars_list <- list(
      "logical_vars" = log_names,
      "double_vars" = dbl_names,
      "integer_vars" = int_names,
      "character_vars" = chr_names,
      "factor_vars" = fct_names,
      "binary_vars" = bin_names,
      "facet_vars" = facet_vars,
      "date_vars" = date_names,
      "posixct_vars" = posixct_names,
      "posixlt_vars" = posixlt_names,
      "posixt_vars" = posix_names,
      "list_vars" = list_names)
      # reduce
      vars_list <- purrr::compact(all_vars_list)

  return(vars_list)

}

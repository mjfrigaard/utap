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

  df_cols <- switch(type,
    log = dplyr::select(tibble::as_tibble(df),
                        dplyr::where(is.logical)),
    int = dplyr::select(tibble::as_tibble(df),
                        dplyr::where(is.integer)),
    chr = dplyr::select(tibble::as_tibble(df),
                        dplyr::where(is.character)),
    fct = dplyr::select(tibble::as_tibble(df),
                        dplyr::where(is.factor)),
    list = dplyr::select(tibble::as_tibble(df),
                        dplyr::where(is.list)))

  if (ncol(df_cols) < 1 || nrow(df_cols) < 1 ) {
    df_cols <- structure(list(),
                         class = c("tbl_df", "tbl", "data.frame"),
                         row.names = integer(0),
                         names = character(0))
    cli::cli_alert_info("No columns of that type...")
    return(df_cols)
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
  dbls <- get_col_types(df = df, type = 'dbl')
  ints <- get_col_types(df = df, type = 'int')
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
  chrs <- get_col_types(df = df, type = 'chr')
  fcts <- get_col_types(df = df, type = 'fct')
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
  facets <- get_col_types(df = df, type = 'facet')
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
  bins <- get_col_types(df = df, type = 'binary')
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
#'
#' @examples
#' x <- deconstruct(names(mtcars), return = TRUE)
#' x
#' deconstruct(names(mtcars))
deconstruct <- function(x, return = FALSE, quote = TRUE) {
  raw_obj <- capture.output(dput(x, control = "all"))
  if (isFALSE(quote)) {
    obj_noquote <- gsub(pattern = '"', replacement = "", x = raw_obj)
    decon_noquote <- paste0(obj_noquote, collapse = "")
    decon_obj <- gsub("\\s+"," ", decon_noquote)
  } else {
    obj_quote <- gsub(pattern = '"', replacement = "'", x = raw_obj)
    decon_quote <- paste0(obj_quote, collapse = "")
    decon_obj <- gsub("\\s+"," ", decon_quote)
  }
  if (isFALSE(return)) {
    base::cat(decon_obj)
  } else {
    return(noquote(decon_obj))
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
#' require(palmerpenguins)
#' make_ui_inputs(palmerpenguins::penguins)
make_ui_inputs <- function(app_data) {
  ui_inputs <- names(col_type_list(df = app_data))
  lhs_out <- deconstruct(x = ui_inputs, return = TRUE, quote = FALSE)
  zeallot_operator <- deconstruct(x = "%<-%", return = TRUE, quote = FALSE)
  rhs_out <- deconstruct(x = col_type_list(df = app_data), return = TRUE)
  cat(lhs_out, zeallot_operator, rhs_out)
}


#' Get data variable types (as list)
#'
#' @param df a data frame or tibble
#'
#' @return list of vectors by type:
#'  * `logical_vars`
#'  * `double_vars`
#'  * `integer_vars`
#'  * `character_vars`
#'  * `factor_vars`
#'  * `date_vars`
#'  * `posixct_vars`
#'  * `posixlt_vars`
#'  * `posixt_vars`
#'  * `list_vars`
#'
#' @export col_type_list
#'
#' @importFrom dplyr select where
#' @importFrom lubridate is.Date is.POSIXct is.POSIXlt is.POSIXt
#' @importFrom purrr compact
#'
#' @examples
#' require(palmerpenguins)
#' col_type_list(palmerpenguins::penguins)
#' # great with zealott!
#' require(zeallot)
#' c(dbl_vars, int_vars,
#'   fct_vars) %<-% col_type_list(penguins)
#' dbl_vars
#' int_vars
#' fct_vars
col_type_list <- function(df) {
  # atomic
    log_vars <- names(dplyr::select(df,
                    dplyr::where(is.logical)))
    dbl_vars <- names(dplyr::select(df,
                    dplyr::where(is.double)))
    int_vars <- names(dplyr::select(df,
                    dplyr::where(is.integer)))
    chr_vars <- names(dplyr::select(df,
                    dplyr::where(is.character)))

    # s3
    fct_vars <- names(dplyr::select(df,
                    dplyr::where(is.factor)))
    date_vars <- names(dplyr::select(df,
                    dplyr::where(lubridate::is.Date)))
    posixct_vars <- names(dplyr::select(df,
                    dplyr::where(lubridate::is.POSIXct)))
    posixlt_vars <- names(dplyr::select(df,
                    dplyr::where(lubridate::is.POSIXlt)))
    posix_vars <- names(dplyr::select(df,
                    dplyr::where(lubridate::is.POSIXt)))
    list_vars <- names(dplyr::select(df,
                    dplyr::where(is.list)))
    # assemble
    all_vars_list <- list(
      "logical_vars" = log_vars,
      "double_vars" = dbl_vars,
      "integer_vars" = int_vars,
      "character_vars" = chr_vars,
      "factor_vars" = fct_vars,
      "date_vars" = date_vars,
      "posixct_vars" = posixct_vars,
      "posixlt_vars" = posixlt_vars,
      "posixt_vars" = posix_vars,
      "list_vars" = list_vars)
      # reduce
      types <- purrr::compact(all_vars_list)
  return(types)

}

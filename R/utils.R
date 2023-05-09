#' Inverted versions of `%in%`
#'
#' @export
#'
#' @examples
#' 1 %nin% 1:10
#' "A" %nin% 1:10
`%nin%` <- function(x, table) {
  match(x, table, nomatch = 0) == 0
}

#' Inverted versions of is.null
#'
#' @export
#'
#' @examples
#' not_null(c(NULL, NA_character_))
not_null <- Negate(is.null)

#' Inverted versions of is.na
#'
#' @export
#'
#' @examples
#' not_na(c(NA_character_, "A"))
not_na <- Negate(is.na)

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
  dbls <- make_col_types_vec(df = df, type = 'dbl')
  ints <- make_col_types_vec(df = df, type = 'int')
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
  chrs <- make_col_types_vec(df = df, type = 'chr')
  fcts <- make_col_types_vec(df = df, type = 'fct')
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
  facets <- make_col_types_vec(df = df, type = 'facet')
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
  bins <- make_col_types_vec(df = df, type = 'binary')
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

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
  check_log_binary <- function(x) {
    all(na.omit(x) %in% TRUE:FALSE)
  }
  check_int_binary <- function(x) {
    all(na.omit(x) %in% 0:1)
  }
  check_chr_binary <- function(x) {
    length(unique(na.omit(x))) == 2
  }
  check_fct_binary <- function(x) {
    length(levels(na.omit(x))) == 2
  }
  switch(type,
    log = check_log_binary(x),
    int = check_int_binary(x),
    chr = check_chr_binary(x),
    fct = check_fct_binary(x)
  )
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
  check_chr_facet <- function(x) {
    length(unique(na.omit(x))) <= 5
  }
  check_fct_facet <- function(x) {
    length(unique(na.omit(x))) <= 5
  }
  switch(type,
    chr = check_chr_facet(x),
    ord = check_fct_facet(x),
    fct = check_fct_facet(x)
  )
}

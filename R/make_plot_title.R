#' Make plot title
#'
#' @param x x variable
#' @param y y variable
#' @param color color variable
#'
#' @return String for plot title
#' @export make_plot_title
#'
#' @importFrom glue glue
#' @importFrom stringr str_replace_all
#' @importFrom snakecase to_title_case
#'
#' @examples
#' make_plot_title(
#'   x = "imdb_rating",
#'   y = "audience_score",
#'   color = "mpaa_rating"
#' )
make_plot_title <- function(x, y, color) {
  x_chr <- stringr::str_replace_all(
    snakecase::to_title_case(x), "_", " "
  )
  y_chr <- stringr::str_replace_all(
    snakecase::to_title_case(y), "_", " "
  )
  color_chr <- stringr::str_replace_all(
    snakecase::to_title_case(color), "_", " "
  )
  glue::glue("{x_chr} vs. {y_chr} by {color_chr}")
}

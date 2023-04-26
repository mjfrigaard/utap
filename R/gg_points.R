#' Point plot (ggplot2)
#'
#' @param df input dataset (tibble or data.frame)
#' @param x_var x variable
#' @param y_var y variable
#' @param col_var color variable
#' @param ... other arguments passed to `geom_point()`, outside of `aes()`
#'
#' @return plot object
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_point
#'
#' @examples
#' require(testPkgApp)
#' movies <- testPkgApp::movies
#' gg_points(
#'   df = movies, x_var = "critics_score", y_var = "imdb_rating",
#'   col_var = "critics_rating", alpha = 1 / 3, size = 2
#' )
gg_points <- function(df, x_var, y_var, col_var, ...) {

  base <- gg_base(
    df = df,
    x_var = x_var,
    y_var = y_var
  )

  base +
    ggplot2::geom_point(
      ggplot2::aes(color = .data[[col_var]]), ...
    )
}

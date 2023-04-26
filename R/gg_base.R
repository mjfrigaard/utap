#' Base plot (ggplot2)
#'
#' @param df input dataset (tibble or data.frame)
#' @param x_var x variable
#' @param y_var y variable
#'
#' @return plot object
#' @export gg_base
#'
#' @importFrom ggplot2 ggplot aes
#'
#' @examples
#' require(testPkgApp)
#' movies <- testPkgApp::movies
#' gg_base(df = movies, x_var = "critics_score", y_var = "imdb_rating")
gg_base <- function(df, x_var, y_var) {
  ggplot2::ggplot(data = df,
    mapping = ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]]))
}

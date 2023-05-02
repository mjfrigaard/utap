# remotes::install_github("rstudio/chromote", force = TRUE)
library(chromote)
Sys.setenv(CHROMOTE_CHROME = "/Applications/Chromium.app/Contents/MacOS/Chromium")
chromote::find_chrome()
b <- ChromoteSession$new()
b$view()
rm(b)

library(shinytest2)
# shinytest2::record_test()


# gg_points <- function(df, x_var, y_var, col_var, ...) {
#     ggplot2::ggplot(data = df,
#       ggplot2::aes(x = .data[[x_var]],
#           y = .data[[y_var]],
#           color = .data[[col_var]])) +
#       ggplot2::geom_point(...)
# }

#' @examples
#' require(testPkgApp)
#' movies <- testPkgApp::movies
#' gg_points(df = movies,
#'   x_var = "critics_score",
#'   y_var = "imdb_rating",
#'   col_var = "critics_rating",
#'   alpha = 1/3,
#'   size = 2)
#'


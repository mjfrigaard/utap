# remotes::install_github("rstudio/chromote", force = TRUE)
library(chromote)
Sys.setenv(CHROMOTE_CHROME = "/Applications/Chromium.app/Contents/MacOS/Chromium")
chromote::find_chrome()
b <- ChromoteSession$new()
b$view()
rm(b)

library(shinytest2)
# shinytest2::record_test()

# test_that("{shinytest2} recording: test-run-01", {
#   app <- AppDriver$new(name = "test-run-01", height = 556, width = 937)
#   app$set_inputs(`vars-y` = "critics_score")
#   app$set_inputs(`vars-x` = "audience_score")
#   app$set_inputs(`vars-alpha` = 0.3)
#   app$set_inputs(`vars-alpha` = 0.2)
#   app$set_inputs(`vars-size` = 3)
#   app$set_inputs(`vars-plot_title` = "New")
#   app$set_inputs(`vars-plot_title` = "New Title")
#   app$expect_values()
# })


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

# this also works,
# gg_points <- function(df, x_var, y_var, col_var, ...) {
#
#   ggplot2::ggplot(data = df,
#     mapping = ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]])) +
#
#     ggplot2::geom_point(ggplot2::aes(color = .data[[col_var]]), ...)
#
# }

 # list("x" = "imdb_rating",
 #      "y" = "audience_score",
 #      "z" = "mpaa_rating",
 #      "alpha" = 0.5,
 #      "size" = 2,
 #      "plot_title" = make_plot_title(x = "imdb_rating", y = "audience_score",
 #                                    color = "mpaa_rating")
 #   )

library(palmerpenguins)
# devtools::install_github("nteetor/zeallot")
library(zeallot)
library(dplyr)
library(lubridate)
library(purrr)
movies <- testPkgApp::movies
penguins <- palmerpenguins::penguins
df_test <- tibble::tibble(
  a = sample(c(TRUE, FALSE), 10, TRUE),
  b = c(1.5:10.5),
  c = c(1:10),
  d1 = rep(LETTERS[1:5], times = 2),
  d2 = LETTERS[1:10],
  e = sample(c(
    Sys.Date(), Sys.Date() + 1,
    Sys.Date() + 2, Sys.Date() + 3,
    Sys.Date() + 4
  ), size = 10, replace = TRUE),
  f1 = factor(rep(letters[1:5], times = 2),
    levels = letters[1:5]
  ),
  f2 = factor(letters[1:10],
    levels = letters[1:10]
  )
)



deconstruct(names(mtcars), quotes = FALSE)
deconstruct(names(mtcars), quotes = FALSE, console = FALSE)
deconstruct(x = names(mtcars), quotes = TRUE, console = TRUE)

get_var_types(df = palmerpenguins::penguins)


get_ui_inputs(dplyr::starwars)

c(is_double, is_integer, is_character, is_facet_var, is_list) %<-% list(
  is_double = c('mass', 'birth_year'),
  is_integer = 'height',
  is_character = c(
    'name',
    'hair_color',
    'skin_color',
    'eye_color',
    'sex',
    'gender',
    'homeworld',
    'species'
  ),
  is_facet_var = c('sex', 'gender'),
  is_list = c('films', 'vehicles', 'starships')
)


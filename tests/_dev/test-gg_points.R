library(vdiffr)
testthat::test_that("gg_points works", {
  # define inputs
  set.seed(1234)
  df_test <- data.frame(
    "x" = rnorm(n = 100, mean = 5, sd = 2),
    "y" = rnorm(n = 100, mean = 10, sd = 5),
    "col" = sample(LETTERS[1:3], size = 100, replace = TRUE)
  )
  # create points
  points <- gg_points(
    df = df_test,
    x_var = "x",
    y_var = "y",
    col_var = "col"
  )
  vdiffr::expect_doppelganger("default points plot", points)
})
# testthat::test_that("gg_points check data, geom, labels, class, and attributes", {
#       x <- "imdb_num_votes"
#       y <- "imdb_rating"
#       color <- "genre"
#       movies_test <- dplyr::slice_sample(movies, n = 100, replace = FALSE)
#       ggp_pnts <- gg_points(
#         df = movies_test,
#         x_var = x,
#         y_var = y,
#         col_var = color,
#         alpha = 1 / 3,
#         size = 2
#       )
#       # labels
#       expect_equal(
#         object = ggp_pnts$labels,
#         expected = list(
#           x = "imdb_num_votes",
#           y = "imdb_rating",
#           colour = "genre"
#         )
#       )
#     # class
#     expect_equal(
#       object = class(ggp_pnts),
#       expected = c("gg", "ggplot")
#     )
#     # coordinate attributes
#     expect_equal(
#       object = attributes(ggp_pnts$coordinates),
#       expected = list(class = c(
#         "CoordCartesian", "Coord", "ggproto", "gg"
#       ))
#     )
#     # geom
#     expect_equal(
#       object = purrr::map_vec(ggp_pnts$layers, \(x) class(x$geom)[1]),
#       expected = "GeomPoint"
#     )
#     # re-define inputs
#       set.seed(1234)
#       df_test <- data.frame(
#       'x' = rnorm(n = 100, mean = 5, sd = 2),
#       'y' = rnorm(n = 100, mean = 10, sd = 5),
#       'z' = rep(sample(LETTERS, size = 5, replace = FALSE),
#                 each = 5,
#                 times = 4))
#       ggp_pnts <- gg_points(
#         df = df_test,
#         x_var = "x",
#         y_var = "y",
#         col_var = "z",
#         alpha = 1 / 3,
#         size = 2
#       )
#       # re-check geoms
#       expect_equal(
#         object = purrr::map_vec(ggp_pnts$layers, \(x) class(x$geom)[1]),
#         expected = "GeomPoint")
# })

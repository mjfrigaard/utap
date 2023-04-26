testthat::test_that("gg_points works", {
  x <- 'imdb_num_votes'
  y <- 'imdb_rating'
  color <- 'genre'
  ggp_pnts <- gg_points(df = movies,
    x_var = x,
    y_var = y,
    col_var = color,
    alpha = 1/3,
    size = 2)
  # class
  expect_equal(object = class(ggp_pnts),
    expected = c("gg", "ggplot"))
  # geom
  expect_equal(
    object = purrr::map_vec(ggp_pnts$layers, \(x) class(x$geom)[1]),
    expected = "GeomPoint")
  # labels
  expect_equal(object = ggp_pnts$labels,
    expected = list(x = "imdb_num_votes", y = "imdb_rating", colour = "genre"))
  # coordinate attributes
  expect_equal(object = attributes(ggp_pnts$coordinates),
    expected = list(class = c(
      "CoordCartesian", "Coord", "ggproto", "gg"
    )))
})

library(testPkgApp)
testthat::test_that("gg_points check geom, labels, class, and attributes", {
  x <- "imdb_num_votes"
  y <- "imdb_rating"
  color <- "genre"
  ggp_pnts <- gg_points(
    df = movies,
    x_var = x,
    y_var = y,
    col_var = color,
    alpha = 1 / 3,
    size = 2
  )
  # geom
  expect_equal(
    object = purrr::map_vec(ggp_pnts$layers, \(x) class(x$geom)[1]),
    expected = "GeomPoint"
  )
  # labels
  expect_equal(
    object = ggp_pnts$labels,
    expected = list(
      x = "imdb_num_votes",
      y = "imdb_rating",
      colour = "genre"
    )
  )
  # class
  expect_equal(
    object = class(ggp_pnts),
    expected = c("gg", "ggplot")
  )
  # coordinate attributes
  expect_equal(
    object = attributes(ggp_pnts$coordinates),
    expected = list(class = c(
      "CoordCartesian", "Coord", "ggproto", "gg"
    ))
  )
})
testthat::test_that("gg_points ggplot2movies geom, labels, class, and attributes", {
  x <- "budget"
  y <- "rating"
  color <- "mpaa"
  ggp2movies_pnts <- gg_points(
    df = ggplot2movies::movies,
    x_var = "budget",
    y_var = "rating",
    col_var = "mpaa",
    alpha = 1 / 3,
    size = 2
  )
  # labels
  expect_equal(
    object = ggp2movies_pnts$labels,
    expected = list(
      x = "budget",
      y = "rating",
      colour = "mpaa"
    )
  )
  # class
  expect_equal(
    object = class(ggp2movies_pnts),
    expected = c("gg", "ggplot")
  )
  # geom
  expect_equal(
    object = purrr::map_vec(ggp2movies_pnts$layers, \(x) class(x$geom)[1]),
    expected = "GeomPoint"
  )
  # coordinate attributes
  expect_equal(
    object = attributes(ggp2movies_pnts$coordinates),
    expected = list(class = c(
      "CoordCartesian", "Coord", "ggproto", "gg"
    ))
  )
})

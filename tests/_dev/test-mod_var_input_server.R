shiny::testServer(mod_var_input_server, {
  # set inputs
  session$setInputs(
    x = "imdb_rating",
    y = "audience_score",
    z = "mpaa_rating",
    alpha = 0.5,
    size = 2L,
    plot_title = ""
  )
  # check structure
  expect_true(object = is.list(session$returned()))
  # check class
  expect_equal(
    object = class(session$returned()),
    expected = c("list")
  )
  # check type
  expect_equal(
    object = typeof(session$returned()),
    expected = c("list")
  )
  # check names
  expect_equal(
    object = names(session$returned()),
    expected = c("x", "y", "z", "alpha", "size", "plot_title")
  )
  # check single input
  expect_equal(object = session$returned()$x, expected = "imdb_rating")
  # check all inputs
  expect_equal(
    object = session$returned(),
    expected = list(
      x = "imdb_rating",
      y = "audience_score",
      z = "mpaa_rating",
      alpha = 0.5,
      size = 2L,
      plot_title = ""
    )
  )
})

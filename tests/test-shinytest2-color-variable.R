library(shinytest2)
test_that("{shinytest2} verify color variable", {
  app <- AppDriver$new(
    name = "color-variable",
    height = 596, width = 1156
  )
  color_input <- app$get_value(input = "vars-z")
  expect_equal(object = color_input, expected = "mpaa_rating")
})

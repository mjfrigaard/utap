library(shinytest2)

test_that("scatterplot mappings", {
  shiny_app <- testPkgApp::runShinyApp()
  app <- AppDriver$new(shiny_app, height = 596, width = 1156)
  # get output value for scatterplot
  plot_value <- app$get_value(output = "plot-scatterplot")
  # extract mappings object:
  plot_mappings <- plot_value$coordmap$panels[[1]]$mapping
  expect_equal(
    object = plot_mappings,
    expected = list(
      colour = ".data[[\"mpaa_rating\"]]",
           x = ".data[[\"imdb_rating\"]]",
           y = ".data[[\"audience_score\"]]"))
})

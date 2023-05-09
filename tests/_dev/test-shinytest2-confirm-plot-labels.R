library(shinytest2)

test_that("{shinytest2} confirm plot labels", {
  app <- AppDriver$new(name = "confirm-plot-labels", height = 596, width = 1156)
  app$set_inputs(`vars-z` = "genre")
  app$set_inputs(`vars-x` = "audience_score")
  app$set_inputs(`vars-y` = "critics_score")
  scatter_object <- app$get_value(output = "plot-scatterplot")
  plot_mappings <- scatter_object$coordmap$panels[[1]]$mapping
  get_plot_labels <- function(plot_mappings) {
    exp_mpngs <- purrr::map(
      .x = plot_mappings,
      .f = stringr::str_remove_all,
      pattern = "^.data|\\[\\[|\\]\\]|\"|\""
    )
    expected_labels <- list(
      x = stringr::str_replace_all(
        tools::toTitleCase(exp_mpngs$x), "_", " "
      ),
      y = stringr::str_replace_all(
        tools::toTitleCase(exp_mpngs$y), "_", " "
      ),
      colour = exp_mpngs$colour
    )
    return(expected_labels)
  }
  expected_labels <- get_plot_labels(plot_mappings)
  exported_app_graph <- app$get_value(export = "plot-app_graph")
  plot_labels <- exported_app_graph$labels[c("x", "y", "colour")]
  expect_equal(object = plot_labels, expected = expected_labels)
})

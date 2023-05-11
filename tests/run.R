library(testthat)
library(testPkgApp)
# run utility function tests ----------------------------------------------

testthat::test_file(path = "tests/testthat/test-binary_app_inputs.R")
testthat::test_file(path = "tests/testthat/test-get_col_types.R")
testthat::test_file(path = "tests/testthat/test-check_binary_vec.R")
testthat::test_file(path = "tests/testthat/test-check_facet_vec.R")
testthat::test_file(path = "tests/testthat/test-make_binary_vec.R")
testthat::test_file(path = "tests/testthat/test-make_facet_vec.R")

# run plot function tests ----------------------------------------------
testthat::test_file("tests/testthat/test-gg_base.R")
testthat::test_file("tests/testthat/test-gg_points.R")
testthat::test_file("tests/testthat/test-gg_points-attr.R")


# run module function tests -----------------------------------------------
# testthat::test_file("tests/testthat/test-mod_var_input_server.R")
# testthat::test_file("tests/testthat/test-mod_display_plot_server.R")


library(shinytest2)
# remotes::install_github("rstudio/chromote", force = TRUE, quiet = TRUE)
library(chromote)
# Chromium checks ---------------------------------------------------------
Sys.setenv(CHROMOTE_CHROME = "/Applications/Chromium.app/Contents/MacOS/Chromium")
chromote::find_chrome()
# verify chromote
# b <- ChromoteSession$new()
# b$view()
# rm(b)



# run shinytest2 recording -----------------------------------------------
record_test()
## in console ------
# app <- AppDriver$new(name = "test-run-01", height = 596, width = 1156)
# app$view()
# app$set_inputs(`vars-y` = "critics_score")
# app$set_inputs(`vars-x` = "audience_score")
# app$get_values()
# app$get_value(input = "vars-z")
# app$stop()
testthat::test_file("tests/testthat/test-shinytest2.R")

# run shinytest2-color-variable -------------------------------------------
testthat::test_file("tests/testthat/test-shinytest2-color-variable.R")
## in console ------
# app <- AppDriver$new(name = "color-variable", height = 596, width = 1156)
# app$view()
# app$get_values()
# color_input <- app$get_value(input = "vars-z")
# color_input
# expect_equal(object = color_input, expected = "mpaa_rating")



# run confirm-plot-labels.R ---------------------------------------------
testthat::test_file("tests/testthat/test-shinytest2-confirm-plot-labels.R")
## in console ------
# app <- AppDriver$new(name = "confirm-plot-labels", height = 596, width = 1156)
# # confirm
# app$view()
# app$get_value(export = "plot-app_data")
# app$get_value(export = "plot-app_graph")
# # set values
# app$set_inputs(`vars-z` = "genre")
# app$set_inputs(`vars-x` = "audience_score")
# app$set_inputs(`vars-y` = "critics_score")
# # get scatterplot object
# scatter_object <- app$get_value(output = "plot-scatterplot")
# names(scatter_object)
# str(scatter_object)
# # find mappings
# scatter_object$coordmap$panels[[1]]$mapping
# # store plot mappings
# plot_mappings <- scatter_object$coordmap$panels[[1]]$mapping
# # test
# stringr::str_remove_all(
#   plot_mappings[2],
#   pattern = "^.data|\\[\\[|\\]\\]|\"|\"")
# # bundle
# get_plot_labels <- function(plot_mappings) {
#   # clean text
#     exp_mpngs <- purrr::map(.x = plot_mappings,
#                       .f = stringr::str_remove_all,
#                       pattern = "^.data|\\[\\[|\\]\\]|\"|\"")
#
#     # create list of labels (similar to module)
#     expected_labels <- list(
#       x = stringr::str_replace_all(
#                   tools::toTitleCase(exp_mpngs$x), "_", " "),
#       y = stringr::str_replace_all(
#                   tools::toTitleCase(exp_mpngs$y), "_", " "),
#       colour = exp_mpngs$colour)
#
#     return(expected_labels)
# }
# expected_labels <- get_plot_labels(plot_mappings)
# expected_labels
# # export plot
# exported_app_graph <- app$get_value(export = "plot-app_graph")
# names(exported_app_graph)
# exported_app_graph$labels
# # create plot labels
# plot_labels <- exported_app_graph$labels[c('x', 'y', 'colour')]
# plot_labels
# # now write expectation using waldo::compare()
# waldo::compare(
#   x = plot_labels,
#   y = expected_labels)


# run test-runShinyApp.R -------------------------------------------------
# from: https://rstudio.github.io/shinytest2/articles/use-package.html#application-objects-created-by-functions
testthat::test_file("tests/testthat/test-runShinyApp.R")

# create test-scatterplot-mappings.R -----------------------------------------
## in console ------
# shiny_app <- testPkgApp::runShinyApp()
# app <- AppDriver$new(shiny_app, height = 596, width = 1156)
# put all values in list
# all_values <- app$get_values()
# inspect
# str(all_values)
# all_values$output$`plot-scatterplot`$coordmap$panels[[1]]$mapping
# get module output
# plot_value <- app$get_value(output = "plot-scatterplot")
# get mappings
# plot_mappings <- plot_value$coordmap$panels[[1]]$mapping
# plot_mappings
# now write expectation
# expect_equal(
#   object = plot_mappings,
#   expected = list(
#     colour = ".data[[\"mpaa_rating\"]]",
#          x = ".data[[\"imdb_rating\"]]",
#          y = ".data[[\"audience_score\"]]"))
# run test-scatterplot-mappings.R -----------------------------------------
testthat::test_file("tests/testthat/test-scatterplot-mappings.R")

# run all tests -----------------------------------------------------------
shinytest2::test_app()

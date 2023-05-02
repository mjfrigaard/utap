library(testthat)

# for shinytest2
# remotes::install_github("rstudio/chromote", force = TRUE, quiet = TRUE)
library(chromote)
Sys.setenv(CHROMOTE_CHROME = "/Applications/Chromium.app/Contents/MacOS/Chromium")
chromote::find_chrome()
# verify chromote
# b <- ChromoteSession$new()
# b$view()
# rm(b)

# run utility function tests ----------------------------------------------
testthat::test_file("tests/testthat/test-gg_base.R")
testthat::test_file("tests/testthat/test-gg_points.R")

# run module function tests -----------------------------------------------
testthat::test_file("tests/testthat/test-mod_var_input_server.R")
testthat::test_file("tests/testthat/test-mod_display_plot_server.R")

# run shinytest2 tests -----------------------------------------------
testthat::test_file("tests/testthat/test-shinytest2.R")
## in console ------
# app <- AppDriver$new(name = "test-run-01", height = 596, width = 1156)
# app$view()
# app$set_inputs(`vars-y` = "critics_score")
# app$set_inputs(`vars-x` = "audience_score")
# app$get_values()
# app$get_value(input = "vars-z")
# app$stop()

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

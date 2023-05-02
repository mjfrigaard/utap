library(shinytest2)

test_that("{shinytest2} recording: test-run-01", {
  app <- AppDriver$new(name = "test-run-01", height = 596, width = 1156)
  app$set_inputs(`vars-y` = "critics_score")
  app$set_inputs(`vars-x` = "audience_score")
  app$set_inputs(`vars-alpha` = 0.3)
  app$set_inputs(`vars-alpha` = 0.2)
  app$set_inputs(`vars-size` = 3)
  app$set_inputs(`vars-plot_title` = "New ")
  app$set_inputs(`vars-plot_title` = "New Title")
  app$expect_values()
})

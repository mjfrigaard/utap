shiny::testServer(app_module_server, {
  cat("\n Start:  is.null(input$num) = ", is.null(input$num))
  session$setInputs(num = 1)
  cat("\n Check: input$num =", input$num, "\n")
  expect_equal(myreactive(), 2)
})
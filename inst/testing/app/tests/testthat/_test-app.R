shiny::testServer(server, {
  cat("\n Start:  is.null(input$num) = ", is.null(input$num))
  session$setInputs(num = 1)
  cat("\n Set: session$setInputs(num = 1)")
  cat("\n Check: input$num =", input$num)
  cat("\n Run: expect_equal(myreactive(), 2) \n")
  testthat::expect_equal(myreactive(), 2)
})

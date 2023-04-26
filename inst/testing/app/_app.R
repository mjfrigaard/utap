# packages --------------------------------------------------------
library(shiny)
library(testthat)
# UI
ui <- fluidPage(
  numericInput(
    inputId = "num",
    label = "The Doubler",
    value = 5),
  textOutput(
    outputId = "txt")
)
# server
server <- function(input, output, session) {
  myreactive <- reactive({
    input$num * 2
  })
  output$txt <- renderText({
    paste0("I am ", myreactive())
  })
}
shinyApp(ui = ui, server = server)
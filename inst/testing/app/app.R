# packages --------------------------------------------------------
library(shiny)
library(testthat)
# ui module
app_module_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::numericInput(
      inputId = ns("num"),
      label = "The Doubler",
      value = 5
    ),
    shiny::textOutput(
      outputId = ns("txt"))
  )
}
# server module
app_module_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    
    myreactive <- reactive({ input$num * 2 })
    
    output$txt <- renderText({
      paste0("I am ", myreactive())
    })
    
  })
}
# app
doublerApp <- function() {
  shiny::shinyApp(
    ui = shiny::fluidPage(
      app_module_ui("x")
    ), 
    server = function(input, output, session) { 
      app_module_server("x")
    }
  )
}
doublerApp()
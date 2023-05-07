#' Import variables UI module
#'
#' @param id module id
#' @param app_data data for application
#'
#' @return shiny UI module
#' @export mod_var_input_ui
#'
#' @importFrom shiny NS tagList selectInput
#' @importFrom shiny sliderInput textInput
#'
mod_var_input_ui <- function(id, app_data) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::selectInput(
      inputId = ns("x"),
      label = "X-axis:",
      choices = c(
        num_app_inputs(df = app_data)
      ),
      selected = num_app_inputs(df = app_data)[1]
    ),
    shiny::selectInput(
      inputId = ns("y"),
      label = "Y-axis:",
      choices = num_app_inputs(df = app_data),
      selected = num_app_inputs(df = app_data)[2]
    ),
    shiny::selectInput(
      inputId = ns("col"),
      label = "Color by:",
      choices = binary_app_inputs(df = app_data),
      selected = binary_app_inputs(df = app_data)[1]
    ),
    shiny::selectInput(
      inputId = ns("facet"),
      label = "Facet by:",
      choices = facet_app_inputs(df = app_data),
      selected = facet_app_inputs(df = app_data)[1]
    ),
    shiny::sliderInput(
      inputId = ns("alpha"),
      label = "Point opacity:",
      min = 0, max = 1, step = 0.1,
      value = 0.5
    ),
    shiny::sliderInput(
      inputId = ns("size"),
      label = "Point size:",
      min = 0, max = 5,
      step = 0.2,
      value = 2
    ),
    # include these for showing reactive values to include in tests:
    shiny::code("return(shiny::reactive({list()}))"),
    shiny::verbatimTextOutput(ns("vals"))
  )
}

#' Import variables Server module
#'
#' @param id module id
#'
#' @return shiny server module
#' @export mod_var_input_server
#'
#' @importFrom shiny NS moduleServer reactive
mod_var_input_server <- function(id) {

  shiny::moduleServer(id, function(input, output, session) {

    # include these for showing reactive values to include in tests:
    output$vals <- shiny::renderPrint({
      all_vals <- shiny::reactiveValuesToList(x = input,
                                              all.names = TRUE)
      print(all_vals)
    })

    return(
      shiny::reactive({
         list("x" = input$x,
              "y" = input$y,
              "col" = input$col,
              "facet" = input$facet,
              "alpha" = input$alpha,
              "size" = input$size,
              "plot_title" = make_plot_title(x = input$x,
                                             y = input$y,
                                             color = input$col)
           )
        })
    )
  })

}

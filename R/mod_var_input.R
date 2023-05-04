#' Import variables UI module
#'
#' @param id module id
#'
#' @return shiny UI module
#' @export mod_var_input_ui
#'
#' @importFrom shiny NS tagList selectInput
#' @importFrom shiny sliderInput textInput
#'
mod_var_input_ui <- function(id) {
  ns <- shiny::NS(id)
  # get inputs (in console)
  # get_ui_inputs(app_data = palmerpenguins::penguins)
  c(is_double, is_integer,
    is_factor,
    is_facet_var) %<-% list(is_double = c('bill_length_mm', 'bill_depth_mm'),
                        is_integer = c('flipper_length_mm',  'body_mass_g', 'year'),
                        is_factor = c('species', 'island', 'sex'),
                        is_facet_var = c('species', 'island', 'sex'))
  # create numeric vars
  num_vars <- purrr::set_names(c(is_integer, is_double))

  shiny::tagList(
    shiny::selectInput(
      inputId = ns("x"),
      label = "X-axis:",
      choices = c(
        num_vars
      ),
      selected = num_vars[1]
    ),
    shiny::selectInput(
      inputId = ns("y"),
      label = "Y-axis:",
      choices = num_vars,
      selected = num_vars[2]
    ),
    shiny::selectInput(
      inputId = ns("z"),
      label = "Color by:",
      choices = is_facet_var,
      selected = is_facet_var[1]
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
              "z" = input$z,
              "alpha" = input$alpha,
              "size" = input$size,
              "plot_title" = make_plot_title(x = input$x,
                                             y = input$y,
                                             color = input$z)
           )
        })
    )
  })

}

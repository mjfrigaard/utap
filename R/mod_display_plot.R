#' Plot variables UI module
#'
#' @param id module id
#'
#' @return shiny UI module
#' @export mod_display_plot_ui
#'
#' @importFrom shiny NS tagList tags column
#' @importFrom shiny plotOutput verbatimTextOutput
mod_display_plot_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
    shiny::column(
      width = 8,
      shiny::plotOutput(outputId = ns("scatterplot"))
    ),
    # include these for showing reactive values to include in tests:
    shiny::column(
    width = 4,
    shiny::code("names(app_data())"),
    shiny::verbatimTextOutput(ns("data")),
    shiny::code("class(plot())"),
    shiny::verbatimTextOutput(ns("plot"))
    )
    )
  )
}

#' Plot variables Server module
#'
#' @param id module id
#' @param var_inputs inputs from mod_var_input
#'
#' @return shiny server module
#' @export mod_display_plot_server
#'
#' @importFrom shiny NS moduleServer reactive
#' @importFrom shiny renderPlot
#' @importFrom stringr str_replace_all
#' @importFrom ggplot2 labs theme_minimal theme
mod_display_plot_server <- function(id, var_inputs) {

  shiny::moduleServer(id, function(input, output, session) {

    app_data <- shiny::reactive({
      palmerpenguins::penguins
    })

    plot <- shiny::reactive({
      gg_points(
        df = app_data(),
        x_var = var_inputs()$x,
        y_var = var_inputs()$y,
        col_var = var_inputs()$z,
        alpha = var_inputs()$alpha,
        size = var_inputs()$size)
    })

      output$scatterplot <- shiny::renderPlot({
          plot()
        })

    # include these for showing reactive values to include in tests: ----
    output$data <- shiny::renderPrint({
      print(names(app_data()), width = 55, max.levels = NULL)
    })
    output$plot <- shiny::renderPrint({
      class(plot())
    })

    # include for exporting values with shinytest2 ----
    # shiny::exportTestValues(
    #   app_data = app_data(),
    #   plot =  plot()
    # )

    # # safely_export (shinytest2) ----
    # # https://github.com/rstudio/shiny/issues/3768#issuecomment-1398254569
    #   safely_export <- function(r) {
    #     r_quo <- rlang::enquo(r)
    #     rlang::inject({
    #       shiny::reactive({
    #         tryCatch(
    #           !!r_quo,
    #           error = function(e) {
    #             e
    #           }
    #         )
    #       })
    #     })
    #   }
    # # include for safely exporting values with shinytest2 ----
    # shiny::exportTestValues(
    #   app_data = safely_export(app_data()),
    #   plot =  safely_export(plot())
    # )



  })
}

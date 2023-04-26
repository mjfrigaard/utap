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
    shiny::column(
      width = 8,
      shiny::plotOutput(outputId = ns("scatterplot"))
    ),
    shiny::column(
      width = 4,
      shiny::code("names(movies())"),
      shiny::verbatimTextOutput(ns("data")),
      shiny::code("is.list(inputs())"),
      shiny::verbatimTextOutput(ns("inputs")),
      shiny::code("class(plot())"),
      shiny::verbatimTextOutput(ns("plot"))
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
#' @importFrom tools toTitleCase
#' @importFrom shiny renderPlot
#' @importFrom stringr str_replace_all
#' @importFrom ggplot2 labs theme_minimal theme
mod_display_plot_server <- function(id, var_inputs) {
  shiny::moduleServer(id, function(input, output, session) {
    movies <- shiny::reactive({
      testPkgApp::movies
    })

    inputs <- shiny::reactive({
      list(
        x = var_inputs$x(),
        y = var_inputs$y(),
        z = var_inputs$z(),
        alpha = var_inputs$alpha(),
        size = var_inputs$size(),
        plot_title = tools::toTitleCase(var_inputs$plot_title())
      )
    })

    plot <- shiny::reactive({
      points <- gg_points(
        df = movies(),
        x_var = inputs()$x,
        y_var = inputs()$y,
        col_var = inputs()$z,
        alpha = inputs()$alpha,
        size = inputs()$size
      )
      points +
        ggplot2::labs(
          title = inputs()$plot_title,
          x = stringr::str_replace_all(tools::toTitleCase(inputs()$x), "_", " "),
          y = stringr::str_replace_all(tools::toTitleCase(inputs()$y), "_", " ")
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = "bottom")
    })

    output$data <- shiny::renderPrint({
      print(names(movies()), width = 60, max.levels = NULL)
    })

    output$inputs <- shiny::renderPrint({
      is.list(inputs())
    })

    output$plot <- shiny::renderPrint({
      class(plot())
    })

    output$scatterplot <- shiny::renderPlot({
      plot()
    })
  })
}

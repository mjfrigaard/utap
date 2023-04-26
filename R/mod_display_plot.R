#' Plot variables UI module
#'
#' @param id module id
#'
#' @return shiny UI module
#' @export mod_display_plot_ui
#'
#' @importFrom shiny NS tagList tags
#' @importFrom shiny plotOutput verbatimTextOutput
mod_display_plot_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$br(),
    shiny::tags$blockquote(
      shiny::tags$em(
        shiny::tags$h6("The code for this application comes from the ",
        shiny::tags$a("Building web applications with Shiny",
          href = "https://rstudio-education.github.io/shiny-course/"),
                      "tutorial"))),
    shiny::plotOutput(outputId = ns("scatterplot"))
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

    movies <- testPkgApp::movies

    inputs <- shiny::reactive({
      plot_title <- tools::toTitleCase(var_inputs$plot_title())
      list(
        x = var_inputs$x(),
        y = var_inputs$y(),
        z = var_inputs$z(),
        alpha = var_inputs$alpha(),
        size = var_inputs$size(),
        plot_title = plot_title
      )
    })

    output$scatterplot <- shiny::renderPlot({
      plot <- gg_points(
        df = movies,
        x_var = inputs()$x,
        y_var = inputs()$y,
        col_var = inputs()$z,
        alpha = inputs()$alpha,
        size = inputs()$size
      )
      plot +
        ggplot2::labs(
          title = inputs()$plot_title,
          x = stringr::str_replace_all(tools::toTitleCase(inputs()$x), "_", " "),
          y = stringr::str_replace_all(tools::toTitleCase(inputs()$y), "_", " ")
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = "bottom")
    })
  })
}

#' App UI
#'
#' @importFrom shiny fluidPage sidebarLayout sidebarPanel mainPanel
#'
#' @export appUI
appUI <- function() {
  shiny::tagList(
    shiny::fluidPage(
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          # inputs
          mod_var_input_ui(
            id = "vars",
            app_data = palmerpenguins::penguins
          )
        ),
        shiny::mainPanel(
          shiny::tags$br(),
          # outputs
          mod_display_plot_ui(id = "plot"),
          # # include these for showing reactive values to include in tests: ----
          shiny::fluidRow(
            shiny::code("reactive values"),
            shiny::verbatimTextOutput("vals")
          )
        )
      )
    )
  )
}

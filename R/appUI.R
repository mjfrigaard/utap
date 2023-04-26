#' App UI
#'
#' @importFrom shiny fluidPage sidebarLayout sidebarPanel mainPanel
#'
#' @export appUI
appUI <- function() {
  shiny::tagList(
    shiny::fluidPage(
      shiny::sidebarLayout(
        shiny::sidebarPanel(mod_var_input_ui("vars")),
        shiny::mainPanel(mod_display_plot_ui("plot"))
      )
    )
  )
}

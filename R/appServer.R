#' App Server
#'
#' @importFrom shiny fluidPage sidebarLayout sidebarPanel mainPanel
#'
#' @export appServer
appServer <- function(input, output, session) {

  selected_vars <- mod_var_input_server("vars")

  mod_display_plot_server("plot", var_inputs = selected_vars)

}

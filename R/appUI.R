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
          mod_var_input_ui("vars")
        ),
        shiny::mainPanel(
          shiny::tags$br(),
          shiny::tags$blockquote(
            shiny::tags$em(
              shiny::tags$h6(
                "The code for this application comes from the ",
                shiny::tags$a("Building web applications with Shiny",
                  href = "https://rstudio-education.github.io/shiny-course/"
                ),
                "tutorial"
              )
            )
          ),
          mod_display_plot_ui("plot")
        )
      )
    )
  )
}

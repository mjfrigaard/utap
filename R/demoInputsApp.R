#' Demo `*app_inputs()` functions
#'
#' @param data `app_data` can be any dataset (automatically loads with `NHANES::NHANES`)
#'
#' @importFrom skimr skim
#' @importFrom shiny fluidPage sidebarLayout sidebarPanel mainPanel
#' @importFrom shiny selectInput verbatimTextOutput h3
#' @importFrom shiny reactive bindEvent shinyApp
#' @importFrom purrr compact list_c
#' @importFrom dplyr select any_of
#'
#' @return demo shiny application
#' @export demoInputsApp
#'
#' @examples
#' require(utap)
#' # demoInputsApp()
demoInputsApp <- function(data) {
  # data
  app_data <- data
  # ui
  ui <- shiny::fluidPage(
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::selectInput(
          inputId = "x",
          label = "X variable",
          choices = c(
            pull_numeric_cols(df = app_data)
          ),
          selected = pull_numeric_cols(df = app_data)[1]
        ),
        shiny::selectInput(
          inputId = "y",
          label = "Y variable:",
          choices = pull_numeric_cols(df = app_data),
          selected = pull_numeric_cols(df = app_data)[2]
        ),
        shiny::selectInput(
          inputId = "color",
          label = "Color variable:",
          choices = pull_binary_cols(df = app_data),
          selected = pull_binary_cols(df = app_data)[1]
        ),
        shiny::selectInput(
          inputId = "facet",
          label = "Facet variable:",
          choices = pull_facet_cols(df = app_data),
          selected = pull_facet_cols(df = app_data)[1]
        )
      ),
      shiny::mainPanel(
        shiny::h3("Testing app inputs"),
        shiny::br(),
        shiny::verbatimTextOutput(outputId = "selected")
      )
    )
  )
  server <- function(input, output, session) {

    cols <- shiny::reactive({
      # combine in list
      all_cols_list <- list(
        x =  input$x,
        y = input$y,
        color = input$color,
        facet = input$facet)
      # remove any null values
      cols_list <- purrr::compact(all_cols_list)
      # convert to vector
      cols <- purrr::list_c(cols_list)
      return(cols)

    })

    output$selected <- shiny::renderPrint({
        selected_df <- dplyr::select(
          app_data,
          dplyr::any_of( cols() ))
          # view with skimr
          skimr::skim(selected_df)
        }) |>
              shiny::bindEvent(c(input$x, input$y,
                               input$color, input$facet))
  }
  # run
  shiny::shinyApp(ui = ui, server = server)
}

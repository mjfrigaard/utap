#' Import variables UI module
#'
#' @param id module id
#'
#' @return shiny UI module
#' @export mod_var_input_ui
#'
#' @importFrom shiny NS tagList selectInput
#' @importFrom shiny sliderInput textInput
mod_var_input_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::selectInput(
      inputId = ns("y"),
      label = "Y-axis:",
      choices = c(
        "IMDB rating" = "imdb_rating",
        "IMDB number of votes" = "imdb_num_votes",
        "Critics Score" = "critics_score",
        "Audience Score" = "audience_score",
        "Runtime" = "runtime"
      ),
      selected = "audience_score"
    ),
    shiny::selectInput(
      inputId = ns("x"),
      label = "X-axis:",
      choices = c(
        "IMDB rating" = "imdb_rating",
        "IMDB number of votes" = "imdb_num_votes",
        "Critics Score" = "critics_score",
        "Audience Score" = "audience_score",
        "Runtime" = "runtime"
      ),
      selected = "imdb_rating"
    ),
    shiny::selectInput(
      inputId = ns("z"),
      label = "Color by:",
      choices = c(
        "Title Type" = "title_type",
        "Genre" = "genre",
        "MPAA Rating" = "mpaa_rating",
        "Critics Rating" = "critics_rating",
        "Audience Rating" = "audience_rating"
      ),
      selected = "mpaa_rating"
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
      value = 2
    ),
    shiny::textInput(
      inputId = ns("plot_title"),
      label = "Plot title",
      placeholder = "Enter plot title"
    ),
    shiny::code("return(list())"),
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

    var_inputs <- shiny::reactive({
         list("x" = input$x,
              "y" = input$y,
              "z" = input$z,
              "alpha" = input$alpha,
              "size" = input$size,
              "plot_title" = input$plot_title
           )
        })

    output$vals <- shiny::renderPrint({
      all_vals <- reactiveValuesToList(x = input, all.names = TRUE)
      print(all_vals)
      print(var_inputs())
    })

    return(
      shiny::reactive({
         list("x" = input$x,
              "y" = input$y,
              "z" = input$z,
              "alpha" = input$alpha,
              "size" = input$size,
              "plot_title" = input$plot_title
           )
        })
    )
  })

}

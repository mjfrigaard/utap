#' Point plot with facets (ggplot2)
#'
#' @param df input dataset (tibble or data.frame)
#' @param x_var x variable (supplied to `ggplot2::aes(x = )`)
#' @param y_var y variable (supplied to `ggplot2::aes(y = )`)
#' @param col_var color variable (supplied to `ggplot2::geom_point(ggplot2::aes(color = ))`)
#' @param facet_var facet variable (supplied to `ggplot2::geom_point(ggplot2::aes(color = ))`)
#' @param ... other arguments passed to (`ggplot2::facet_wrap(vars())`)
#'
#' @return A `ggplot2` plot object
#' @export gg_points_facet
#'
#' @importFrom ggplot2 ggplot aes vars facet_wrap geom_point labs
#' @importFrom rlang .data
#'
#' @examples
#' require(palmerpenguins)
#' gg_points_facet(
#'   df = palmerpenguins::penguins,
#'   x_var = "bill_length_mm",
#'   y_var = "flipper_length_mm",
#'   col_var = "island",
#'   facet_var = "species",
#'   alpha = 1 / 3,
#'   size = 2
#' )
#' gg_points_facet(
#'   df = palmerpenguins::penguins,
#'   x_var = "bill_length_mm",
#'   y_var = "flipper_length_mm",
#'   col_var = "island",
#'   facet_var = NULL,
#'   alpha = 1 / 3,
#'   size = 2
#' )
#' gg_points_facet(
#'   df = palmerpenguins::penguins,
#'   x_var = "bill_length_mm",
#'   y_var = "flipper_length_mm",
#'   col_var = NULL,
#'   facet_var = NULL,
#'   alpha = 1 / 3,
#'   size = 2
#' )
gg_points_facet <- function(df, x_var, y_var, col_var, facet_var, ...) {
  base <- ggplot2::ggplot(
    data = df,
    mapping = ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]])
  )

  if (is.null(facet_var)) {
    facet_layer <- NULL
  } else {
    facet_layer <- ggplot2::facet_wrap(ggplot2::vars(.data[[facet_var]]))
  }
  base +
    # points layer
    ggplot2::geom_point(ggplot2::aes(colour = .data[[col_var]]), ...) +
    # facet layer
    facet_layer +
    # labels
    ggplot2::labs(
      title = make_plot_title(x = x_var, y = y_var, color = col_var),
      x = stringr::str_replace_all(
        snakecase::to_title_case(x_var), "_", " "
      ),
      y = stringr::str_replace_all(
        snakecase::to_title_case(y_var), "_", " "
      ),
      color = stringr::str_replace_all(
        snakecase::to_title_case(col_var), "_", " "
      )
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")
}

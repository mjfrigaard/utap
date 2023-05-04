test_that("mod_display_plot_ui works", {
  ui <- mod_display_plot_ui(id = "test")
  testthat::expect_type(object = ui, type = "list")
  testthat::expect_true(is(ui, "list"))
  testthat::expect_true(is(ui, "shiny.tag.list"))
})

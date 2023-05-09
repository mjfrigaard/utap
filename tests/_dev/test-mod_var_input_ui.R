test_that("mod var input ui works", {
  ui <- mod_var_input_ui("test")
  # check list
  testthat::expect_type(object = ui, type = "list")
  testthat::expect_true(is(object = ui, "list"))
  testthat::expect_true(is(object = ui, "shiny.tag.list"))
})

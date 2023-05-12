test_that("make_ui_inputs() works", {
  # Define a helper function for creating a test data frame
  create_test_data <- function() {
    data.frame(
      col_a = c(1, 2, 3),
      col_b = c("A", "B", "C"),
      col_c = as.factor(c("X", "Y", "Z"))
    )
  }
  test_data <- create_test_data()

  # Set up a capture of the output printed by cat()
  cat_output <- capture.output(output = make_ui_inputs(test_data))

  # Define the expected output
  expected_output <- "c(double_vars, character_vars, factor_vars) %<-% list(double_vars = 'col_a', character_vars = 'col_b', factor_vars = 'col_c')"

  # Test if the output is as expected
  expect_equal(cat_output, expected_output)

  # Create an empty data frame
  empty_data <- data.frame()

  # Set up a capture of the output printed by cat()
  cat_output <- capture.output(output = make_ui_inputs(empty_data))

  # Test if the output is empty
  expect_equal(cat_output, "character(0) %<-% structure(list(), names = character(0))")
})

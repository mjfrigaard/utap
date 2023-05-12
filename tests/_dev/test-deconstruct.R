# Test cases
test_that("deconstruct returns expected output", {
  # Test with quotes = TRUE and console = FALSE
  test_string <- names(mtcars)
  x <- deconstruct(test_string, return = TRUE)

  expect_equal(
    object = deconstruct(test_string, return = TRUE),
    expected = noquote("c('mpg', 'cyl', 'disp', 'hp', 'drat', 'wt', 'qsec', 'vs', 'am', 'gear', 'carb')")
  )

  expect_s3_class(object = x, class = "noquote")
})

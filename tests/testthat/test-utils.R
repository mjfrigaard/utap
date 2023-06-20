testthat::test_that("%nin% works", {
  testthat::expect_false(
    object = "A" %nin% LETTERS)
  testthat::expect_false(
    object = 1 %nin% 1:10)
  testthat::expect_true(
    object = 1 %nin% 2:10)
})

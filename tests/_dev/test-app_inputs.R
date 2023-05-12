test_that("num_app_inputs returns expected output", {
  # Create a sample data frame
  app_input_test <- tibble::tibble(
    a = c(1, 2, 3, 4, 5, 6),
    b = c("A", "B", "C", "D", "E", "F"),
    c = c(TRUE, TRUE, FALSE, FALSE, TRUE, TRUE),
    d = as.Date(c(
      "2023-01-01",
      "2023-01-02",
      "2023-01-03",
      "2023-01-04",
      "2023-01-05",
      "2023-01-06"
    )),
    e = as.POSIXct(c(
      "2023-01-01",
      "2023-01-02",
      "2023-01-03",
      "2023-01-04",
      "2023-01-05",
      "2023-01-06"
    )),
    # test binary
    f = factor(
      sample(
        x = c(
          "high", "low",
          "high", "low",
          NA_character_
        ),
        size = 6, replace = TRUE
      ),
      levels = c("low", "high"),
      ordered = TRUE
    ),
    # test facet
    g = factor(
      sample(
        x = c(
          "very low", "low", "medium",
          "high", "very high", NA_character_
        ),
        size = 6, replace = TRUE
      ),
      levels = c(
        "very low", "low", "medium",
        "high", "very high"
      ),
      ordered = TRUE
    )
  )
  # Test num_app_inputs
  expect_equal(
    object = num_app_inputs(app_input_test),
    expected = c(a = "a", d = "d", e = "e")
  )
  # cat_app_inputs test cases
  expect_equal(
    object = cat_app_inputs(app_input_test),
    expected = c(b = "b", f = "f", g = "g")
  )
  # facet_app_inputs test cases
  expect_equal(
    object = facet_app_inputs(app_input_test),
    expected = c(f = "f", g = "g")
  )
})

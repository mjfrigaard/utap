# Test cases
testthat::test_that("col_type returns expected output", {
  # Create a sample data frame
  col_type_test <- tibble::tibble(
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

  # Test
  testthat::expect_equal(
    object = col_type(col_type_test, "dbl"),
    expected = c(a = "a", d = "d", e = "e")
  )

  testthat::expect_equal(
    object = col_type(col_type_test, "chr"),
    expected = c("b" = "b")
  )

  testthat::expect_equal(
    object = col_type(col_type_test, "log"),
    expected = c("c" = "c")
  )

  testthat::expect_equal(
    object = col_type(col_type_test, "date"),
    expected = c("d" = "d")
  )

  testthat::expect_equal(
    object = col_type(col_type_test, "posixct"),
    expected = c("e" = "e")
  )

  testthat::expect_equal(
    object = col_type(col_type_test, "binary"),
    expected = c(
      "c" = "c",
      "f" = "f"
    )
  )

  testthat::expect_equal(
    object = col_type(col_type_test, "facet"),
    expected = c(f = "f", g = "g")
  )
})

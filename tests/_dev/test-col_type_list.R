# Test cases
testthat::test_that("col_type_list returns expected output", {
  # Create a sample data frame
  col_type_test_01 <- tibble::tibble(
    a = c(1, 2, 3, 4, 5, 6),
    b = c("A", "B", "C", "D", "E", "F"),
    c = c(TRUE, TRUE, FALSE, FALSE, TRUE, TRUE)
  )
  col_type_test_02 <- tibble::tibble(
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
    ))
  )
  col_type_test_03 <- tibble::tibble(
    # test factors
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
    # test factors
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
  # test lists
  col_type_test_04 <- tibble::tibble(
    # single list column
    lst_01 = list(
      # three level factor (with missing)
      fct_03 = factor(
        x = sample(
          c(
            "group 1", "group 2", "group 3",
            NA_character_
          ),
          size = 100, replace = TRUE
        ),
        levels = c("group 1", "group 2", "group 3")
      ),
      # three level factor, with only two levels represented (with missing)
      fct_04 = factor(
        x = sample(c("group x", "group y", NA_character_),
          size = 100, replace = TRUE
        ),
        levels = c(
          "group x", "group y",
          "group z"
        )
      )
    )
  )

  # Test
  testthat::expect_equal(
    object = col_type_list(col_type_test_01),
    expected = list(
      logical_vars = "c",
      double_vars = "a",
      character_vars = "b"
    )
  )
  # Test
  testthat::expect_equal(
    object = col_type_list(col_type_test_02),
    expected = list(
      double_vars = c("d", "e"),
      date_vars = "d", posixct_vars = "e",
      posixt_vars = "e"
    )
  )
  # Test
  testthat::expect_equal(
    object = col_type_list(col_type_test_03),
    expected = list(factor_vars = c("f", "g"))
  )
  # Test
  testthat::expect_equal(
    object = col_type_list(col_type_test_04),
    expected = list(list_vars = "lst_01")
  )
})

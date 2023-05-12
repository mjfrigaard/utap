test_that("facet_vars() works", {
  # define inputs
  set.seed(1234)
  # test with character data ----------------------------
  chr_vars_test <- tibble::tibble(
    # character 5 unique (with missing values)
    chr_05 = sample(
      x = c(LETTERS[1:5], NA_character_),
      size = 100, replace = TRUE
    ),
    # character 6 unique (with missing values)
    chr_06 = sample(c(LETTERS[1:6], NA_character_),
      size = 100, replace = TRUE
    )
  )
  # create object
  facets <- facet_vars(chr_vars_test)
  # test
  expect_equal(
    object = facets,
    expected = c(chr_05 = "chr_05")
  )

  # test with ordered factor data ----------------------------
  ord_vars_test <- tibble::tibble(
    # tests for five level ordered factor (with missing)
    ord_05 = factor(
      sample(
        x = c(
          "very low", "low", "medium",
          "high", "very high", NA_character_
        ),
        size = 100, replace = TRUE
      ),
      levels = c("very low", "low", "medium", "high", "very high"),
      ordered = TRUE
    ),
    # tests for six level ordered factor (with missing)
    ord_06 = factor(
      sample(
        x = c(
          "very low", "low", "medium", "neutral",
          "high", "very high", NA_character_
        ),
        size = 100, replace = TRUE
      ),
      levels = c(
        "very low", "low", "medium",
        "neutral", "high", "very high"
      ),
      ordered = TRUE
    )
  )
  # create object
  facets <- facet_vars(ord_vars_test)
  # test
  expect_equal(
    object = facets,
    expected = c(ord_05 = "ord_05")
  )
  # test with factor data ----------------------------
  fct_vars_test <- tibble::tibble(
    # five level factor (with missing)
    fct_05 = factor(
      sample(
        x = c(
          "group a", "group b", "group c",
          "group d", "group e",
          NA_character_
        ),
        size = 100, replace = TRUE
      ),
      levels = c(
        "group a", "group b", "group c",
        "group d", "group e"
      )
    ),
    # six level factor (with missing)
    fct_06 = factor(
      x = sample(
        c(
          "group a", "group b", "group c",
          "group d", "group e", "group f",
          NA_character_
        ),
        size = 100, replace = TRUE
      ),
      levels = c(
        "group a", "group b", "group c",
        "group d", "group e", "group f"
      )
    ),
    # six level factor, with only five levels represented (with missing)
    fct_07 = factor(
      x = sample(
        c(
          "group a", "group b", "group c",
          "group d", "group e", NA_character_
        ),
        size = 100, replace = TRUE
      ),
      levels = c(
        "group a", "group b", "group c",
        "group d", "group e", "group f"
      )
    )
  )
  # create object
  facets <- facet_vars(fct_vars_test)
  # test
  expect_equal(
    object = facets,
    expected = c(
      fct_05 = "fct_05",
      fct_07 = "fct_07"
    )
  )
})

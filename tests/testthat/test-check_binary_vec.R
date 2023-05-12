test_that("check_binary_vec logical works", {
  # test logical
  expect_equal(
    object = check_binary_vec(
      x = log_maker(
        size = 2,
        missing = TRUE
      ),
      type = "log"
    ),
    expected = TRUE
  )

  expect_equal(
    object = check_binary_vec(
      x = log_maker(size = 2),
      type = "log"
    ),
    expected = TRUE
  )

  expect_equal(
    object = check_binary_vec(x = c(FALSE, FALSE, NA), type = "log"),
    expected = TRUE
  )
  expect_equal(
    object = check_binary_vec(x = c(FALSE, FALSE), type = "log"),
    expected = TRUE
  )
})

test_that("check_binary_vec integer works", {
  # test
  expect_equal(
    object = check_binary_vec(x = c(0L, 1L, NA_integer_), type = "int"),
    expected = TRUE
  )
  expect_equal(
    object = check_binary_vec(x = c(0L, 1L), type = "int"),
    expected = TRUE
  )
  expect_equal(
    object = check_binary_vec(x = c(0L, 0L, NA_integer_), type = "int"),
    expected = TRUE
  )
  expect_equal(
    object = check_binary_vec(x = c(0L, 0L), type = "int"),
    expected = TRUE
  )
  expect_equal(
    object = check_binary_vec(x = c(1L, 2L, NA_integer_), type = "int"),
    expected = FALSE
  )
  expect_equal(
    object = check_binary_vec(x = c(1L, 2L), type = "int"),
    expected = FALSE
  )
})

test_that("check_binary_vec character works", {
  # test characters
  expect_equal(
    object = check_binary_vec(
      x = c("TRUE", "FALSE", NA_character_), type = "chr"
    ),
    expected = TRUE
  )
  expect_equal(
    object = check_binary_vec(
      x = c("TRUE", "FALSE"), type = "chr"
    ),
    expected = TRUE
  )
  expect_equal(
    object = check_binary_vec(
      x = c("TRUE", "FALSE", "MAYBE", NA_character_), type = "chr"
    ),
    expected = FALSE
  )
  expect_equal(
    object = check_binary_vec(
      x = c("TRUE", "FALSE", "MAYBE"), type = "chr"
    ),
    expected = FALSE
  )
  expect_equal(
    object = check_binary_vec(
      x = c("TRUE", "TRUE", NA_character_), type = "chr"
    ),
    expected = FALSE
  )
  expect_equal(
    object = check_binary_vec(
      x = c("TRUE", "TRUE", NA_character_), type = "chr"
    ),
    expected = FALSE
  )
  expect_equal(
    object = check_binary_vec(
      x = c("TRUE", "TRUE"), type = "chr"
    ),
    expected = FALSE
  )
})

test_that("check_binary_vec factor works", {
  # test factors
  expect_equal(
    object = check_binary_vec(
      x =
        factor(c("hot", "cold", NA_character_),
          levels = c("hot", "cold")
        ), type = "fct"
    ),
    expected = TRUE
  )
  expect_equal(
    object = check_binary_vec(
      x =
        factor(c("hot", "cold", NA_character_, "warm"),
          levels = c("hot", "cold", "warm")
        ), type = "fct"
    ),
    expected = FALSE
  )
  expect_equal(
    object = check_binary_vec(
      x =
        factor(c("hot", "cold", "warm"),
          levels = c("hot", "cold")
        ), type = "fct"
    ),
    expected = TRUE
  )
  expect_equal(
    object = check_binary_vec(
      x =
        factor(c("hot", "cold"),
          levels = c("hot", "cold", "warm")
        ), type = "fct"
    ),
    expected = FALSE
  )
  expect_equal(
    object = check_binary_vec(
      x =
        factor(c("hot", "cold"),
          levels = c("hot", "cold")
        ), type = "fct"
    ),
    expected = TRUE
  )
  expect_equal(
    object = check_binary_vec(
      x =
        factor(c("hot", "cold"),
          levels = c("hot", "cold")
        ), type = "fct"
    ),
    expected = TRUE
  )
  expect_equal(
    object = check_binary_vec(
      factor(
        x = c("high", "low", NA_character_),
        levels = c("low", "high"),
        ordered = TRUE
      ),
      type = "fct"
    ),
    expected = TRUE
  )
  expect_equal(
    object = check_binary_vec(
      factor(
        x = c("high", "low"),
        levels = c("low", "high"),
        ordered = TRUE
      ),
      type = "fct"
    ),
    expected = TRUE
  )
})

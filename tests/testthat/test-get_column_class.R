testthat::test_that("get_column_class() works", {
  # test logical class
  testthat::expect_equal(
    object = get_column_class(
      df = col_maker(
        col_type = c("log", "int", "dbl", "chr", "fct", "ord"),
          size = 6,
          missing = FALSE,
          lvls = 4),
      class = "log") |>
      unlist() |>
      is.logical(),
    expected = TRUE
  )
  # test logical names
  testthat::expect_equal(
    object = get_column_class(
      df = col_maker(
            col_type = c("log", "int", "dbl", "chr", "fct", "ord"),
            size = 6,
            missing = FALSE,
            lvls = 4),
     class = "log",
      return_tbl = FALSE
    ),
    expected = c(log = "log")
  )
  # test integer class
  testthat::expect_equal(
    object = get_column_class(
      df = col_maker(
            col_type = c("log", "int", "dbl", "chr", "fct", "ord"),
            size = 6,
            missing = FALSE,
            lvls = 4),
      class = "int") |>
      unlist() |>
      is.integer(),
    expected = TRUE
  )
  # test integer names
  testthat::expect_equal(
    object = get_column_class(
      df = col_maker(
            col_type = c("log", "int", "dbl", "chr", "fct", "ord"),
            size = 6,
            missing = FALSE,
            lvls = 4),
     class = "int",
      return_tbl = FALSE
    ),
    expected = c(int = "int")
  )
  # test double class
  testthat::expect_equal(
    object = get_column_class(
      df = col_maker(
            col_type = c("log", "int", "dbl", "chr", "fct", "ord"),
            size = 6,
            missing = FALSE,
            lvls = 4),
      class = "dbl") |>
      unlist() |>
      is.double(),
    expected = TRUE
  )
  # test double names
  testthat::expect_equal(
    object = get_column_class(
      df = col_maker(
            col_type = c("log", "int", "dbl", "chr", "fct", "ord"),
            size = 6,
            missing = FALSE,
            lvls = 4),
     class = "dbl",
      return_tbl = FALSE
    ),
    expected = c(dbl = "dbl")
  )
})

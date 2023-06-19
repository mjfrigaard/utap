test_that("select_column_class() works", {
  testdata_col_class <- readRDS(test_path("fixtures", "testdata_col_class.rds"))

  testthat::expect_equal(
    object = select_column_class(testdata_col_class, "log") |> is.data.frame(),
    expected = TRUE)
  # check tibble
  testthat::expect_equal(
    object =
      select_column_class(
        df = testdata_col_class,
        class = "log") |>
          tibble::is_tibble(),
    expected = TRUE)
  # check logical
  testthat::expect_equal(
    object =
      select_column_class(
        df = testdata_col_class,
        class = "log") |>
          lapply(is.logical) |> unlist() |> unique(),
    expected = TRUE)
  # check integer
  testthat::expect_equal(
    object =
      select_column_class(
        df = testdata_col_class,
        class = "int") |>
          lapply(is.integer) |> unlist() |> unique(),
    expected = TRUE)
  # check double
  testthat::expect_equal(
    object =
      select_column_class(
        df = testdata_col_class,
        class = "dbl") |>
          lapply(is.double) |> unlist() |> unique(),
    expected = TRUE)
  # check character
  testthat::expect_equal(
    object =
      select_column_class(
        df = testdata_col_class,
        class = "chr") |>
          lapply(is.character) |> unlist() |> unique(),
    expected = TRUE)
  # check factor
  testthat::expect_equal(
    object =
      select_column_class(
        df = testdata_col_class,
        class = "fct") |>
          lapply(is.factor) |> unlist() |> unique(),
    expected = TRUE)
  # check factor (ordered)
  testthat::expect_equal(
    object =
      select_column_class(
        df = testdata_col_class,
        class = "ord") |>
          lapply(is.ordered) |> unlist() |> unique(),
    expected = TRUE)
  # check list
  testthat::expect_equal(
    object =
      select_column_class(
        df = testdata_col_class,
        class = "list") |>
          lapply(is.list) |> unlist() |> unique(),
    expected = TRUE)
})

# test error type
testthat::test_that("get_column_class() type error", {
  testdata_col_class <- readRDS(test_path("fixtures", "testdata_col_class.rds"))
  # test type error
  testthat::expect_error(
    object = select_column_class(testdata_col_class, class = "array"))
})

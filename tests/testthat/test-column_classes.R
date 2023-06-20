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
testthat::test_that("select_column_class() type error", {
  testdata_col_class <- readRDS(test_path("fixtures", "testdata_col_class.rds"))
  # test type error
  testthat::expect_error(
    object = select_column_class(testdata_col_class, class = "array"))
})


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
testthat::test_that("select_column_class() type error", {
  testdata_col_class <- readRDS(test_path("fixtures", "testdata_col_class.rds"))
  # test type error
  testthat::expect_error(
    object = select_column_class(testdata_col_class, class = "array"))
})


testthat::test_that("get_column_class() logical", {
  # test logical class
  log_test <- col_maker(
        col_type = c("log", "int", "dbl", "chr"),
          size = 6,
          missing = FALSE,
          lvls = 4)
  testthat::expect_equal(
    object = get_column_class(
      df = log_test,
      class = "log") |>
      unlist() |>
      is.logical(),
    expected = TRUE
  )
  # test logical names
  testthat::expect_equal(
    object = get_column_class(
      df = log_test,
     class = "log",
      return_tbl = FALSE
    ),
    expected = c(log_var = "log_var")
  )
})

testthat::test_that("get_column_class() integer", {
  int_test <- col_maker(
        col_type = c("log", "int", "dbl", "chr"),
          size = 6,
          missing = FALSE,
          lvls = 4)
  # test integer class
  testthat::expect_equal(
    object = get_column_class(
      df = int_test,
      class = "int") |>
      unlist() |>
      is.integer(),
    expected = TRUE
  )
  # test integer names
  testthat::expect_equal(
    object = get_column_class(
      df = int_test,
     class = "int",
      return_tbl = FALSE
    ),
    expected = c(int_var = "int_var")
  )
})

testthat::test_that("get_column_class() double", {
  int_test <- col_maker(
        col_type = c("log", "int", "dbl", "chr"),
          size = 6,
          missing = FALSE,
          lvls = 4)
  # test integer class
  testthat::expect_equal(
    object = get_column_class(
      df = int_test,
      class = "dbl") |>
      unlist() |>
      is.double(),
    expected = TRUE
  )
  # test integer names
  testthat::expect_equal(
    object = get_column_class(
      df = int_test,
     class = "dbl",
      return_tbl = FALSE
    ),
    expected = c(dbl_var = "dbl_var")
  )
})

testthat::test_that("get_column_class() character", {
  chr_test <- col_maker(
        col_type = c("log", "int", "dbl", "chr"),
          size = 6,
          missing = FALSE,
          lvls = 4)
  # test integer class
  testthat::expect_equal(
    object = get_column_class(
      df = chr_test,
      class = "chr") |>
      unlist() |>
      is.character(),
    expected = TRUE
  )
  # test integer names
  testthat::expect_equal(
    object = get_column_class(
      df = chr_test,
     class = "chr",
      return_tbl = FALSE
    ),
    expected = c(chr_var = "chr_var")
  )
})

testthat::test_that("get_column_class() factor", {
  fct_test <- col_maker(
        col_type = c("int", "chr", "fct"),
          size = 6,
          missing = FALSE,
          lvls = 4)
  # test integer class
  testthat::expect_equal(
    object = get_column_class(
      df = fct_test,
      class = "fct") |>
      unlist() |>
      is.factor(),
    expected = TRUE
  )
  # test integer names
  testthat::expect_equal(
    object = get_column_class(
      df = fct_test,
     class = "fct",
      return_tbl = FALSE
    ),
    expected = c(fct_var = "fct_var")
  )
})

testthat::test_that("get_column_class() ordered", {
  ord_test <- col_maker(
        col_type = c("int", "chr", "fct", "ord"),
          size = 6,
          missing = FALSE,
          lvls = 4)
  # test integer class
  testthat::expect_equal(
    object = get_column_class(
      df = ord_test,
      class = "ord") |>
      dplyr::pull(ord_var) |>
      is.ordered(),
    expected = TRUE
  )
  # test integer names
  testthat::expect_equal(
    object = get_column_class(
      df = ord_test,
     class = "ord",
      return_tbl = FALSE
    ),
    expected = c(ord_var = "ord_var")
  )
})

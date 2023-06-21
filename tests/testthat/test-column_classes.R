test_that("select_column_class() is.tibble/is.data.frame works", {
  # check tibble
  testthat::expect_s3_class(
    object =
      select_column_class(
        df = col_maker(col_type = c("log", "int", "dbl", "chr"),
                                  size = 6, missing = FALSE),
        class = "log"),
    class = c("tbl_df", "tbl", "data.frame"))
})

test_that("select_column_class() class works", {
  # testdata_col_class <- readRDS(testthat::test_path("fixtures",
  #                                                   "testdata_col_class.rds"))
  testdata_col_class <- col_maker(col_type = c("log", "int", "dbl", "chr",
                                               "fct", "ord"),
                                                size = 3,
                                                missing = FALSE)
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
  testdata_col_class <- tibble::tibble(
    list_var = list(fct_maker(size = 3), ord_maker(size = 3)),
    log_var = log_maker(size = 2),
    int_var = int_maker(size = 2),
    dbl_var = dbl_maker(size = 2),
    chr_var = chr_maker(size = 2)
  )
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
  # test type error
  testthat::expect_error(
    object = select_column_class(
      df = col_maker(col_type = c("log", "int",
                                  "dbl", "chr",
                                  "fct", "ord"),
                                  size = 3,
                                  missing = FALSE),
      class = "array"))
})

# test empty table
testthat::test_that("select_column_class() return zero columns", {
  # test class of empty tibble
  testthat::expect_s3_class(
    object = select_column_class(
      df = col_maker(col_type = c("int", "dbl"),
                                  size = 6,
                                  missing = FALSE),
      class = "log"),
    class = c("tbl_df", "tbl", "data.frame"))
  # test rows of empty tibble
  testthat::expect_equal(
    object = ncol(select_column_class(
      df = col_maker(col_type = c("int", "dbl"),
                                  size = 6,
                                  missing = FALSE),
      class = "log")),
    expected = 0L)
})

testthat::test_that("get_column_class() logical", {
  # test logical class
  testthat::expect_equal(
    object = get_column_class(
      df = col_maker(
        col_type = c("log", "int", "dbl", "chr"),
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
        col_type = c("log", "int", "dbl", "chr"),
          size = 6,
          missing = FALSE,
          lvls = 4),
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

testthat::test_that("get_column_class() list", {
  list_test <- tibble::tibble(
    list_var = list(fct_maker(size = 3), ord_maker(size = 3)),
    int = int_maker(size = 2),
    chr = chr_maker(size = 2)
    )
  # test list class
  testthat::expect_equal(
    object = get_column_class(
      df = list_test,
      class = "list") |>
      dplyr::pull(list_var) |>
      is.list(),
    expected = TRUE
  )
  # test integer names
  testthat::expect_equal(
    object = get_column_class(
      df = list_test,
     class = "list",
      return_tbl = FALSE
    ),
    expected = c(list_var = "list_var")
  )
})

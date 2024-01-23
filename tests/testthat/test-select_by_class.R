test_that("select_by_class() is.tibble/is.data.frame works", {
  # check tibble
  expect_s3_class(
    object =
      select_by_class(
        df = col_maker(col_type = c("log", "int", "dbl", "chr"),
                       size = 6, missing = FALSE),
        class = "log"
      ),
    class = c("tbl_df", "tbl", "data.frame")
  )
})

test_that("select_by_class() class works", {
  # testdata_col_class <- readRDS(test_path("fixtures",
  #                                                   "testdata_col_class.rds"))
  testdata_col_class <- col_maker(col_type = c("log", "int", "dbl", "chr",
                                               "fct", "ord"),
                                                size = 3,
                                                missing = FALSE)
  # check logical ----
  expect_equal(
    object =
      select_by_class(df = testdata_col_class,class = "log") |>
          lapply(is.logical) |> unlist() |> unique(),
    expected = TRUE)
  # check integer ----
  expect_equal(
    object =
      select_by_class(df = testdata_col_class, class = "int") |>
          lapply(is.integer) |> unlist() |> unique(),
    expected = TRUE)
  # check double ----
  expect_equal(
    object =
      select_by_class(df = testdata_col_class, class = "dbl") |>
          lapply(is.double) |> unlist() |> unique(),
    expected = TRUE)
  # check character ----
  expect_equal(
    object =
      select_by_class(df = testdata_col_class, class = "chr") |>
          lapply(is.character) |>
          unlist() |>
          unique(),
    expected = TRUE)
  # check factor ----
  expect_equal(
    object =
      select_by_class(df = testdata_col_class, class = "fct") |>
          lapply(is.factor) |> unlist() |> unique(),
    expected = TRUE)
  # check factor (ordered) ----
  expect_equal(
    object =
      select_by_class(df = testdata_col_class, class = "ord") |>
          lapply(is.ordered) |> unlist() |> unique(),
    expected = TRUE)
  # check list ----
  testdata_col_class <- tibble::tibble(
    list_var = list(fct_maker(size = 3),
                    ord_maker(size = 3)),
    log_var = log_maker(size = 2),
    int_var = int_maker(size = 2),
    dbl_var = dbl_maker(size = 2),
    chr_var = chr_maker(size = 2))
  expect_equal(
    object =
      select_by_class(df = testdata_col_class, class = "list") |>
          lapply(is.list) |> unlist() |> unique(),
    expected = TRUE)

})

# test error type ----
test_that("select_by_class() type error", {
  # test type error
  expect_error(
    object = select_by_class(
      df = col_maker(col_type = c("log", "int",
                                  "dbl", "chr",
                                  "fct", "ord"),
                                  size = 3,
                                  missing = FALSE),
      class = "array"))
})

# test empty table ----
test_that("select_by_class() return zero columns", {
  # test class of empty tibble
  expect_s3_class(
    object = select_by_class(
      df = col_maker(col_type = c("int", "dbl"),
                                  size = 6,
                                  missing = FALSE),
      class = "log"),
    class = c("tbl_df", "tbl", "data.frame"))
  # test rows of empty tibble
  expect_equal(
    object = ncol(select_by_class(
      df = col_maker(col_type = c("int", "dbl"),
                                  size = 6,
                                  missing = FALSE),
      class = "log")),
    expected = 0L)
})

test_that("select_by_class() logical", {
  # test logical class ----
  expect_equal(
    object = select_by_class(
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
  ## test logical names ----
  expect_equal(
    object = select_by_class(
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

test_that("select_by_class() integer", {
  int_test <- col_maker(
        col_type = c("log", "int", "dbl", "chr"),
          size = 6,
          missing = FALSE,
          lvls = 4)
  # test integer class ----
  expect_equal(
    object = select_by_class(
      df = int_test,
      class = "int") |>
      unlist() |>
      is.integer(),
    expected = TRUE
  )
  ## test integer names ----
  expect_equal(
    object = select_by_class(
      df = int_test,
     class = "int",
      return_tbl = FALSE
    ),
    expected = c(int_var = "int_var")
  )
})

test_that("select_by_class() double", {
  int_test <- col_maker(
        col_type = c("log", "int", "dbl", "chr"),
          size = 6,
          missing = FALSE,
          lvls = 4)
  # test double class ----
  expect_equal(
    object = select_by_class(
      df = int_test,
      class = "dbl") |>
      unlist() |>
      is.double(),
    expected = TRUE
  )
  ## test double names ----
  expect_equal(
    object = select_by_class(
      df = int_test,
     class = "dbl",
      return_tbl = FALSE
    ),
    expected = c(dbl_var = "dbl_var")
  )
})

test_that("select_by_class() character", {
  chr_test <- col_maker(
        col_type = c("log", "int", "dbl", "chr"),
          size = 6,
          missing = FALSE,
          lvls = 4)
  # test character class ----
  expect_equal(
    object = select_by_class(
      df = chr_test,
      class = "chr") |>
      unlist() |>
      is.character(),
    expected = TRUE
  )
  ## test character names ----
  expect_equal(
    object = select_by_class(
      df = chr_test,
     class = "chr",
      return_tbl = FALSE
    ),
    expected = c(chr_var = "chr_var")
  )
})

test_that("select_by_class() factor", {
  fct_test <- col_maker(
        col_type = c("int", "chr", "fct"),
          size = 6,
          missing = FALSE,
          lvls = 4)
  # test factor class ----
  expect_equal(
    object = select_by_class(
      df = fct_test,
      class = "fct") |>
      unlist() |>
      is.factor(),
    expected = TRUE
  )
  ## test factor names ----
  expect_equal(
    object = select_by_class(
      df = fct_test,
     class = "fct",
      return_tbl = FALSE
    ),
    expected = c(fct_var = "fct_var")
  )
})

test_that("select_by_class() ordered", {
  ord_test <- col_maker(
        col_type = c("int", "chr", "fct", "ord"),
          size = 6,
          missing = FALSE,
          lvls = 4)
  # test ordered class ----
  expect_equal(
    object = select_by_class(
      df = ord_test,
      class = "ord") |>
      dplyr::pull(ord_var) |>
      is.ordered(),
    expected = TRUE
  )
  ## test integer names ----
  expect_equal(
    object = select_by_class(
      df = ord_test,
     class = "ord",
      return_tbl = FALSE
    ),
    expected = c(ord_var = "ord_var")
  )
})

test_that("select_by_class() list", {
  list_test <- tibble::tibble(
    list_var = list(fct_maker(size = 3), ord_maker(size = 3)),
    int = int_maker(size = 2),
    chr = chr_maker(size = 2)
    )
  # test list class ----
  expect_equal(
    object = select_by_class(
      df = list_test,
      class = "list") |>
      dplyr::pull(list_var) |>
      is.list(),
    expected = TRUE
  )
  ## test list names ----
  expect_equal(
    object = select_by_class(
      df = list_test,
     class = "list",
      return_tbl = FALSE
    ),
    expected = c(list_var = "list_var")
  )
})

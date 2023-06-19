testthat::test_that("get_column_class() works", {

  col_types_test <- readRDS(testthat::test_path("fixtures", "col_types_test.rds"))
  # test logical class
  testthat::expect_equal(
    object = class(get_column_class(col_types_test, class = "log")),
    expected = c("tbl_df", "tbl", "data.frame")
  )

  # test logical columns
  testthat::expect_equal(
    object = names(get_column_class(col_types_test, class = "log")),
    expected = c("log_na", "log_var")
  )

  # test logical names
  testthat::expect_equal(
    object = get_column_class(
      df = col_types_test,
     class = "log",
      return_tbl = FALSE
    ),
    expected = purrr::set_names(c("log_na", "log_var"))
  )

  # test integer class
  testthat::expect_equal(
    object = class(get_column_class(col_types_test, class = "int")),
    expected = c("tbl_df", "tbl", "data.frame")
  )

  # test integer columns
  testthat::expect_equal(
    object = names(get_column_class(col_types_test, class = "int")),
    expected = c("int_na", "int_var")
  )
  # test integer names
  testthat::expect_equal(
    object = get_column_class(
      df = col_types_test,
     class = "int",
      return_tbl = FALSE
    ),
    expected = purrr::set_names(
      c("int_na", "int_var")
    )
  )
  # test double class
  testthat::expect_equal(
    object = class(get_column_class(col_types_test, class = "dbl")),
    expected = c("tbl_df", "tbl", "data.frame")
  )
  # test double columns
  testthat::expect_equal(
    object = names(get_column_class(col_types_test, class = "dbl")),
    expected = c("dbl_na", "dbl_var")
  )
  # test integer names
  testthat::expect_equal(
    object = get_column_class(
      df = col_types_test,
     class = "dbl",
      return_tbl = FALSE
    ),
    expected = purrr::set_names(
      c("dbl_na", "dbl_var")
    )
  )
  # test character class
  testthat::expect_equal(
    object = class(get_column_class(col_types_test, class = "chr")),
    expected = c("tbl_df", "tbl", "data.frame")
  )
  # test character columns
  testthat::expect_equal(
    object = names(get_column_class(col_types_test, class = "chr")),
    expected = c("chr_na", "chr_var")
  )
  # test character names
  testthat::expect_equal(
    object = get_column_class(
      df = col_types_test,
     class = "chr",
      return_tbl = FALSE
    ),
    expected = purrr::set_names(
      c("chr_na", "chr_var")
    )
  )
  # test factor class
  testthat::expect_equal(
    object = class(get_column_class(col_types_test, class = "fct")),
    expected = c("tbl_df", "tbl", "data.frame")
  )
  # test factor columns
  testthat::expect_equal(
    object = names(get_column_class(col_types_test,
     class = "fct",
      return_tbl = TRUE
    )),
    expected = c("fct_na", "fct_var", "ord_na", "ord_fct")
  )
  # test factor names
  testthat::expect_equal(
    object = get_column_class(
      df = col_types_test,
     class = "fct",
      return_tbl = FALSE
    ),
    expected = purrr::set_names(
      c(
        "fct_na",
        "fct_var",
        "ord_na",
        "ord_fct"
      )
    )
  )
})
# test error type
testthat::test_that("get_column_class() type error", {
  col_types_test <- readRDS(test_path(
    "fixtures",
    "col_types_test.rds"
  ))
  # test type error
  testthat::expect_error(
    object = get_column_class(col_types_test, class = "array")
  )
})

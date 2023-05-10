  testthat::test_that("get_col_types() works", {
    col_types_test <- readRDS(testthat::test_path("fixtures", "col_types_test.rds"))
    # test logical class
    col_types_class <- class(get_col_types(col_types_test, type = "log"))
    testthat::expect_equal(
      object = col_types_class,
      expected = c("tbl_df", "tbl", "data.frame")
    )
    # test logical colums
    col_types_names <- names(get_col_types(col_types_test, type = "log"))
    testthat::expect_equal(
      object = col_types_names,
      expected = c("log_na", "log_var")
    )
    # test logical names
    testthat::expect_equal(
      object = get_col_types(df = col_types_test,
                             type = "log",
                             return_tbl = FALSE),
      expected = purrr::set_names(c("log_na", "log_var"))
    )
    # test integer class
    col_types_class <- class(get_col_types(col_types_test, type = "int"))
    testthat::expect_equal(
      object = col_types_class,
      expected = c("tbl_df", "tbl", "data.frame")
    )
    # test integer columns
    col_types_names <- names(get_col_types(col_types_test, type = "int"))
    testthat::expect_equal(
      object = col_types_names,
      expected = c("int_na", "int_var")
    )
    # test integer names
    testthat::expect_equal(
      object = get_col_types(df = col_types_test,
                             type = "int",
                             return_tbl = FALSE),
      expected = purrr::set_names(
                        c("int_na", "int_var")
                  )
    )
    # test double class
    col_types_class <- class(get_col_types(col_types_test, type = "dbl"))
    testthat::expect_equal(
      object = col_types_class,
      expected = c("tbl_df", "tbl", "data.frame")
    )
    # test double columns
    col_types_names <- names(get_col_types(col_types_test, type = "dbl"))
    testthat::expect_equal(
      object = col_types_names,
      expected = c("dbl_na", "dbl_var")
    )
    # test integer names
    testthat::expect_equal(
      object = get_col_types(df = col_types_test,
                             type = "dbl",
                             return_tbl = FALSE),
      expected = purrr::set_names(
                        c("dbl_na", "dbl_var")
                  )
    )
    # test character class
    col_types_class <- class(get_col_types(col_types_test, type = "chr"))
    testthat::expect_equal(
      object = col_types_class,
      expected = c("tbl_df", "tbl", "data.frame")
    )
    # test character columns
    col_types_names <- names(get_col_types(col_types_test, type = "chr"))
    testthat::expect_equal(
      object = col_types_names,
      expected = c("chr_na", "chr_var")
    )
    # test character names
    testthat::expect_equal(
      object = get_col_types(df = col_types_test,
                             type = "chr",
                             return_tbl = FALSE),
      expected = purrr::set_names(
                        c("chr_na", "chr_var")
                  )
    )
    # test factor class
    col_types_class <- class(get_col_types(col_types_test, type = "fct"))
    testthat::expect_equal(
      object = col_types_class,
      expected = c("tbl_df", "tbl", "data.frame")
    )
    # test factor columns
    col_types_names <- names(get_col_types(col_types_test, type = "fct"))
    testthat::expect_equal(
      object = col_types_names,
      expected = c(
        "fct_var", "fct_na",
        "ord_fct", "ord_na")
      )
    # test factor names
    testthat::expect_equal(
      object = get_col_types(df = col_types_test,
                             type = "fct",
                             return_tbl = FALSE),
      expected = purrr::set_names(
                    c("fct_var", "fct_na",
                    "ord_fct", "ord_na")
                  )
    )
  })
  # test error type
  testthat::test_that("get_col_types() type error", {    t
    col_types_test <- readRDS(test_path("fixtures",
                                          "col_types_test.rds"))
    # test type error
    testthat::expect_error(
      object = get_col_types(col_types_test, type = "array")
    )
  })

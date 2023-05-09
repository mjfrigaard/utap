  testthat::test_that("get_col_type_tbl() logical", {
    col_types_test <- readRDS(test_path("fixtures", "col_types_test.rds"))
    # test logical class
    col_type_log <- get_col_type_tbl(col_types_test, type = "log")
    testthat::expect_equal(
      object = class(col_type_log),
      expected = c("tbl_df", "tbl", "data.frame")
    )
    # test logical names
    testthat::expect_equal(
      object = names(col_type_log),
      expected = c("log_na_bin", "log_bin")
    )
  })

  testthat::test_that("get_col_type_tbl() integer", {
    col_types_test <- readRDS(test_path("fixtures", "col_types_test.rds"))
    # test integer class
    col_types_class <- class(get_col_type_tbl(col_types_test, type = "int"))
    testthat::expect_equal(
      object = col_types_class,
      expected = c("tbl_df", "tbl", "data.frame")
    )
    # test integer names
    col_types_names <- names(get_col_type_tbl(col_types_test, type = "int"))
    testthat::expect_equal(
      object = col_types_names,
      expected = c("int_bin")
    )
  })

  testthat::test_that("get_col_type_tbl() double", {
    col_types_test <- readRDS(test_path("fixtures", "col_types_test.rds"))
    # test double class
    col_types_class <- class(get_col_type_tbl(col_types_test, type = "dbl"))
    testthat::expect_equal(
      object = col_types_class,
      expected = c("tbl_df", "tbl", "data.frame")
    )
    # test double names
    col_types_names <- names(get_col_type_tbl(col_types_test, type = "dbl"))
    testthat::expect_equal(
      object = col_types_names,
      expected = c("dbl_3l_na")
    )
  })
  #
  # testthat::test_that("get_col_type_tbl() character", {
  #   col_types_test <- readRDS(test_path("fixtures", "col_types_test.rds"))
  #   # test character class
  #   col_types_class <- class(get_col_type_tbl(col_types_test, type = "chr"))
  #   testthat::expect_equal(
  #     object = col_types_class,
  #     expected = c("tbl_df", "tbl", "data.frame")
  #   )
  #   # test character names
  #   col_types_names <- names(get_col_type_tbl(col_types_test, type = "chr"))
  #   testthat::expect_equal(
  #     object = col_types_names,
  #     expected = c("chr_3l_na_bin", "chr_4l_facet",
  #                  "chr_10l", "chr_10l_na")
  #   )
  # })
  #
  # testthat::test_that("get_col_type_tbl() factor", {
  #   col_types_test <- readRDS(test_path("fixtures", "col_types_test.rds"))
  #   # test factor class
  #   col_types_class <- class(get_col_type_tbl(col_types_test, type = "fct"))
  #   testthat::expect_equal(
  #     object = col_types_class,
  #     expected = c("tbl_df", "tbl", "data.frame")
  #   )
  #   # test character names
  #   col_types_names <- names(get_col_type_tbl(col_types_test, type = "fct"))
  #   testthat::expect_equal(
  #     object = col_types_names,
  #     expected = c(
  #       "ord_bin",
  #       "fct_bin",
  #       "ord_3l_na",
  #       "ord_3l",
  #       "fct_3l_facet",
  #       "fct_3l_na_facet",
  #       "fct_5lv6rep_na_facet",
  #       "fct_4lv3rep_na_facet",
  #       "fct_6l_na",
  #       "fct_10l",
  #       "fct_10l_na")
  #     )
  # })
  # # test error type
  # testthat::test_that("get_col_type_tbl() type error", {    t
  #   estdf_app_inputs <- readRDS(test_path("fixtures",
  #                                         "col_types_test.rds"))
  #   # test type error
  #   testthat::expect_error(
  #     object = get_col_type_tbl(col_types_test, type = "array")
  #   )
  # })

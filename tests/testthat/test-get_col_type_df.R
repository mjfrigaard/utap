  testthat::test_that("get_col_type_df() logical", {
    testdf_app_inputs <- readRDS(test_path("fixtures", "testdf_app_inputs.rds"))
    # test logical class
    col_type_log <- get_col_type_df(testdf_app_inputs, type = "log")
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

  testthat::test_that("get_col_type_df() integer", {
    testdf_app_inputs <- readRDS(test_path("fixtures", "testdf_app_inputs.rds"))
    # test integer class
    col_types_class <- class(get_col_type_df(testdf_app_inputs, type = "int"))
    testthat::expect_equal(
      object = col_types_class,
      expected = c("tbl_df", "tbl", "data.frame")
    )
    # test integer names
    col_types_names <- names(get_col_type_df(testdf_app_inputs, type = "int"))
    testthat::expect_equal(
      object = col_types_names,
      expected = c("int_bin", "int_3l_na")
    )
  })

  testthat::test_that("get_col_type_df() double", {
    testdf_app_inputs <- readRDS(test_path("fixtures", "testdf_app_inputs.rds"))
    # test double class
    col_types_class <- class(get_col_type_df(testdf_app_inputs, type = "dbl"))
    testthat::expect_equal(
      object = col_types_class,
      expected = c("tbl_df", "tbl", "data.frame")
    )
    # test double names
    col_types_names <- names(get_col_type_df(testdf_app_inputs, type = "dbl"))
    testthat::expect_equal(
      object = col_types_names,
      expected = c("dbl_3l_na")
    )
  })

  testthat::test_that("get_col_type_df() character", {
    testdf_app_inputs <- readRDS(test_path("fixtures", "testdf_app_inputs.rds"))
    # test character class
    col_types_class <- class(get_col_type_df(testdf_app_inputs, type = "chr"))
    testthat::expect_equal(
      object = col_types_class,
      expected = c("tbl_df", "tbl", "data.frame")
    )
    # test character names
    col_types_names <- names(get_col_type_df(testdf_app_inputs, type = "chr"))
    testthat::expect_equal(
      object = col_types_names,
      expected = c(
        "chr_3l_na_facet", "chr_4l_facet",
        "chr_10l", "chr_10l_na"
      )
    )
  })

  testthat::test_that("get_col_type_df() factor", {
    testdf_app_inputs <- readRDS(test_path("fixtures", "testdf_app_inputs.rds"))
    # test factor class
    col_types_class <- class(get_col_type_df(testdf_app_inputs, type = "fct"))
    testthat::expect_equal(
      object = col_types_class,
      expected = c("tbl_df", "tbl", "data.frame")
    )
    # test character names
    col_types_names <- names(get_col_type_df(testdf_app_inputs, type = "fct"))
    testthat::expect_equal(
      object = col_types_names,
      expected = c(
        "ord_bin",
        "fct_bin",
        "ord_3l_na",
        "ord_3l",
        "fct_3l_facet",
        "fct_3l_na_facet",
        "fct_5lv6rep_na_facet",
        "fct_4lv3rep_na_facet",
        "fct_6l_na",
        "fct_10l",
        "fct_10l_na")
      )
  })
  # test error type
  testthat::test_that("get_col_type_df() type error", {    t
    estdf_app_inputs <- readRDS(test_path("fixtures",
                                          "testdf_app_inputs.rds"))
    # test type error
    testthat::expect_error(
      object = get_col_type_df(testdf_app_inputs, type = "array")
    )
  })

  testthat::test_that("get_col_type_df() logical", {
    testdf_app_inputs <- testPkgApp::testdf_app_inputs
    # test logical class
    col_types_class <- class(get_col_type_df(testdf_app_inputs, type = "log"))
    testthat::expect_equal(
      object = col_types_class,
      expected = c("tbl_df", "tbl", "data.frame")
    )
    # test logical names
    col_types_names <- names(get_col_type_df(testdf_app_inputs, type = "log"))
    testthat::expect_equal(
      object = col_types_names,
      expected = c("log_na_bin", "log_bin")
    )
  })

  testthat::test_that("get_col_type_df() integer", {
    testdf_app_inputs <- testPkgApp::testdf_app_inputs
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
      expected = c("int_bin", "int_3na")
    )
  })

  testthat::test_that("get_col_type_df() double", {
    testdf_app_inputs <- testPkgApp::testdf_app_inputs
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
      expected = c("dbl_3na")
    )
  })

  testthat::test_that("get_col_type_df() character", {
    testdf_app_inputs <- testPkgApp::testdf_app_inputs
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
        "chr_facet_3na", "chr_facet_4",
        "no_facet_chr_10", "no_facet_chr_10na"
      )
    )
  })

  testthat::test_that("get_col_type_df() factor", {
    testdf_app_inputs <- testPkgApp::testdf_app_inputs
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
        "ord_bin", "fct_bin", "ord_3na", "ord_3",
        "fct_facet_3", "fct_facet_3na",
        "fct_facet_5naS", "fct_facet_4naS",
        "no_facet_fct_6na", "no_facet_10fct",
        "no_facet_fct_10na"
      )
    )
  })

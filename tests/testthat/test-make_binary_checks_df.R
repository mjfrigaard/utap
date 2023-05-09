test_that("make_binary_checks_df() logical", {
  testdf_app_inputs <- readRDS(test_path("fixtures", "testdf_app_inputs.rds"))
  log_cols <- get_col_type_df(df = testdf_app_inputs, type = "log")
  expect_equal(
    object = make_binary_checks_df(df = log_cols, type = "log"),
    expected = c(log_na_bin = "log_na_bin",
                 log_bin = "log_bin")
    )
})

test_that("make_binary_checks_df() integer", {
  int_cols <- get_col_type_df(df = testdf_app_inputs, type = "int")
  expect_equal(
    object = make_binary_checks_df(df = int_cols, type = "int"),
    expected = c(int_bin = "int_bin"))
})

test_that("make_binary_checks_df() character", {
  chr_cols <- get_col_type_df(df = testdf_app_inputs, type = "chr")
  expect_equal(
    object = make_binary_checks_df(df = chr_cols, type = "chr"),
    expected = c(chr_3l_na_bin = "chr_3l_na_bin"))
})

test_that("make_binary_checks_df() factor", {
  fct_cols <- get_col_type_df(df = testdf_app_inputs, type = "fct")
  expect_equal(
    object = make_binary_checks_df(df = fct_cols, type = "fct"),
    expected = c(ord_bin = "ord_bin",
                 fct_bin = "fct_bin"))
})

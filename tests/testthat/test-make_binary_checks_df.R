test_that("make_binary_vec() logical", {
  testdf_app_inputs <- readRDS(test_path("fixtures", "testdf_app_inputs.rds"))
  log_cols <- get_col_type_tbl(df = testdf_app_inputs, type = "log")
  expect_equal(
    object = make_binary_vec(df = log_cols, type = "log"),
    expected = c(log_na_bin = "log_na_bin",
                 log_bin = "log_bin")
    )
})

test_that("make_binary_vec() integer", {
  int_cols <- get_col_type_tbl(df = testdf_app_inputs, type = "int")
  expect_equal(
    object = make_binary_vec(df = int_cols, type = "int"),
    expected = c(int_bin = "int_bin"))
})

test_that("make_binary_vec() character", {
  chr_cols <- get_col_type_tbl(df = testdf_app_inputs, type = "chr")
  expect_equal(
    object = make_binary_vec(df = chr_cols, type = "chr"),
    expected = c(chr_3l_na_bin = "chr_3l_na_bin"))
})

test_that("make_binary_vec() factor", {
  fct_cols <- get_col_type_tbl(df = testdf_app_inputs, type = "fct")
  expect_equal(
    object = make_binary_vec(df = fct_cols, type = "fct"),
    expected = c(ord_bin = "ord_bin",
                 fct_bin = "fct_bin"))
})

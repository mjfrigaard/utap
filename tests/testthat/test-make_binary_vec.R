test_that("make_binary_vec() logical works", {
  binary_vec_test <- readRDS(testthat::test_path("fixtures", "binary_vec_test.rds"))
  # test logical
  expect_equal(
    object = select(binary_vec_test, where(is.logical)) |>
      make_binary_vec(type = "log"),
    expected = purrr::set_names(c("log", "log_na"))
  )
})
test_that("make_binary_vec() integer works", {
  binary_vec_test <- readRDS(testthat::test_path("fixtures", "binary_vec_test.rds"))
  # test integer
  expect_equal(
    object = select(binary_vec_test, where(is.integer)) |>
      make_binary_vec(type = "log"),
    expected = purrr::set_names(c("int", "int_na"))
  )
})
test_that("make_binary_vec() character works", {
  binary_vec_test <- readRDS(testthat::test_path("fixtures", "binary_vec_test.rds"))
  # test character
  expect_equal(
    object = select(binary_vec_test, where(is.character)) |>
      make_binary_vec(type = "chr"),
    expected = purrr::set_names(c("chr", "chr_na"))
  )
})
test_that("make_binary_vec() factor works", {
  binary_vec_test <- readRDS(testthat::test_path("fixtures", "binary_vec_test.rds"))
  # test factor
  expect_equal(
    object = select(
      binary_vec_test,
      dplyr::all_of(c("fct", "fct_na"))
    ) |>
      make_binary_vec(type = "fct"),
    expected = purrr::set_names(c("fct", "fct_na"))
  )
})
test_that("make_binary_vec() ordered works", {
  binary_vec_test <- readRDS(testthat::test_path("fixtures", "binary_vec_test.rds"))
  # test ordered
  expect_equal(
    object = select(
      binary_vec_test,
      dplyr::all_of(c("ord", "ord_na"))
    ) |>
      make_binary_vec(type = "fct"),
    expected = purrr::set_names(c("ord", "ord_na"))
  )
})

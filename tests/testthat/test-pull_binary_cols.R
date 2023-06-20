test_that("check_binary_vec logical works", {
  # test logical
  expect_equal(object =
      check_binary_vec(
        x = log_maker(
          size = 2,
          missing = TRUE),
    type = "log"),
    expected = TRUE)
  expect_equal(object =
      check_binary_vec(
        x = log_maker(size = 2),
        type = "log"),
    expected = TRUE)
  expect_equal(object =
      check_binary_vec(
        x = c(NA, NA, NA),
        type = "log"),
    expected = TRUE)
  expect_equal(object =
      check_binary_vec(
        x = c(FALSE, FALSE),
        type = "log"),
    expected = TRUE)
})

test_that("check_binary_vec integer works", {
  # test
  expect_equal(object = check_binary_vec(
    x = bin_maker(bin_type = "int", size = 3,
        missing = FALSE),
    type = "int"),
    expected = TRUE)
  expect_equal(object = check_binary_vec(
    x = bin_maker(bin_type = "int", size = 3,
        missing = TRUE),
    type = "int"),
    expected = TRUE)
  expect_equal(object = check_binary_vec(
    x = c(0L, 0L, NA_integer_),
    type = "int"),
    expected = TRUE)
  expect_equal(object = check_binary_vec(
    x = c(0L, 0L),
    type = "int"),
    expected = TRUE)
  expect_equal(object = check_binary_vec(
    x = c(1L, 2L, NA_integer_),
    type = "int"),
    expected = FALSE)
  expect_equal(object = check_binary_vec(
    x = c(1L, 2L), type = "int"),
    expected = FALSE)
})

test_that("check_binary_vec character works", {
  # test characters
  expect_equal(object = check_binary_vec(
    x = c("TRUE", "FALSE", NA_character_),
    type = "chr"
  ),
    expected = TRUE)
  expect_equal(object = check_binary_vec(
    x = c("TRUE", "FALSE"),
    type = "chr"),
    expected = TRUE)
  expect_equal(object = check_binary_vec(
    x = c("TRUE", "FALSE", "MAYBE", NA_character_),
    type = "chr"
  ),
    expected = FALSE)
  expect_equal(object = check_binary_vec(
    x = c("TRUE", "FALSE", "MAYBE"),
    type = "chr"),
    expected = FALSE)
  expect_equal(object = check_binary_vec(
    x = c("TRUE", "TRUE", NA_character_),
    type = "chr"
  ),
    expected = FALSE)
  expect_equal(object = check_binary_vec(
    x = c("TRUE", "TRUE", NA_character_),
    type = "chr"
  ),
    expected = FALSE)
  expect_equal(object = check_binary_vec(
    x = c("TRUE", "TRUE"),
    type = "chr"),
    expected = FALSE)
})

test_that("check_binary_vec factor works", {
  # test factors
  expect_equal(object = check_binary_vec(
    x = fct_maker(size = 4, lvls = 2,
            missing = TRUE),
        type = "fct"),
    expected = TRUE)

  expect_equal(object = check_binary_vec(
    x = fct_maker(size = 4, lvls = 2,
            missing = FALSE),
        type = "fct"),
    expected = TRUE)

  expect_equal(object = check_binary_vec(
    x = fct_maker(size = 4, lvls = 3,
            missing = TRUE),
        type = "fct"),
    expected = FALSE)

  expect_equal(object = check_binary_vec(
    x = fct_maker(size = 3, lvls = 2,
            missing = FALSE),
        type = "fct"),
    expected = TRUE)

  expect_equal(object = check_binary_vec(
    x = fct_maker(size = 2, lvls = 3,
            missing = FALSE),
        type = "fct"),
    expected = TRUE)

  expect_equal(object = check_binary_vec(x =
      factor(
        c("hot", "cold"),
        levels = c("hot", "cold", "warm")
      ), type = "fct"),
    expected = FALSE)
})

test_that("make_binary_vec() works", {
  binary_vec_test <- tibble::tibble(
    log = bin_maker(bin_type = "log", size = 3, missing = FALSE),
    log_na = bin_maker(bin_type = "log", size = 3, missing = TRUE),
    int = bin_maker(bin_type = "int", size = 3, missing = FALSE),
    int_na = bin_maker(bin_type = "int", size = 3, missing = TRUE),
    chr = bin_maker(bin_type = "chr", size = 3, missing = FALSE),
    chr_na = bin_maker(bin_type = "chr", size = 3, missing = TRUE),
    fct = bin_maker(bin_type = "fct", size = 3, missing = FALSE),
    fct_na = bin_maker(bin_type = "fct", size = 3, missing = TRUE),
    ord = bin_maker(bin_type = "ord", size = 3, missing = FALSE),
    ord_na = bin_maker(bin_type = "ord", size = 3, missing = TRUE)
  )
  # test logical
  expect_equal(
    object = select(binary_vec_test, where(is.logical)) |>
      make_binary_vec(type = "log"),
    expected = purrr::set_names(c("log", "log_na"))
  )
  # test integer
  expect_equal(
    object = select(binary_vec_test, where(is.integer)) |>
      make_binary_vec(type = "int"),
    expected = purrr::set_names(c("int", "int_na"))
  )
  # test character
  expect_equal(
    object = select(binary_vec_test, where(is.character)) |>
      make_binary_vec(type = "chr"),
    expected = purrr::set_names(c("chr", "chr_na"))
  )
  # test factor
  expect_equal(
    object = select(binary_vec_test,
      dplyr::all_of(c("fct", "fct_na"))) |>
      make_binary_vec(type = "fct"),
    expected = purrr::set_names(c("fct", "fct_na"))
  )
  # test ordered
  expect_equal(
    object = select(binary_vec_test,
      dplyr::all_of(c("ord", "ord_na"))) |>
      make_binary_vec(type = "fct"),
    expected = purrr::set_names(c("ord", "ord_na"))
  )
})

testthat::test_that("pull_binary_cols() works", {
  app_inputs_test <-
    readRDS(testthat::test_path("fixtures", "pull_cols_test.rds"))
  expect_equal(
    object = pull_binary_cols(app_inputs_test),
    expected = c(log_bin_na = "log_bin_na",
                 log_bin = "log_bin",
                 int_bin_na = "int_bin_na",
                 int_bin = "int_bin",
                 chr_bin_na = "chr_bin_na",
                 chr_bin = "chr_bin")
  )
})

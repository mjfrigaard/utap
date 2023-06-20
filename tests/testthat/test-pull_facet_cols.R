test_that("check_facet_vec() character", {
  # test character (4 unique values)
  chr_test <- check_facet_vec(x =
      chr_maker(size = 10, lvls = 4),
    type = "chr")
  expect_equal(object = chr_test,
    expected = TRUE)
  # test character (4 unique values, missing)
  chr_test <- check_facet_vec(x =
      chr_maker(
        size = 10,
        lvls = 4,
        missing = TRUE
      ),
    type = "chr")
  expect_equal(object = chr_test,
    expected = TRUE)
  # test character (5 unique values)
  chr_test <- check_facet_vec(
    x = chr_maker(size = 10, lvls = 5), type = "chr")
  expect_equal(object = chr_test,
    expected = FALSE)
  # test character (5 unique values, missing)
  chr_test <- check_facet_vec(x =
      chr_maker(
        size = 10,
        lvls = 5,
        missing = TRUE
      ),
    type = "chr")
  expect_equal(object = chr_test,
    expected = FALSE)
})

test_that("check_facet_vec() factor", {
  # test factor (4 levels)
  fct_test <- fct_maker(size = 10, lvls = 4)
  expect_equal(object = check_facet_vec(x = fct_test,
    type = "fct"),
    expected = TRUE)

  # test factor (4 levels, missing)
  fct_test_na <- fct_maker(size = 10,
    lvls = 4,
    missing = TRUE)
  expect_equal(object = check_facet_vec(x = fct_test_na,
    type = "fct"),
    expected = TRUE)

  # test factor (5 levels)
  fct_test <- fct_maker(size = 10, lvls = 5)
  expect_equal(object = check_facet_vec(x = fct_test, type = "fct"),
    expected = FALSE)

  # test factor (5 levels, missing)
  fct_test <- check_facet_vec(x = fct_maker(
    size = 10,
    lvls = 5,
    missing = TRUE
  ),
    type = "fct")
  expect_equal(object = fct_test,
    expected = FALSE)

  # test factor (4 levels, 5 represented, with missing = 5 unique)
  test_facet <- check_facet_vec(x =
      factor(
        c("group 1",
          "group 2",
          "group 3",
          NA_character_,
          "group 4",
          "group 5"),
        levels = c("group 1", "group 2", "group 3",
          "group 4")
      ),
    type = "fct")
  expect_equal(object = test_facet,
    expected = TRUE)
  # test factor (5 levels, 4 represented, with missing = 4 unique)
  test_facet <- check_facet_vec(x =
      factor(
        c(
          "group 1",
          "group 2",
          "group 3",
          NA_character_,
          "group 4"
        ),
        levels = c("group 1", "group 2", "group 3",
          "group 4", "group 5")
      ),
    type = "fct")
  expect_equal(object = test_facet,
    expected = TRUE)
})

test_that("check_facet_vec() ordered", {
  # test ordered factor (4 levels)
  ord_test <- check_facet_vec(
    x = ord_maker(
      size = 10,
      lvls = 4),
      type = "ord")
  expect_equal(object = ord_test,
    expected = TRUE)

  # test ordered factor (4 levels, missing)
  ord_test <- check_facet_vec(
    x = ord_maker(
      size = 10,
      lvls = 4,
      missing = TRUE
  ),
    type = "ord")
  expect_equal(object = ord_test,
    expected = TRUE)

  # test ordered factor (6 levels)
  ord_test <- check_facet_vec(
    x = ord_maker(
      size = 10,
      lvls = 5),
      type = "ord")
  expect_equal(object = ord_test,
    expected = FALSE)

  # test ordered factor (5 levels, missing)
  ord_test <- check_facet_vec(
    x = ord_maker(size = 10,
      lvls = 5,
      missing = TRUE),
    type = "ord")
  expect_equal(object = ord_test,
    expected = FALSE)

  # test ordered factor (4 levels, 5 represented, missing)
  ord_test <- check_facet_vec(
    x = factor(c("level 1", "level 2", "level 3",
                 NA_character_, "level 4", "level 5",
                 "level 6"),
        levels = c("level 1", "level 2", "level 3",
          "level 4"),
        ordered = TRUE),
    type = "ord")
  expect_equal(object = ord_test, expected = TRUE)

  # test ordered factor (5 levels, 4 represented, missing)
  ord_test <- check_facet_vec(x = factor(
    c("level 1",
      "level 2",
      "level 3",
      NA_character_,
      "level 4"),
    levels = c("level 1", "level 2", "level 3",
      "level 4", "level 5")
  ),
    type = "ord")
  expect_equal(object = ord_test, expected = TRUE)
})

testthat::test_that("pull_facet_cols() character cols", {
  expect_equal(
    object = pull_facet_cols(
      df = tibble::tibble(
        facet_na = facet_maker(
          facet_type = "chr", size = 10, lvls = 4, missing = TRUE),
        facet = facet_maker(
          facet_type = "fct", size = 10, lvls = 4, missing = FALSE),
        no_facet_na = facet_maker(
          facet_type = "chr", size = 10, lvls = 5, missing = TRUE),
        no_facet = facet_maker(
          facet_type = "fct", size = 10, lvls = 5, missing = FALSE))),
    expected = purrr::set_names(c("facet_na", "facet"))
  )
})

testthat::test_that("pull_facet_cols() factor cols", {
  expect_equal(
    object = pull_facet_cols(df =
      tibble::tibble(
        facet = facet_maker(
          facet_type = "fct", size = 10, lvls = 4, missing = FALSE),
        facet_na = facet_maker(
          facet_type = "fct", size = 10, lvls = 4, missing = TRUE),
        no_facet = facet_maker(
          facet_type = "fct", size = 10, lvls = 5, missing = FALSE),
        no_facet_na = facet_maker(
          facet_type = "fct", size = 10, lvls = 5, missing = TRUE))),
    expected = purrr::set_names(c("facet", "facet_na"))
  )
})

testthat::test_that("pull_facet_cols() ordinal cols", {
  expect_equal(
    object = pull_facet_cols(
      df = tibble::tibble(
        facet = facet_maker(
          facet_type = "ord", size = 10, lvls = 4, missing = FALSE),
        facet_na = facet_maker(
          facet_type = "ord", size = 10, lvls = 4, missing = TRUE),
        no_facet = facet_maker(
          facet_type = "ord", size = 10, lvls = 5, missing = FALSE),
        no_facet_na = facet_maker(
          facet_type = "ord", size = 10, lvls = 5, missing = TRUE))),
    expected = purrr::set_names(c("facet", "facet_na"))
  )
})

testthat::test_that("pull_facet_cols() works", {
  app_inputs_test <-
    readRDS(testthat::test_path("fixtures", "pull_cols_test.rds"))
  # remove binary columns
  expect_equal(
    object = dplyr::select(app_inputs_test,!dplyr::contains("bin")) |> pull_facet_cols(),
    expected =
      c(chr_facet4 = "chr_facet4",
        chr_facet4_na = "chr_facet4_na",
        fct_facet4 = "fct_facet4",
        fct_facet4_na = "fct_facet4_na",
        ord_facet4 = "ord_facet4",
        ord_facet4_na = "ord_facet4_na")
  )
})

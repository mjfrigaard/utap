test_that("check_facet_vec() character", {
  # test character (5 unique values)
  chr_test <- check_facet_vec(x =
      chr_maker(size = 10, lvls = 5),
    type = "chr")
  expect_equal(object = chr_test,
    expected = TRUE)
  # test character (5 unique values, missing)
  chr_test <- check_facet_vec(x =
      chr_maker(
        size = 10,
        lvls = 5,
        missing = TRUE
      ),
    type = "chr")
  expect_equal(object = chr_test,
    expected = TRUE)
  # test character (6 unique values)
  chr_test <- check_facet_vec(
    x = chr_maker(size = 10, lvls = 6), type = "chr")
  expect_equal(object = chr_test,
    expected = FALSE)
  # test character (6 unique values, missing)
  chr_test <- check_facet_vec(x =
      chr_maker(
        size = 10,
        lvls = 6,
        missing = TRUE
      ),
    type = "chr")
  expect_equal(object = chr_test,
    expected = FALSE)
})

test_that("check_facet_vec() factor", {
  # test factor (5 levels)
  fct_test <- fct_maker(size = 10, lvls = 5)
  expect_equal(object = check_facet_vec(x = fct_test,
    type = "fct"),
    expected = TRUE)

  # test factor (5 levels, missing)
  fct_test_na <- fct_maker(size = 10,
    lvls = 5,
    missing = TRUE)
  expect_equal(object = check_facet_vec(x = fct_test_na,
    type = "fct"),
    expected = TRUE)

  # test factor (6 levels)
  fct_test <- fct_maker(size = 10, lvls = 6)
  expect_equal(object = check_facet_vec(x = fct_test, type = "fct"),
    expected = FALSE)

  # test factor (6 levels, missing)
  fct_test <- check_facet_vec(x = fct_maker(
    size = 10,
    lvls = 6,
    missing = TRUE
  ),
    type = "fct")
  expect_equal(object = fct_test,
    expected = FALSE)

  # test factor (5 levels, 6 represented, with missing = 5 unique)
  test_facet <- check_facet_vec(x =
      factor(
        c("group 1",
          "group 2",
          "group 3",
          NA_character_,
          "group 4",
          "group 5",
          "group 6"),
        levels = c("group 1", "group 2", "group 3",
          "group 4", "group 5")
      ),
    type = "fct")
  expect_equal(object = test_facet,
    expected = TRUE)
  # test factor (6 levels, 5 represented, with missing = 5 unique)
  test_facet <- check_facet_vec(x =
      factor(
        c(
          "group 1",
          "group 2",
          "group 3",
          NA_character_,
          "group 4",
          "group 5"
        ),
        levels = c("group 1", "group 2", "group 3",
          "group 4", "group 5", "group 6")
      ),
    type = "fct")
  expect_equal(object = test_facet,
    expected = TRUE)
})

test_that("check_facet_vec() ordered", {
  # test ordered factor (5 levels)
  ord_test <- check_facet_vec(x = ord_maker(size = 10, lvls = 5),
    type = "ord")
  expect_equal(object = ord_test,
    expected = TRUE)

  # test ordered factor (5 levels, missing)
  ord_test <- check_facet_vec(x = ord_maker(
    size = 10,
    lvls = 5,
    missing = TRUE
  ),
    type = "ord")
  expect_equal(object = ord_test,
    expected = TRUE)

  # test ordered factor (6 levels)
  ord_test <- check_facet_vec(x = ord_maker(size = 10, lvls = 6),
    type = "ord")
  expect_equal(object = ord_test,
    expected = FALSE)

  # test ordered factor (6 levels, missing)
  ord_test <- check_facet_vec(
    x = ord_maker(size = 10, lvls = 6, missing = TRUE),
    type = "ord")
  expect_equal(object = ord_test,
    expected = FALSE)

  # test ordered factor (5 levels, 6 represented, missing)
  ord_test <- check_facet_vec(
    x = factor(c("level 1", "level 2", "level 3",
                 NA_character_, "level 4", "level 5",
                 "level 6"),
        levels = c("level 1", "level 2", "level 3",
          "level 4", "level 5"),
        ordered = TRUE),
    type = "ord")
  expect_equal(object = ord_test, expected = TRUE)

  # test ordered factor (6 levels, 5 represented, missing)
  ord_test <- check_facet_vec(x = factor(
    c(
      "level 1",
      "level 2",
      "level 3",
      NA_character_,
      "level 4",
      "level 5"
    ),
    levels = c("level 1", "level 2", "level 3",
      "level 4", "level 5", "level 6")
  ),
    type = "ord")
  expect_equal(object = ord_test, expected = TRUE)
})

test_that("make_facet_vec() facets work", {
  facet_vec_test <-
    readRDS(testthat::test_path("fixtures", "facet_vec_test.rds"))
  # test character
  expect_equal(
    object = dplyr::select(facet_vec_test,
      dplyr::where(is.character)) |>
      make_facet_vec(type = "chr"),
    expected = purrr::set_names(c(chr5 = "chr5", chr5_na = "chr5_na"))
  )
  # test all factors
  expect_equal(
    object = dplyr::select(facet_vec_test,
      dplyr::where(is.factor)) |>
      make_facet_vec(type = "fct"),
    expected = purrr::set_names(
      c(
        fct5 = "fct5",
        fct5_na = "fct5_na",
        ord5 = "ord5",
        ord5_na = "ord5_na"
      )
    )
  )
  # test ordered factors
  expect_equal(
    object = dplyr::select(facet_vec_test,
      dplyr::all_of(c("ord5", "ord5_na"))) |>
      make_facet_vec(type = "ord"),
    expected = purrr::set_names(c(ord5 = "ord5", ord5_na = "ord5_na"))
  )
  # test factors
  expect_equal(
    object = dplyr::select(facet_vec_test,
      dplyr::all_of(c("fct5", "fct5_na"))) |>
      make_facet_vec(type = "fct"),
    expected = purrr::set_names(c(fct5 = "fct5", fct5_na = "fct5_na"))
  )
})

test_that("make_facet_vec() non-facets work", {
  facet_vec_test <-
    readRDS(testthat::test_path("fixtures", "facet_vec_test.rds"))
  # test character (failures)
  expect_equal(
    object = dplyr::select(facet_vec_test,
      dplyr::all_of(c("chr6", "chr6_na"))) |>
      make_facet_vec(type = "chr"),
    expected = structure(character(0), names = character(0))
  )
  # test factor (failures)
  expect_equal(
    object = dplyr::select(facet_vec_test,
      dplyr::all_of(c("fct6", "fct6_na"))) |>
      make_facet_vec(type = "fct"),
    expected = structure(character(0), names = character(0))
  )
  # test ordered factor (failures)
  expect_equal(
    object = dplyr::select(facet_vec_test,
      dplyr::all_of(c("ord6", "ord6_na"))) |>
      make_facet_vec(type = "ord"),
    expected = structure(character(0), names = character(0))
  )
})

testthat::test_that("pull_facet_cols() character cols", {
test_chr_facets <- tibble::tibble(
        chr_facet5_na = facet_maker(
          facet_type = "chr", size = 10, lvls = 5, missing = TRUE),
        chr_facet5 = facet_maker(
          facet_type = "fct", size = 10, lvls = 5, missing = FALSE),
        chr_facet6_na = facet_maker(
          facet_type = "chr", size = 10, lvls = 6, missing = TRUE),
        chr_facet6 = facet_maker(
          facet_type = "fct", size = 10, lvls = 6, missing = FALSE))

  expect_equal(
    object = pull_facet_cols(test_chr_facets),
    expected = purrr::set_names(c("chr_facet5_na", "chr_facet5"))
  )
})

testthat::test_that("pull_facet_cols() factor cols", {
test_fct_facets <- tibble::tibble(
        fct_facet5 = facet_maker(
          facet_type = "fct", size = 10, lvls = 5, missing = FALSE),
        fct_facet5_na = facet_maker(
          facet_type = "fct", size = 10, lvls = 5, missing = TRUE),
        fct_facet6 = facet_maker(
          facet_type = "fct", size = 10, lvls = 6, missing = FALSE),
        fct_facet6_na = facet_maker(
          facet_type = "fct", size = 10, lvls = 6, missing = TRUE))
  expect_equal(
    object = pull_facet_cols(test_fct_facets),
    expected = purrr::set_names(c("fct_facet5", "fct_facet5_na"))
  )
})

testthat::test_that("pull_facet_cols() ordinal cols", {
test_ord_facets <- tibble::tibble(
        ord_facet5 = facet_maker(
          facet_type = "ord", size = 10, lvls = 5, missing = FALSE),
        ord_facet5_na = facet_maker(
          facet_type = "ord", size = 10, lvls = 5, missing = TRUE),
        ord_facet6 = facet_maker(
          facet_type = "ord", size = 10, lvls = 6, missing = FALSE),
        ord_facet6_na = facet_maker(
          facet_type = "ord", size = 10, lvls = 6, missing = TRUE))
  expect_equal(
    object = pull_facet_cols(test_ord_facets),
    expected = purrr::set_names(c("ord_facet5", "ord_facet5_na"))
  )
})

testthat::test_that("pull_facet_cols() works", {
  app_inputs_test <-
    readRDS(testthat::test_path("fixtures", "pull_cols_test.rds"))
  # remove binary columns
  expect_equal(
    object = dplyr::select(app_inputs_test,!dplyr::contains("bin")) |> pull_facet_cols(),
    expected =
      c(chr_facet5 = "chr_facet5",
        chr_facet5_na = "chr_facet5_na",
        fct_facet5 = "fct_facet5",
        fct_facet5_na = "fct_facet5_na",
        ord_facet5 = "ord_facet5",
        ord_facet5_na = "ord_facet5_na")
  )
})

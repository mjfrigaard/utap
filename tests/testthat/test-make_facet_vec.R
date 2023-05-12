test_that("make_facet_vec() facets work", {
  facet_vec_test <- readRDS(testthat::test_path("fixtures", "facet_vec_test.rds"))
  # test character
  expect_equal(
    object = dplyr::select(
      facet_vec_test,
      dplyr::where(is.character)
    ) |>
      make_facet_vec(type = "chr"),
    expected = purrr::set_names(
      c(chr5 = "chr5", chr5_na = "chr5_na")
    )
  )
  # test all factors
  expect_equal(
    object = dplyr::select(
      facet_vec_test,
      dplyr::where(is.factor)
    ) |>
      make_facet_vec(type = "fct"),
    expected = purrr::set_names(
      c(
        fct5 = "fct5", fct5_na = "fct5_na",
        ord5 = "ord5", ord5_na = "ord5_na"
      )
    )
  )
  # test ordered factors
  expect_equal(
    object = dplyr::select(
      facet_vec_test,
      dplyr::all_of(c("ord5", "ord5_na"))
    ) |>
      make_facet_vec(type = "ord"),
    expected = purrr::set_names(
      c(ord5 = "ord5", ord5_na = "ord5_na")
    )
  )
  # test factors
  expect_equal(
    object = dplyr::select(
      facet_vec_test,
      dplyr::all_of(c("fct5", "fct5_na"))
    ) |>
      make_facet_vec(type = "fct"),
    expected = purrr::set_names(
      c(fct5 = "fct5", fct5_na = "fct5_na")
    )
  )
})

test_that("make_facet_vec() non-facets work", {
  facet_vec_test <- readRDS(testthat::test_path("fixtures", "facet_vec_test.rds"))
  # test character (failures)
  expect_equal(
    object = dplyr::select(
      facet_vec_test,
      dplyr::all_of(c("chr6", "chr6_na"))
    ) |>
      make_facet_vec(type = "chr"),
    expected = structure(character(0), names = character(0))
  )
  # test factor (failures)
  expect_equal(
    object = dplyr::select(
      facet_vec_test,
      dplyr::all_of(c("fct6", "fct6_na"))
    ) |>
      make_facet_vec(type = "fct"),
    expected = structure(character(0), names = character(0))
  )
  # test ordered factor (failures)
  expect_equal(
    object = dplyr::select(
      facet_vec_test,
      dplyr::all_of(c("ord6", "ord6_na"))
    ) |>
      make_facet_vec(type = "ord"),
    expected = structure(character(0), names = character(0))
  )
})

test_that("check_facet_vec() factor", {
    # test factor (5 levels)
    test_facet <- check_facet_vec(x =
        factor(c("group 1", "group 2", "group 3",
                 "group 4", "group 5"),
        levels = c("group 1", "group 2", "group 3",
                   "group 4", "group 5")),
        type = "fct")
    expect_equal(object = test_facet,
                 expected = TRUE)
    # test factor (5 levels, missing)
    test_facet <- check_facet_vec(x =
        factor(c("group 1", "group 2", "group 3", NA_character_,
                 "group 4", "group 5"),
          levels = c("group 1", "group 2", "group 3",
                     "group 4", "group 5")), type = "fct")
    expect_equal(object = test_facet,
      expected = TRUE)

    # test factor (6 levels)
    test_facet <- check_facet_vec(x =
                factor(c("group 1", "group 2", "group 3",
                         "group 4", "group 5", "group 6"),
                  levels = c("group 1", "group 2", "group 3",
                             "group 4", "group 5", "group 6")),
                  type = "fct")
    expect_equal(object = test_facet,
                 expected = FALSE)
    # test factor (6 levels, missing)
    test_facet <- check_facet_vec(x =
                factor(c("group 1", "group 2", "group 3", NA_character_,
                         "group 4", "group 5", "group 6"),
                  levels = c("group 1", "group 2", "group 3",
                             "group 4", "group 5", "group 6")),
                  type = "fct")
    expect_equal(object = test_facet,
                 expected = FALSE)

    # test factor (5 levels, 6 represented, missing)
    test_facet <- check_facet_vec(x =
                factor(c("group 1", "group 2", "group 3", NA_character_,
                         "group 4", "group 5", "group 6"),
                  levels = c("group 1", "group 2", "group 3",
                             "group 4", "group 5")),
                  type = "fct")
    expect_equal(object = test_facet,
                 expected = TRUE)
    # test factor (6 levels, 5 represented, missing)
    test_facet <- check_facet_vec(x =
                factor(c("group 1", "group 2", "group 3", NA_character_,
                         "group 4", "group 5"),
                  levels = c("group 1", "group 2", "group 3",
                             "group 4", "group 5", "group 6")),
                    type = "fct")
    expect_equal(object = test_facet,
                 expected = FALSE)
})

test_that("check_facet_vec() ordered", {
  # test ordered factor (5 levels)
  ord_test <- check_facet_vec(x =
                    factor(c("level 1", "level 2", "level 3",
                        "level 4", "level 5"),
                      levels = c("level 1", "level 2", "level 3",
                        "level 4", "level 5"), ordered = TRUE),
                type = "fct")

  expect_equal(object = ord_test,
    expected = TRUE)

  # test ordered factor (5 levels, missing)
  ord_test <- check_facet_vec(x =
                    factor(c("level 1", "level 2", "level 3",
                        "level 4", "level 5", NA_character_),
                      levels = c("level 1", "level 2", "level 3",
                                 "level 4", "level 5"),
                      ordered = TRUE),
                type = "fct")
  expect_equal(
    object = ord_test,
    expected = TRUE)

  # test ordered factor (6 levels)
  ord_test <- check_facet_vec(x =
      factor(c("level 1", "level 2", "level 3",
               "level 4", "level 5", "level 6"),
        levels = c("level 1", "level 2", "level 3",
                   "level 4", "level 5", "level 6"),
        ordered = TRUE), type = "fct")

  expect_equal(
    object = ord_test,
    expected = FALSE)

  # test ordered factor (6 levels, missing)
  ord_test <- check_facet_vec(x =
                  factor(c("level 1", "level 2", "level 3",
                          NA_character_, "level 4", "level 5",
                          "level 6"),
                    levels = c("level 1", "level 2", "level 3",
                          "level 4", "level 5", "level 6"),
                    ordered = TRUE),
                type = "fct")
  expect_equal(
    object = ord_test,
    expected = FALSE)

  # test ordered factor (5 levels, 6 represented, missing)
  ord_test <- check_facet_vec(x =
                  factor(c("level 1", "level 2", "level 3", NA_character_,
                           "level 4", "level 5", "level 6"),
                    levels = c("level 1", "level 2", "level 3",
                                "level 4", "level 5"),
                    ordered = TRUE),
              type = "fct")
  expect_equal(
    object = ord_test,
    expected = TRUE)

  # test ordered factor (6 levels, 5 represented, missing)
  ord_test <- check_facet_vec(
    x =
      factor(c("level 1", "level 2", "level 3", NA_character_,
               "level 4", "level 5"),
            levels = c("level 1", "level 2", "level 3",
                        "level 4", "level 5", "level 6")),
            type = "fct")
  expect_equal(
    object = ord_test,
    expected = FALSE)
})

test_that("check_facet_vec() character", {

    # test character (5 unique values)
    chr_test <- check_facet_vec(x =
                              c("value 1", "value 2", "value 3",
                                "value 4", "value 5"),
                              type = "chr")
    expect_equal(object = chr_test,
                 expected = TRUE)
    # test character (5 unique values, missing)
    chr_test <- check_facet_vec(x =
                              c("value 1", "value 2", "value 3",
                                "value 4", "value 5", NA_character_),
                              type = "chr")
    expect_equal(object = chr_test,
                 expected = TRUE)
    # test character (6 unique values)
    chr_test <- check_facet_vec(x =
                              c("value 1", "value 2", "value 3",
                                "value 4", "value 5", "value 6"),
                              type = "chr")
    expect_equal(object = chr_test,
                 expected = FALSE)
    # test character (6 unique values, missing)
    chr_test <- check_facet_vec(x =
                              c("value 1", "value 2", "value 3",
                                "value 4", "value 5", "value 6",
                                NA_character_),
                              type = "chr")
    expect_equal(object = chr_test,
                 expected = FALSE)
})

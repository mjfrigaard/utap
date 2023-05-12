facet_vec_test <- tibble::tibble(
  # character facets
  facet_maker(type = "chr", size = 10, lvls = 5, missing = FALSE),
  facet_maker(type = "chr", size = 10, lvls = 5, missing = TRUE),
  facet_maker(type = "chr", size = 10, lvls = 6, missing = FALSE),
  facet_maker(type = "chr", size = 10, lvls = 6, missing = TRUE),
  # factor facets
  facet_maker(type = "fct", size = 10, lvls = 5, missing = FALSE),
  facet_maker(type = "fct", size = 10, lvls = 5, missing = TRUE),
  facet_maker(type = "fct", size = 10, lvls = 6, missing = FALSE),
  facet_maker(type = "fct", size = 10, lvls = 6, missing = TRUE),
  # ordered facets
  facet_maker(type = "ord", size = 10, lvls = 5, missing = FALSE),
  facet_maker(type = "ord", size = 10, lvls = 5, missing = TRUE),
  facet_maker(type = "ord", size = 10, lvls = 6, missing = FALSE),
  facet_maker(type = "ord", size = 10, lvls = 6, missing = TRUE)
) |>
  purrr::set_names(c(
    "chr5", "chr5_na", "chr6", "chr6_na",
    "fct5", "fct5_na", "fct6", "fct6_na",
    "ord5", "ord5_na", "ord6", "ord6_na"
  ))
# export to tests/testthat/fixtures/
saveRDS(facet_vec_test,
  file = "tests/testthat/fixtures/facet_vec_test.rds"
)

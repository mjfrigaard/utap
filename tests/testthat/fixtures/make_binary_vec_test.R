binary_vec_test <- tibble::tibble(
  bin_maker(type = "log", size = 10, missing = FALSE),
  bin_maker(type = "log", size = 10, missing = TRUE),
  bin_maker(type = "int", size = 10, missing = FALSE),
  bin_maker(type = "int", size = 10, missing = TRUE),
  bin_maker(type = "chr", size = 10, missing = FALSE),
  bin_maker(type = "chr", size = 10, missing = TRUE),
  bin_maker(type = "fct", size = 10, missing = FALSE),
  bin_maker(type = "fct", size = 10, missing = TRUE),
  bin_maker(type = "ord", size = 10, missing = FALSE),
  bin_maker(type = "ord", size = 10, missing = TRUE)
) |>
  purrr::set_names(c(
    "log", "log_na", "int", "int_na", "chr",
    "chr_na", "fct", "fct_na", "ord", "ord_na"
  ))
# export to tests/testthat/fixtures/
saveRDS(binary_vec_test,
  file = "tests/testthat/fixtures/binary_vec_test.rds"
)

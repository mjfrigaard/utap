col_types_test <- tibble::tibble(
               log_na = log_maker(size = 10, missing = TRUE),
               log_var = log_maker(size = 10),
               int_na = int_maker(size = 10, missing = TRUE),
               int_var = int_maker(size = 10),
               dbl_na = dbl_maker(10, missing = TRUE),
               dbl_var = dbl_maker(size = 10),
               chr_na = chr_maker(10, missing = TRUE),
               chr_var = chr_maker(10),
               fct_var = fct_maker(10),
               fct_na = fct_maker(10, missing = TRUE),
               ord_fct = ord_maker(10),
               ord_na = ord_maker(10, missing = TRUE))
# export to tests/testthat/fixtures/
saveRDS(col_types_test,
  file = "tests/testthat/fixtures/col_types_test.rds")

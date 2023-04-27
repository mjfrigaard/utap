# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/tests.html
# * https://testthat.r-lib.org/reference/test_package.html#special-files

library(testthat)
library(testPkgApp)
# test_file("tests/testthat/test-gg_base.R")
# test_file("tests/testthat/test-gg_points.R")
# test_file("tests/testthat/test-mod_var_input_server.R")
# test_file("tests/testthat/test-mod_display_plot_server.R")
testthat::test_dir("tests/testthat/")
# devtools::test()



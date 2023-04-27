
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `testPkgApp`

<!-- badges: start -->
<!-- badges: end -->

The goal of `testPkgApp` is to demonstrate how to build and test a shiny
application using `usethis`, `shiny::testServer()`, and `shinytest2`.

## Installation

You don’t want to install this package, but you might want to download
it as an example (or read through [this
post](https://mjfrigaard.github.io/posts/testing-shiny/) to learn about
it’s contents).

## Run the app

``` r
testPkgApp::runShinyApp()
```

# Unit tests

Check the unit tests for `gg_base()` and `gg_points()` in

    #> tests/testthat/
    #> ├── test-gg_base.R
    #> └── test-gg_points.R

# Shiny server tests

Check the shiny `testServer()` tests for `mod_var_input_server()` and
`mod_display_plot_server()` in

    #> tests/testthat/
    #> ├── test-mod_display_plot_server.R
    #> └── test-mod_var_input_server.R


<!-- README.md is generated from README.Rmd. Please edit that file -->

# `utap`

<!-- badges: start -->
<!-- badges: end -->

The goal of `utap` is to demonstrate how to perform unit tests for shiny
utility functions with [`testthat`](https://testthat.r-lib.org/).

## Installation

You don’t want to install this package, but you might want to download
it as an example (or read through [this
post](https://mjfrigaard.github.io/posts/test-shiny-p1/) to learn about
it’s contents).

# R files

    #> R/
    #> ├── nin.R
    #> ├── pull_binary_cols.R
    #> ├── pull_cat_cols.R
    #> ├── pull_facet_cols.R
    #> ├── pull_numeric_cols.R
    #> ├── select_by_class.R
    #> ├── testtthat.R
    #> └── utap-package.R

# Unit tests

    #> tests/testthat/
    #> ├── test-pull_binary_cols.R
    #> ├── test-pull_cat_cols.R
    #> ├── test-pull_facet_cols.R
    #> ├── test-pull_numeric_cols.R
    #> ├── test-select_by_class.R
    #> └── test-utils.R

# Unit test results

``` default
==> devtools::test()

ℹ Testing utap
✔ | F W  S  OK | Context
✔ |         29 | pull_binary_cols
✔ |          4 | pull_cat_cols
✔ |         20 | pull_facet_cols
✔ |          5 | pull_numeric_cols
✔ |         25 | select_by_class
✔ |          3 | utils        

══ Results ═══════════════════
Duration: 2.0 s

[ FAIL 0 | WARN 0 | SKIP 0 | PASS 86 ]
```

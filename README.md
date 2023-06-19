
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

# Unit tests

    #> tests/testthat/
    #> ├── test-check_binary_vec.R
    #> ├── test-check_facet_vec.R
    #> ├── test-get_column_class.R
    #> ├── test-make_binary_vec.R
    #> ├── test-make_facet_vec.R
    #> ├── test-pull_binary_cols.R
    #> ├── test-pull_cat_cols.R
    #> ├── test-pull_facet_cols.R
    #> └── test-pull_numeric_cols.R

# Unit test results

``` bash
==> devtools::test()

ℹ Testing utap
✔ | F W S  OK | Context
✔ |         1 | pull_binary_cols [0.1s]      
✔ |         1 | pull_cat_cols                   
✔ |        25 | check_binary_vec [0.1s]         
✔ |        16 | check_facet_vec                 
✔ |         1 | pull_facet_cols                
✔ |        16 | get_column_class [0.1s]            
✔ |         5 | make_binary_vec                  
✔ |         7 | make_facet_vec                   
✔ |         1 | pull_numeric_cols                   

══ Results ════════════════════════════════════════════
Duration: 0.8 s

[ FAIL 0 | WARN 0 | SKIP 0 | PASS 73 ]
```


<!-- README.md is generated from README.Rmd. Please edit that file -->

# `testPkgApp`

<!-- badges: start -->
<!-- badges: end -->

The goal of `testPkgApp` is to demonstrate how to perform unit tests for
shiny utility functions with [`testthat`](https://testthat.r-lib.org/).

## Installation

You don’t want to install this package, but you might want to download
it as an example (or read through [this
post](https://mjfrigaard.github.io/posts/test-shiny-p1/) to learn about
it’s contents).

# Unit tests

    #> tests/testthat/
    #> ├── test-binary_app_inputs.R
    #> ├── test-cat_app_inputs.R
    #> ├── test-check_binary_vec.R
    #> ├── test-check_facet_vec.R
    #> ├── test-facet_app_inputs.R
    #> ├── test-get_col_types.R
    #> ├── test-make_binary_vec.R
    #> ├── test-make_facet_vec.R
    #> └── test-num_app_inputs.R

# Unit test results

``` bash
==> devtools::test()

ℹ Testing testPkgApp
✔ | F W S  OK | Context
✔ |         1 | binary_app_inputs [0.1s]      
✔ |         1 | cat_app_inputs                   
✔ |        25 | check_binary_vec [0.1s]         
✔ |        16 | check_facet_vec                 
✔ |         1 | facet_app_inputs                
✔ |        16 | get_col_types [0.1s]            
✔ |         5 | make_binary_vec                  
✔ |         7 | make_facet_vec                   
✔ |         1 | num_app_inputs                   

══ Results ════════════════════════════════════════════
Duration: 0.8 s

[ FAIL 0 | WARN 0 | SKIP 0 | PASS 73 ]
```

# Testing (outside of a package)

This is a small example of testing outside of a package environment. You'll find the following files:

```
├── README.md
├── _app.R
├── app.R
├── app.Rproj
└── tests
    ├── testthat
    │   ├── _test-app.R
    │   └── test-mods.R
    └── testthat.R

3 directories, 7 files
```

`_app.R` and `tests/testthat/_test-app.R` show how `shiny::testServer()` works in an application *without* modules.  

`app.R` and `tests/testthat/test-mods.R` show how `shiny::testServer()` works in an application *with* modules.  

The `tests/testthat.R` file contains the code to run the tests:

``` r
library(testthat)
source("app.R")
test_dir(path = "/Users/mjfrigaard/projects/dev/app/tests/testthat/")
# testthat::test_file("/Users/mjfrigaard/projects/dev/app/tests/testthat/test-app.R")
testthat::test_file("/Users/mjfrigaard/projects/dev/app/tests/testthat/test-mods.R")
```

Note that this is currently set to test the application with modules. 

For more information on testing outside of the package environment, see this discussion on GitHub: https://github.com/r-lib/testthat/issues/659


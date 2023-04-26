shiny::testServer(
  mod_var_input_server,
  {
    ns <- session$ns
    expect_true(object = inherits(ns, "function"))
    expect_true(object = grepl(id, ns("")))
    expect_true(object = grepl("test", ns("test")))
  }
)

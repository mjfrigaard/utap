shiny::testServer(
  mod_display_plot_server,
  args = list(
    var_inputs =
      list(
        x = "imdb_rating",
        y = "audience_score",
        z = "mpaa_rating",
        alpha = 0.5,
        size = 2,
        plot_title = ""
      )
  ),
  {
    expect_equal(nrow(movies()), 651)
    expect_equal(ncol(movies()), 34)
    expect_equal(
      object = colnames(movies()),
      expected = c(
        "title", "title_type", "genre", "runtime", "mpaa_rating",
        "studio", "thtr_rel_date", "thtr_rel_year", "thtr_rel_month",
        "thtr_rel_day", "dvd_rel_date", "dvd_rel_year", "dvd_rel_month",
        "dvd_rel_day", "imdb_rating", "imdb_num_votes", "critics_rating",
        "critics_score", "audience_rating", "audience_score", "best_pic_nom",
        "best_pic_win", "best_actor_win", "best_actress_win", "best_dir_win",
        "top200_box", "director", "actor1", "actor2", "actor3", "actor4",
        "actor5", "imdb_url", "rt_url"
      )
    )
    expect_equal(
      object = names(inputs()),
      expected = list(
        x = "imdb_rating",
        y = "audience_score",
        z = "mpaa_rating",
        alpha = 0.5,
        size = 2L,
        plot_title = ""
      )
    )
    ns <- session$ns
    expect_true(object = inherits(ns, "function"))
    expect_true(object = grepl(id, ns("")))
    expect_true(object = grepl("test", ns("test")))

    # Here are some examples of tests you can
    # run on your module
    # - Testing the setting of inputs
    # session$setInputs(x = 1)
    # expect_true(input$x == 1)
    # - If ever your input updates a reactiveValues
    # - Note that this reactiveValues must be passed
    # - to the testServer function via args = list()
    # expect_true(r$x == 1)
    # - Testing output
    # expect_true(inherits(output$tbl$html, "html"))
  }
)

shiny::testServer(
  mod_display_plot_server,
  args = list(var_inputs = reactive(
    list(
      x = "imdb_rating",
      y = "audience_score",
      z = "mpaa_rating",
      alpha = 0.5,
      size = 2,
      plot_title = ""
    )
  )),
  {
    # check movies
    expect_true(is.data.frame(movies()))
    # check dimensions
    expect_equal(object = dim(movies()), expected = c(651L, 34L))
    # check names
    expect_equal(
      object = colnames(movies()),
      expected = c(
        "title", "title_type", "genre",
        "runtime", "mpaa_rating",
        "studio", "thtr_rel_date",
        "thtr_rel_year", "thtr_rel_month",
        "thtr_rel_day", "dvd_rel_date",
        "dvd_rel_year", "dvd_rel_month",
        "dvd_rel_day", "imdb_rating",
        "imdb_num_votes", "critics_rating",
        "critics_score", "audience_rating",
        "audience_score", "best_pic_nom",
        "best_pic_win", "best_actor_win",
        "best_actress_win", "best_dir_win",
        "top200_box", "director", "actor1",
        "actor2", "actor3", "actor4",
        "actor5", "imdb_url", "rt_url"
      )
    )
    # check var_inputs()
    expect_true(is.list(var_inputs()))
    # check plot()
    expect_equal(class(plot()), c("gg", "ggplot"))
    # view plot
    # print(plot())
  }
)

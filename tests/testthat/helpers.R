
log_maker <- function(size, missing = FALSE) {
  if (isTRUE(missing)) {
    sample(c(TRUE, FALSE, NA), size = size, replace = TRUE)
  } else {
    sample(c(TRUE, FALSE), size = size, replace = TRUE)
  }
}

int_maker <- function(size, missing = FALSE) {
  if (isTRUE(missing)) {
    indx <- sample(x = 1:as.integer(size),
      size = as.integer(size) - 1,
      replace = TRUE)
    samp_indx <- sample(indx, size = round(as.integer(size)*0.5), 0)
    int_raw <- rnorm(n = size, mean = 5, sd = 3)
    int_raw[samp_indx] <- NA_integer_
    int_vec <- as.vector(int_raw, mode = "integer")
    return(int_vec)
  } else {
    x <- rnorm(n = size, mean = 5, sd = 3)
    y <- as.vector(x, mode = "integer")
    int_vec <- sample(x = y,
      size = size, replace = TRUE)
    return(int_vec)
  }
}

dbl_maker <- function(size, missing = FALSE) {
  if (isTRUE(missing)) {
    indx <- sample(x = 1:as.integer(size),
      size = as.integer(size) - 1,
      replace = TRUE)
    samp_indx <- sample(indx, size = round(as.integer(size)*0.5), 0)
    dbl_raw <- rnorm(n = size, mean = 5, sd = 3)
    dbl_raw[samp_indx] <- NA_real_
    dbl_vec <- round(as.vector(dbl_raw, mode = "double"), digits = 3)
    return(dbl_vec)
  } else {
    x <- rnorm(n = size, mean = 5, sd = 3)
    y <- round(as.vector(x, mode = "double"), digits = 3)
    dbl_vec <- sample(x = y,
      size = size, replace = TRUE)
    return(dbl_vec)
  }
}

chr_maker <- function(size, missing = FALSE) {
  if (isTRUE(missing)) {
    x <- sample(state.name, size = size, replace = TRUE)
    nas <- rep(NA_character_, times = as.integer(size) - 1)
    chr_raw <- as.vector(c(x, nas), mode = "character")
    chr_vec <- sample(x = chr_raw, size = size, replace = TRUE)
    return(chr_vec)
  } else {
    chr_raw <- state.name[1:as.integer(size)]
    chr_vec <- sample(x = chr_raw, size = size, replace = TRUE)
    return(chr_vec)
  }
}

fct_maker <- function(size, missing = FALSE) {
  if (isTRUE(missing)) {
    x <- sample(state.name, size = size, replace = TRUE)
    nas <- rep(NA_character_, times = as.integer(size) - 1)
    chr_raw <- as.vector(c(x, nas), mode = "character")
    chr_vec <- sample(x = chr_raw, size = size, replace = TRUE)
    fct_vec <- factor(chr_vec, levels = unique(sort(chr_vec)))
    return(fct_vec)
  } else {
    chr_raw <- state.name[1:as.integer(size)]
    chr_vec <- sample(x = chr_raw, size = size, replace = TRUE)
    fct_vec <- factor(chr_vec, levels = unique(sort(chr_vec)))
    return(fct_vec)
  }
}

ord_maker <- function(size, missing = FALSE) {
  if (isTRUE(missing)) {
    x <- sample(state.name, size = size, replace = TRUE)
    nas <- rep(NA_character_, times = as.integer(size) - 1)
    chr_raw <- as.vector(c(x, nas), mode = "character")
    chr_vec <- sample(x = chr_raw, size = size, replace = TRUE)
    ord_vec <- factor(chr_vec,
                      levels = unique(sort(chr_vec)),
                      ordered = TRUE)
    return(ord_vec)
  } else {
    chr_raw <- state.name[1:as.integer(size)]
    chr_vec <- sample(x = chr_raw, size = size, replace = TRUE)
    ord_vec <- factor(chr_vec,
                      levels = unique(sort(chr_vec)),
                      ordered = TRUE)
    return(ord_vec)
  }
}

bin_maker <- function(type, size, missing = FALSE) {
  if (isTRUE(missing)) {
    switch(type,
      log = sample(x = c(TRUE, FALSE, NA), size = size, replace = TRUE),
      int = sample(x = c(0L, 1L, NA_integer_),  size = size, replace = TRUE),
      chr = sample(x = c("group A", "group B", NA_character_), size = size, replace = TRUE),
      fct = factor(sample(x = c("group A", "group B", NA_character_),
                   size = size, replace = TRUE),
                   levels = unique(sort(
                            sample(x = c("group A", "group B", NA_character_),
                              size = size, replace = TRUE)))),
      ord = factor(sample(x = c("group 1", "group 2", NA_character_),
                   size = size, replace = TRUE),
                   levels = unique(sort(
                            sample(x = c("group 1", "group 2", NA_character_),
                              size = size, replace = TRUE))),
                  ordered = TRUE),
    )
  } else {
    switch(type,
      log = sample(x = c(TRUE, FALSE), size = size, replace = TRUE),
      int = sample(x = c(0L, 1L),  size = size, replace = TRUE),
      chr = sample(x = c("group A", "group B"), size = size, replace = TRUE),
      fct = factor(sample(x = c("group A", "group B"),
                   size = size, replace = TRUE),
                   levels = unique(sort(
                            sample(x = c("group A", "group B"),
                              size = size, replace = TRUE)))),
      ord = factor(sample(x = c("group 1", "group 2"),
                   size = size, replace = TRUE),
                   levels = unique(sort(
                            sample(x = c("group 1", "group 2"),
                              size = size, replace = TRUE))),
                  ordered = TRUE),
    )
  }
}

facet_maker <- function(type, size, missing = FALSE) {
  if (isTRUE(missing)) {
    switch(type,
      chr = sample(x = c("group A", "group B", "group C",
                         "group D", NA_character_),
                   size = size, replace = TRUE),
      fct = factor(sample(x = c("group A", "group B",
                                "group C", "group D",
                                NA_character_),
                   size = size, replace = TRUE),
                   levels = unique(sort(
                            sample(x = c("group A", "group B",
                                         "group C", "group D",
                                          NA_character_),
                              size = size, replace = TRUE)))),
      ord = factor(sample(x = c("group 1", "group 2",
                                "group 3", "group 4",
                                NA_character_),
                   size = size, replace = TRUE),
                   levels = unique(sort(
                            sample(x = c("group 1", "group 2",
                                         "group 3", "group 4",
                                          NA_character_),
                              size = size, replace = TRUE))),
                  ordered = TRUE))
  } else {
    switch(type,
      chr = sample(x = c("group A", "group B",
                         "group C", "group D"),
                  size = size, replace = TRUE),
      fct = factor(sample(x = c("group A", "group B",
                                "group C", "group D"),
                   size = size, replace = TRUE),
                   levels = unique(sort(
                            sample(x = c("group A", "group B",
                                         "group C", "group D"),
                              size = size, replace = TRUE)))),
      ord = factor(sample(x = c("group 1", "group 2",
                                "group 3", "group 4"),
                   size = size, replace = TRUE),
                   levels = unique(sort(
                            sample(x = c("group 1", "group 2",
                                          "group 3", "group 4"),
                              size = size, replace = TRUE))),
                  ordered = TRUE),
    )
  }
}

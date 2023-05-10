
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
    x <- sample(paste("group ", LETTERS), size = size, replace = TRUE)
    nas <- rep(NA_character_, times = as.integer(size) - 1)
    chr_raw <- as.vector(c(x, nas), mode = "character")
    chr_vec <- sample(x = chr_raw, size = size, replace = TRUE)
    return(chr_vec)
  } else {
    chr_raw <- paste("group ", LETTERS)[1:as.integer(size)]
    chr_vec <- sample(x = chr_raw, size = size, replace = TRUE)
    return(chr_vec)
  }
}
# chr_maker(10)
# chr_maker(10, TRUE)

fct_maker <- function(size, lvls, ord = FALSE, missing = FALSE) {
  if (isTRUE(missing) & isTRUE(ord)) {
    levs <- paste0("group ", as.integer(1:lvls))
    x <- sample(levs, size = size, replace = TRUE)
    nas <- rep(NA_character_, times = as.integer(size) - 1)
    chr_raw <- as.vector(c(x, nas), mode = "character")
    chr_vec <- sample(x = chr_raw, size = size, replace = TRUE)
    fct_vec <- factor(chr_vec, levels = unique(sort(x)),
                ordered = TRUE)
  } else if (isFALSE(missing) & isTRUE(ord)) {
    levs <- paste0("group ", as.integer(1:lvls))
    chr_raw <- rep(levs, times = size)
    chr_vec <- sample(x = chr_raw, size = size, replace = TRUE)
    ord_levels <-  sort(unique(chr_vec))
    fct_vec <- factor(chr_vec, levels = ord_levels, ordered = TRUE)
  } else if (isTRUE(missing) & isFALSE(ord)) {
    levs <- paste0("group ", as.integer(1:lvls))
    x <- sample(levs, size = size, replace = TRUE)
    nas <- rep(NA_character_, times = as.integer(size) - 1)
    chr_raw <- as.vector(c(x, nas), mode = "character")
    chr_vec <- sample(x = chr_raw, size = size, replace = TRUE)
    fct_vec <- factor(chr_vec, levels = unique(sort(x)))
  } else {
    levs <- paste0("group ", as.integer(1:lvls))
    chr_raw <- rep(levs, times = size)
    chr_vec <- sample(x = chr_raw, size = size, replace = TRUE)
    fct_vec <- factor(chr_vec, levels = unique(sort(chr_vec)))
  }
  return(fct_vec)
}
# fct_maker(size = 10, lvls = 5, ord = TRUE, missing = TRUE)
# fct_maker(size = 10, lvls = 5, ord = TRUE, missing = FALSE)
# fct_maker(size = 10, lvls = 5, ord = FALSE, missing = TRUE)
# fct_maker(size = 10, lvls = 5, ord = FALSE, missing = FALSE)

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

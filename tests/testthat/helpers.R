log_maker <- function(size, missing = FALSE) {
  if (size <= 2 & isTRUE(missing)) {
    as.vector(c(TRUE, NA), mode = "logical")
  } else if (size <= 2 & isFALSE(missing)) {
    as.vector(c(TRUE, FALSE), mode = "logical")
  } else if (size > 2 & isTRUE(missing)) {
    rep(c(TRUE, FALSE, NA), length.out = size)
  } else if (size > 2 & isFALSE(missing)) {
    rep(c(TRUE, FALSE), length.out = size)
  }
}
# log_maker(2)
# log_maker(2, TRUE)
# log_maker(3)
# log_maker(3, TRUE)

int_maker <- function(size, missing = FALSE) {
  if (size < 3 & isTRUE(missing)) {
    int_raw <- c(as.integer(exp(size)), NA_real_)
    int_vec <- as.vector(int_raw, mode = "integer")
    return(int_vec)
  } else if (size < 3 & isFALSE(missing)) {
    int_raw <- seq.int(from = 1, to = as.integer(exp(size)), length.out = size)
    int_vec <- as.vector(int_raw, mode = "integer")
    return(int_vec)
  } else if (size >= 3 & isTRUE(missing)) {
    adj_size <- round(size * 0.66, digits = 0)
    int_seq <- seq.int(from = 1, to = as.integer(exp(size)), length.out = adj_size)
    int_raw <- rep(c(int_seq, NA_integer_), length.out = size)
    int_vec <- as.vector(int_raw, mode = "integer")
    return(int_vec)
  } else {
    int_raw <- seq.int(from = 1, to = as.integer(exp(size)), length.out = size)
    int_vec <- as.vector(int_raw, mode = "integer")
    return(int_vec)
  }
}
# int_maker(2)
# int_maker(2, TRUE)
# int_maker(3)
# int_maker(3, TRUE)
# int_maker(5, TRUE)
# int_maker(10, TRUE)

dbl_maker <- function(size, missing = FALSE) {
  if (size < 3 & isTRUE(missing)) {
    dbl_raw <- c(as.double(log(size)), NA_real_)
    dbl_vec <- round(as.vector(dbl_raw, mode = "double"), digits = 3)
    return(dbl_vec)
  } else if (size < 3 & isFALSE(missing)) {
    dbl_raw <- seq.int(from = 0.1, to = as.double(size / 2), length.out = size)
    dbl_vec <- round(as.vector(dbl_raw, mode = "double"), digits = 3)
    return(dbl_vec)
  } else if (size >= 3 & isTRUE(missing)) {
    adj_size <- round(size * 0.33, digits = 0)
    dbl_seq <- seq.int(from = 0.1, to = as.double(size / 2), length.out = adj_size)
    dbl_raw <- rep(c(dbl_seq, NA_real_), length.out = size)
    dbl_vec <- round(as.vector(dbl_raw, mode = "double"), digits = 3)
    return(dbl_vec)
  } else {
    dbl_raw <- seq.int(from = 0.1, to = as.double(size / 2), length.out = size)
    dbl_vec <- round(as.vector(dbl_raw, mode = "double"), digits = 3)
    return(dbl_vec)
  }
}
# dbl_maker(2)
# dbl_maker(2, TRUE)
# dbl_maker(3)
# dbl_maker(3, TRUE)
# dbl_maker(10)
# dbl_maker(10, TRUE)

chr_maker <- function(size, lvls, missing = FALSE) {
  if (size < lvls) {
    lvls <- size - 1
  }
  if (isTRUE(missing) & size < 3) {
    chr_vec <- as.vector(c("item: 1", NA_character_),
      mode = "character"
    )
  } else if (isFALSE(missing) & size < 3) {
    chr_vec <- as.vector(c("item: 1", "item: 2"),
      mode = "character"
    )
  } else if (isTRUE(missing) & size >= 3) {
    adj_size <- size - 1
    levs <- paste0("item:", as.integer(1:lvls))
    adj_chr <- rep(c(levs, NA_character_), length.out = adj_size)
    nas <- rep(adj_chr, length.out = size)
    chr_vec <- as.vector(c(nas), mode = "character")
  } else {
    levs <- paste0("item:", as.integer(1:lvls))
    chr_raw <- rep(levs, length.out = size)
    chr_vec <- as.vector(c(chr_raw), mode = "character")
  }
  return(chr_vec)
}
# chr_maker(size = 2, lvls = 2, TRUE)
# chr_maker(size = 2, lvls = 1, TRUE)
# chr_maker(size = 3, lvls = 3, FALSE)
# chr_maker(size = 3, lvls = 3, TRUE)
# chr_maker(size = 10, lvls = 6, TRUE)

fct_maker <- function(size, lvls, ord = FALSE, missing = FALSE) {
  if (size < lvls) {
    lvls <- size - 1
  }
  if (isTRUE(missing) & isTRUE(ord)) {
    levs <- paste0("level ", as.integer(1:lvls))
    nas <- rep(c(levs, NA_character_), length.out = size)
    chr_raw <- as.vector(c(nas), mode = "character")
    fct_vec <- factor(chr_raw,
      levels = unique(sort(chr_raw)),
      ordered = TRUE
    )
  } else if (isFALSE(missing) & isTRUE(ord)) {
    levs <- paste0("level ", as.integer(1:lvls))
    chr_raw <- rep(levs, length.out = size)
    ord_levels <- sort(unique(chr_raw))
    fct_vec <- factor(chr_raw, levels = ord_levels, ordered = TRUE)
  } else if (isTRUE(missing) & isFALSE(ord)) {
    levs <- paste0("group ", as.integer(1:lvls))
    nas <- rep(c(levs, NA_character_), length.out = 10)
    chr_raw <- as.vector(c(nas), mode = "character")
    fct_vec <- factor(chr_raw, levels = unique(sort(chr_raw)))
  } else {
    levs <- paste0("group ", as.integer(1:lvls))
    chr_raw <- rep(levs, length.out = size)
    fct_levels <- unique(chr_raw)
    fct_vec <- factor(chr_raw, levels = fct_levels)
  }
  return(fct_vec)
}
# fct_maker(size = 5, lvls = 6, ord = TRUE, missing = TRUE)
# fct_maker(size = 10, lvls = 5, ord = TRUE, missing = FALSE)
# fct_maker(size = 10, lvls = 5, ord = FALSE, missing = TRUE)
# fct_maker(size = 10, lvls = 5, ord = FALSE, missing = FALSE)

bin_maker <- function(type, size, missing = FALSE) {
  if (size < 3) {
    size <- 3
    cli::cli_alert_info("size must be >= 3")
  }
  if (isTRUE(missing)) {
    switch(type,
      log = rep(x = c(TRUE, FALSE, NA), length.out = size),
      int = rep(x = c(0L, 1L, NA_integer_), length.out = size),
      chr = rep(
        x = c("item:A", "item:B", NA_character_),
        length.out = size
      ),
      fct = factor(
        rep(
          x = c("group A", "group B", NA_character_),
          length.out = size
        ),
        levels = unique(sort(
          rep(
            x = c("group A", "group B", NA_character_),
            length.out = size
          )
        ))
      ),
      ord = factor(
        rep(
          x = c("level 1", "level 2", NA_character_),
          length.out = size
        ),
        levels = unique(sort(
          rep(
            x = c("level 1", "level 2", NA_character_),
            length.out = size
          )
        )),
        ordered = TRUE
      )
    )
  } else {
    switch(type,
      log = rep(x = c(TRUE, FALSE), length.out = size),
      int = rep(x = c(0L, 1L), length.out = size),
      chr = rep(x = c("item:A", "item:B"), length.out = size),
      fct = factor(rep(x = c("group A", "group B"), length.out = size),
        levels = unique(sort(
          rep(
            x = c("group A", "group B"),
            length.out = size
          )
        ))
      ),
      ord = factor(
        rep(
          x = c("level 1", "level 2"),
          length.out = size
        ),
        levels = unique(sort(
          rep(
            x = c("level 1", "level 2"),
            length.out = size
          )
        )),
        ordered = TRUE
      )
    )
  }
}
# bin_maker(type = "log", size = 2)
# bin_maker(type = "log", size = 2, missing = TRUE)
# bin_maker(type = "int", size = 10)
# bin_maker(type = "int", size = 10, missing = TRUE)
# bin_maker(type = "chr", size = 10)
# bin_maker(type = "chr", size = 10, missing = TRUE)
# bin_maker(type = "fct", size = 10)
# bin_maker(type = "fct", size = 10, missing = TRUE)
# bin_maker(type = "ord", size = 10)
# bin_maker(type = "ord", size = 10, missing = TRUE)

facet_maker <- function(type, size, lvls, missing = FALSE) {
  if (isTRUE(missing)) {
    switch(type,
      chr = chr_maker(size = size, lvls = lvls, missing = TRUE),
      fct = fct_maker(size = size, lvls = lvls, missing = TRUE),
      ord = fct_maker(size = size, lvls = lvls, ord = TRUE, missing = TRUE)
    )
  } else {
    switch(type,
      chr = chr_maker(size = size, lvls = lvls, missing = FALSE),
      fct = fct_maker(size = size, lvls = lvls, missing = FALSE),
      ord = fct_maker(size = size, lvls = lvls, ord = TRUE, missing = FALSE)
    )
  }
}
# facet_maker(type = "chr", size = 10, lvls = 4, missing = TRUE)
# facet_maker(type = "fct", size = 10, lvls = 4, missing = TRUE)
# facet_maker(type = "ord", size = 6, lvls = 5, missing = FALSE)



mm_standardization <- function (data,
                                method,
                                margin,
                                range_global,
                                logbase = 2,
                                na.rm = FALSE,
                                make_matrix = FALSE,
                                site_column,
                                species_column,
                                values_from,
                                values_fill = 0,
                                ...) {

  col_is_chr_list <- list()
  for (column in colnames(data)) {
    is_chr_ <- is.character(data[, which(colnames(data) == column)])
    col_is_chr_list[[column]] <- is_chr_
  }
  col_is_chr_list <- unlist(col_is_chr_list)
  if(any(col_is_chr_list)){
    stop(sprintf("'%s' is not numeric column", colnames(data)[col_is_chr_list]))
  }

  if (make_matrix) {



  }
  wasDataFrame <- is.data.frame(data)
  data <- as.matrix(data)
  METHODS <- c("total", "max", "frequency", "normalize", "range",
               "rank", "rrank", "standardize", "pa", "chi.square", "hellinger",
               "log", "clr", "rclr", "alr")
  method <- match.arg(method, METHODS)
  if (any(data < 0, na.rm = TRUE)) {
    k <- min(data, na.rm = TRUE)
    if (method %in% c("total", "frequency", "pa", "chi.square",
                      "rank", "rrank", "rclr")) {
      warning("input data contains negative entries: result may be non-sense")
    }
  }
  else k <- .Machine$double.eps
  attr <- NULL
  switch(method, total = {
    if (missing(margin)) margin <- 1
    tmp <- pmax.int(k, apply(data, margin, sum, na.rm = na.rm))
    data <- sweep(data, margin, tmp, "/")
    attr <- list(total = tmp, margin = margin)
  }, max = {
    if (missing(margin)) margin <- 2
    tmp <- pmax.int(k, apply(data, margin, max, na.rm = na.rm))
    data <- sweep(data, margin, tmp, "/")
    attr <- list(max = tmp, margin = margin)
  }, frequency = {
    if (missing(margin)) margin <- 2
    tmp <- pmax.int(k, apply(data, margin, sum, na.rm = na.rm))
    fre <- apply(data > 0, margin, sum, na.rm = na.rm)
    tmp <- fre/tmp
    data <- sweep(data, margin, tmp, "*")
    attr <- list(scale = tmp, margin = margin)
  }, normalize = {
    if (missing(margin)) margin <- 1
    tmp <- apply(data^2, margin, sum, na.rm = na.rm)
    tmp <- pmax.int(.Machine$double.eps, sqrt(tmp))
    data <- sweep(data, margin, tmp, "/")
    attr <- list(norm = tmp, margin = margin)
  }, range = {
    if (missing(margin)) margin <- 2
    if (missing(range_global)) xtmp <- data else {
      if (dim(range_global)[margin] != dim(data)[margin]) stop("range matrix does not match data matrix")
      xtmp <- as.matrix(range_global)
    }
    tmp <- apply(xtmp, margin, min, na.rm = na.rm)
    ran <- apply(xtmp, margin, max, na.rm = na.rm)
    ran <- ran - tmp
    ran <- pmax.int(k, ran, na.rm = na.rm)
    data <- sweep(data, margin, tmp, "-")
    data <- sweep(data, margin, ran, "/")
    attr <- list(min = tmp, range = ran, margin = margin)
  }, rank = {
    if (missing(margin)) margin <- 1
    data[data == 0] <- NA
    data <- apply(data, margin, rank, na.last = "keep")
    if (margin == 1) data <- t(data)
    data[is.na(data)] <- 0
    attr <- list(margin = margin)
  }, rrank = {
    if (missing(margin)) margin <- 1
    data <- decostand(data, "rank", margin = margin)
    data <- sweep(data, margin, specnumber(data, margin = margin),
               "/")
    attr <- list(margin = margin)
  }, standardize = {
    if (!missing(margin) && margin == 1) data <- t(scale(t(data))) else {
      data <- scale(data)
      margin <- 2
    }
    attr <- list(center = attr(data, "scaled:center"),
                 scale = attr(data, "scaled:scale"),
                 margin = margin)
  }, pa = {
    data <- ifelse(data > 0, 1, 0)
  }, chi.square = {
    if (missing(margin)) margin <- 1
    if (margin == 2) data <- t(data)
    rs <- pmax.int(k, rowSums(data, na.rm = na.rm))
    cs <- pmax.int(k, colSums(data, na.rm = na.rm))
    tot <- sum(data, na.rm = na.rm)
    data <- sqrt(tot) * data/outer(rs, sqrt(cs))
    attr <- list(tot = tot, rsum = rs, csum = cs, margin = margin)
  }, hellinger = {
    data <- sqrt(decostand(data, "total", margin = margin, na.rm = na.rm))
    attr <- attr(data, "parameters")
  }, log = {
    if (!isTRUE(all.equal(as.integer(data), as.vector(data)))) {
      minpos <- min(data[data > 0], na.rm = TRUE)
      data <- data/minpos
      warning("non-integer data: divided by smallest positive value",
              call. = FALSE)
    } else {
      minpos <- 1
    }
    data[data > 0 & !is.na(data)] <- log(data[data > 0 & !is.na(data)], base = logbase) +
      1
    attr <- list(logbase = logbase, minpos = minpos)
  }, alr = {
    if (missing(margin)) margin <- 1
    if (margin == 1) data <- t(.calc_alr(t(data), ...)) else data <- .calc_alr(data,
                                                                      ...)
    attr <- attr(data, "parameters")
    attr$margin <- margin
  }, clr = {
    if (missing(margin)) margin <- 1
    if (margin == 1) data <- .calc_clr(data, ...) else data <- t(.calc_clr(t(data),
                                                                  ...))
    attr <- attr(data, "parameters")
    attr$margin <- margin
  }, rclr = {
    if (missing(margin)) margin <- 1
    if (margin == 1) data <- .calc_rclr(data, ...) else data <- t(.calc_rclr(t(data),
                                                                    ...))
    attr <- attr(data, "parameters")
    attr$margin <- margin
  })
  if (any(is.nan(data)))
    warning("Result contains NaN, perhaps due to impossible mathematical\n                 operation\n")
  if (wasDataFrame)
    data <- as.data.frame(data)
  attr(data, "parameters") <- attr
  attr(data, "decostand") <- method

  return(data)
}


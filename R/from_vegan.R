# From vegan package

.calc_alr <- function (x, reference = 1, pseudocount = 0, na.rm = TRUE)
{
  x <- x + pseudocount
  if (any(x < 0, na.rm = na.rm)) {
    rlang::abort(sprintf("'alr' cannot be used with negative data: use pseudocount >= %s",
                         -min(x, na.rm = na.rm) + pseudocount), call = NULL)
  }
  if (is.character(reference)) {
    reference <- which(reference == colnames(x))
    if (!length(reference))
      rlang::abort("'reference' name was not found in data", call = NULL)
  }
  if (reference > ncol(x) || reference < 1)
    rlang::abort(sprintf("'reference' should be a name or index 1 to %s", ncol(x)),
         call = NULL)
  clog <- log(x)
  refvector <- clog[, reference]
  clog <- clog[, -reference] - refvector
  attr(clog, "parameters") <- list(reference = refvector, index = reference,
                                   pseudocount = pseudocount)
  clog
}


.calc_clr <- function (x, pseudocount = 0, na.rm = TRUE)
{
  x <- x + pseudocount
  if (any(x <= 0, na.rm = na.rm)) {
    rlang::abort(sprintf("'clr' cannot be used with non-positive data: use pseudocount > %s",
                         -min(x, na.rm = na.rm) + pseudocount), call = NULL)
  }
  clog <- log(x)
  means <- rowMeans(clog, na.rm = na.rm)
  clog <- clog - means
  attr(clog, "parameters") <- list(means = means, pseudocount = pseudocount)
  clog
}


.calc_rclr <- function (x, na.rm = TRUE)
{
  if (any(x < 0, na.rm = na.rm)) {
    rlang::abort("'rclr' cannot be used with negative data", call = NULL)
  }
  clog <- log(x)
  clog[is.infinite(clog)] <- NA
  mean_clog <- rowMeans(clog, na.rm = na.rm)
  xx <- log(x) - mean_clog
  xx[is.infinite(xx)] <- 0
  attr(xx, "parameters") <- list(means = mean_clog)
  xx
}


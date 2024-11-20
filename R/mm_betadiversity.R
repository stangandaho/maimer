#' @noRd
veganMahatrans <- function (x, s2, tol = sqrt(.Machine$double.eps))
{
  if (missing(s2))
    s2 <- cov(x)
  e <- eigen(s2, symmetric = TRUE)
  k <- e$values > max(tol, tol * e$values[1L])
  sisqr <- e$vectors[, k, drop = FALSE] %*% (sqrt(1/e$values[k]) *
                                               t(e$vectors[, k, drop = FALSE]))
  x %*% sisqr
}


mm_betadiversity <- function(x,
                             method = "bray",
                             binary = FALSE,
                             diag = FALSE,
                             upper = FALSE,
                             na.rm = FALSE,
                             ...) {
  {
    ZAP <- 1e-15
    if (!is.na(pmatch(method, "euclidian")))
      method <- "euclidean"
    METHODS <- c("manhattan", "euclidean", "canberra", "bray",
                 "kulczynski", "gower", "morisita", "horn", "mountford",
                 "jaccard", "raup", "binomial", "chao", "altGower", "cao",
                 "mahalanobis", "clark", "chisq", "chord", "hellinger",
                 "aitchison", "robust.aitchison")
    method <- pmatch(method, METHODS)
    inm <- METHODS[method]
    if (is.na(method))
      stop("Invalid distance method")
    if (method == -1)
      stop("Ambiguous distance method")
    x <- as.matrix(x)
    if (!na.rm && anyNA(x))
      stop("Missing values are not allowed with argument 'na.rm = FALSE'")
    if (!(is.numeric(x) || is.logical(x)))
      stop("Input data must be numeric")
    if (!method %in% c(1, 2, 6, 16, 18) && any(rowSums(x, na.rm = TRUE) ==
                                               0))
      warning("You have empty rows: their dissimilarities may be\n meaningless in method ",
              dQuote(inm))
    if (!method %in% c(1, 2, 3, 6, 16, 19, 20) && any(x < 0,
                                                      na.rm = TRUE))
      warning("Results may be meaningless because data have negative entries\n                 in method ",
              dQuote(inm))
    if (method %in% c(11, 18) && any(colSums(x) == 0))
      warning("Data have empty species which influence the results in\n  method ",
              dQuote(inm))
    if (method == 6)
      x <- mm_standardization(x, "range", 2, na.rm = TRUE, ...)
    if (method == 16)
      x <- veganMahatrans(scale(x, scale = FALSE))
    if (method == 18)
      x <- mm_standardization(x, "chi.square")
    if (method == 21)
      x <- mm_standardization(x, "clr", ...)
    if (method == 22)
      x <- mm_standardization(x, "rclr")
    if (binary)
      x <- mm_standardization(x, "pa")
    N <- nrow(x)
    if (method %in% c(7, 13, 15) && !identical(all.equal(x, round(x)),
                                               TRUE))
      warning("Results may be meaningless with non-integer data in method ",
              dQuote(inm))
    d <- .Call(do_vegdist, x, as.integer(method))
    d[d < ZAP] <- 0
    if (any(is.na(d)))
      warning("Missing values in results")
    attr(d, "maxdist") <- if (method %in% c(3, 4, 5, 7, 8, 10,
                                            11, 13, 17))
      1
    else if (method %in% c(19, 20))
      sqrt(2)
    else if (method == 9)
      log(2)
    else NA
    attr(d, "Size") <- N
    attr(d, "Labels") <- dimnames(x)[[1]]
    attr(d, "Diag") <- diag
    attr(d, "Upper") <- upper
    attr(d, "method") <- paste(if (binary)
      "binary ", METHODS[method], sep = "")
    attr(d, "call") <- match.call()
    class(d) <- "dist"

    return(d)
  }
}

#' Standardize community data matrix
#'
#' This function standardizes a given data matrix using different methods such as
#' total sum scaling, max normalization, frequency scaling, standardization,
#' presence-absence transformation, chi-square transformation,
#' Hellinger transformation, log transformation, and others.
#'
#' @param data A numeric matrix or data frame to be standardized.
#' @param method A character string specifying the standardization method (see details).
#' Available methods are:
#'   \itemize{
#'     \item `"total"`: Divides each entry by the total sum in the given margin.
#'     \item `"max"`: Divides each entry by the maximum value in the given margin.
#'     \item `"frequency"`: Frequency transformation.
#'     \item `"normalize"`: Normalization by Euclidean norm.
#'     \item `"range"`: Standardizes by range (min-max scaling).
#'     \item `"rank"`: Converts values to ranks.
#'     \item `"rrank"`: Relative rank transformation.
#'     \item `"standardize"`: Standardization (z-score normalization).
#'     \item `"pa"`: Presence-absence transformation (binary).
#'     \item `"chi.square"`: Chi-square standardization.
#'     \item `"hellinger"`: Hellinger transformation.
#'     \item `"log"`: Log transformation.
#'     \item `"clr"`: Centered log-ratio transformation.
#'     \item `"rclr"`: Robust centered log-ratio transformation.
#'     \item `"alr"`: Additive log-ratio transformation.
#'   }
#' @param margin An integer specifying the margin for standardization:
#'   \itemize{
#'     \item `1`: Rows
#'     \item `2`: Columns
#'   }
#' @param range_global A matrix specifying the range for standardization
#' (optional, used with `"range"` method).
#' @param logbase The base for logarithmic transformation (default is 2).
#' @param na.rm Logical. If `TRUE`, missing values (`NA`) are removed before calculations.
#' @param make_matrix Logical. If `TRUE`, converts the input data into a matrix.
#' @param site_column The column name that identifies site information
#' (used for matrix conversion).
#' @param species_column The column name that identifies species information
#' (used for matrix conversion).
#' @param values_from The column name containing values for transformation.
#' @param values_fill The value to use for missing data (default is `0`).
#' @param ... Additional arguments passed to transformation functions.
#'
#' @return A standardized matrix or tibble with attributes specifying the
#' transformation applied.
#'
#' @details
#' The function provides the following standardization methods for community data:
#'
#' \itemize{
#'   \item \code{"total"}: Divides by margin total (default \code{margin = 1}).
#'   \item \code{"max"}: Divides by margin maximum (default \code{margin = 2}).
#'   \item \code{"frequency"}: Divides by margin total and multiplies by the
#'         number of non-zero items, ensuring the average of non-zero entries is one
#'         (Oksanen 1983; default \code{margin = 2}).
#'   \item \code{"normalize"}: Scales data so that the sum of squares along the
#'         specified margin equals one (default \code{margin = 1}).
#'   \item \code{"range"}: Standardizes values into the range \code{[0,1]} (default \code{margin = 2}).
#'         If all values are constant, they will be transformed to 0.
#'   \item \code{"rank"}, \code{"rrank"}:
#'         \itemize{
#'           \item \code{"rank"} replaces abundance values by their increasing ranks, leaving zeros unchanged.
#'           \item \code{"rrank"} is similar but uses relative ranks with a maximum of 1 (default \code{margin = 1}).
#'         }
#'   \item \code{"standardize"}: Scales \code{x} to zero mean and unit variance (default \code{margin = 2}).
#'   \item \code{"pa"}: Converts \code{x} to presence/absence scale (0/1).
#'   \item \code{"chi.square"}: Divides by row sums and the square root of column sums,
#'         then adjusts for the square root of the matrix total (Legendre & Gallagher 2001).
#'         When used with Euclidean distance, the distances should be similar to Chi-square
#'         distances in correspondence analysis (default \code{margin = 1}).
#'   \item \code{"hellinger"}: Computes the square root of \code{method = "total"} (Legendre & Gallagher 2001).
#'   \item \code{"log"}: Logarithmic transformation suggested by Anderson et al. (2006):
#'         \deqn{\log_b (x) + 1}{log_b (x) + 1} for \eqn{x > 0}, where \eqn{b} is the base of the logarithm.
#'         Zeros remain unchanged. Higher bases give less weight to quantities and more to presences.
#'   \item \code{"alr"}: Additive log ratio (ALR) transformation (Aitchison 1986).
#'         Reduces skewness and compositional bias. Requires positive values; pseudocounts can be added.
#'         The transformation is defined as:
#'         \deqn{alr = [\log(x_1 / x_D), ..., \log(x_{D-1} / x_D)]}{alr = [log(x_i/x_D), ..., log(x_{-D}/x_D)]}
#'         where the denominator sample \eqn{x_D} can be chosen arbitrarily.
#'   \item \code{"clr"}: Centered log ratio (CLR) transformation (Aitchison 1986).
#'         Common in microbial ecology (Gloor et al. 2017). Only supports positive data;
#'         pseudocounts can be used to handle zeros. The transformation is defined as:
#'         \deqn{clr = \log(x / g(x)) = \log x - \log g(x)}{clr = log(x/g(x)) = log x - log g(x)}
#'         where \eqn{x} is a single value, and \eqn{g(x)} is the geometric mean of \eqn{x}.
#'   \item \code{"rclr"}: Robust CLR transformation. Unlike CLR, this method allows zeros
#'         without requiring pseudocounts. It divides values by the geometric mean of
#'         observed (non-zero) features, preserving zeros (Martino et al. 2019). The transformation is defined as:
#'         \deqn{rclr = \log(x / g(x > 0))}{rclr = log(x/g(x > 0))}
#'         where \eqn{x} is a single value, and \eqn{g(x > 0)} is the geometric mean
#'         of sample-wide values \eqn{x} that are positive (\eqn{x > 0}).
#' }
#'
#'   Standardization, as contrasted to transformation, means that the
#'   entries are transformed relative to other entries.
#'
#'   All methods have a default margin. \code{margin=1} means rows
#'   (sites in a normal data set) and \code{margin=2} means columns (species in a
#'                                                                                                                           normal data set).
#'
#'   Command \code{wisconsin} is a shortcut to common Wisconsin double
#'   standardization where species (\code{margin=2}) are first standardized
#'   by maxima (\code{max}) and then sites (\code{margin=1}) by
#'   site totals (\code{tot}).
#'
#'   Most standardization methods will give nonsense results with
#'   negative data entries that normally should not occur in the community
#'   data. If there are empty sites or species (or constant with
#'   \code{method =  "range"}), many standardization will change these into
#'   \code{NaN}.
#'
#'   Function \code{decobackstand} can be used to transform standardized
#'   data back to original. This is not possible for all standardization
#'   and may not be implemented to all cases where it would be
#'   possible. There are round-off errors and back-transformation is not
#'   exact, and it is wise not to overwrite the original data. With
#'   \code{zap=TRUE} original zeros should be exact.
#'
#'
#' @examples
#' # Example usage with sample data
#' cam_data <- read.csv(system.file('penessoulou_season1.csv', package = 'maimer'))
#' cam_data <- cam_data %>%
#'   mm_to_community(site_column = camera, species_column = species,
#'                   size_column = number, values_fill = 0)
#'
#' standardized_data <- mm_standardize(data = cam_data[, 2:11], method = "total")
#' standardized_data
#'
#'
#' @references
#' Aitchison, J. The Statistical Analysis of Compositional Data (1986).
#' London, UK: Chapman & Hall.
#'
#' Anderson, M.J., Ellingsen, K.E. & McArdle, B.H. (2006) Multivariate
#' dispersion as a measure of beta diversity. \emph{Ecology Letters}
#' \strong{9}, 683--693.
#'
#' Egozcue, J.J., Pawlowsky-Glahn, V., Mateu-Figueras, G.,
#' Barcel'o-Vidal, C. (2003) Isometric logratio transformations for
#' compositional data analysis. \emph{Mathematical Geology} \strong{35}, 279--300.
#'
#' Gloor, G.B., Macklaim, J.M., Pawlowsky-Glahn, V. & Egozcue, J.J. (2017)
#' Microbiome Datasets Are Compositional: And This Is Not Optional.
#' \emph{Frontiers in Microbiology} \strong{8}, 2224.
#'
#' Legendre, P. & Gallagher, E.D. (2001) Ecologically meaningful
#' transformations for ordination of species data. \emph{Oecologia}
#' \strong{129}, 271--280.
#'
#' Martino, C., Morton, J.T., Marotz, C.A., Thompson, L.R., Tripathi, A.,
#' Knight, R. & Zengler, K. (2019) A novel sparse compositional technique
#' reveals microbial perturbations. \emph{mSystems} \strong{4}, 1.
#'
#' Oksanen, J. (1983) Ordination of boreal heath-like vegetation with
#' principal component analysis, correspondence analysis and
#' multidimensional scaling. \emph{Vegetatio} \strong{52}, 181--189.
#'
#' @note This function is adapted from the `decostand` function in the `vegan` R package,
#' with modifications to improved handling.
#'
#' @export
mm_standardize <- function (data,
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
    rlang::abort(sprintf("'%s' is not numeric column", colnames(data)[col_is_chr_list]))
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
      rlang::warn("Input data contains negative entries: result may be non-sense")
    }
  }
  else k <- .Machine$double.eps
  attr <- NULL
  switch(method,
         total = {
           if (missing(margin)) margin <- 1
           tmp <- pmax.int(k, apply(data, margin, sum, na.rm = na.rm))
           data <- sweep(data, margin, tmp, "/")
           attr <- list(total = tmp, margin = margin)
           },

         max = {
           if (missing(margin)) margin <- 2
           tmp <- pmax.int(k, apply(data, margin, max, na.rm = na.rm))
           data <- sweep(data, margin, tmp, "/")
           attr <- list(max = tmp, margin = margin)
           },

         frequency = {
           if (missing(margin)) margin <- 2
           tmp <- pmax.int(k, apply(data, margin, sum, na.rm = na.rm))
           fre <- apply(data > 0, margin, sum, na.rm = na.rm)
           tmp <- fre/tmp
           data <- sweep(data, margin, tmp, "*")
           attr <- list(scale = tmp, margin = margin)
         },

         normalize = {
           if (missing(margin)) margin <- 1
           tmp <- apply(data^2, margin, sum, na.rm = na.rm)
           tmp <- pmax.int(.Machine$double.eps, sqrt(tmp))
           data <- sweep(data, margin, tmp, "/")
           attr <- list(norm = tmp, margin = margin)
         },

         range = {
           if (missing(margin)) margin <- 2
           if (missing(range_global)) xtmp <- data else {
             if (dim(range_global)[margin] != dim(data)[margin]) rlang::abort("range matrix does not match data matrix")
             xtmp <- as.matrix(range_global)
           }
           tmp <- apply(xtmp, margin, min, na.rm = na.rm)
           ran <- apply(xtmp, margin, max, na.rm = na.rm)
           ran <- ran - tmp
           ran <- pmax.int(k, ran, na.rm = na.rm)
           data <- sweep(data, margin, tmp, "-")
           data <- sweep(data, margin, ran, "/")
           attr <- list(min = tmp, range = ran, margin = margin)
         },

         rank = {
           if (missing(margin)) margin <- 1
           data[data == 0] <- NA
           data <- apply(data, margin, rank, na.last = "keep")
           if (margin == 1) data <- t(data)
           data[is.na(data)] <- 0
           attr <- list(margin = margin)
         },

         rrank = {
           if (missing(margin)) margin <- 1
           data <- decostand(data, "rank", margin = margin)
           data <- sweep(data, margin, specnumber(data, margin = margin),
                         "/")
           attr <- list(margin = margin)
         },

         standardize = {
           if (!missing(margin) && margin == 1) data <- t(scale(t(data))) else {
             data <- scale(data)
             margin <- 2
           }
           attr <- list(center = attr(data, "scaled:center"),
                        scale = attr(data, "scaled:scale"),
                        margin = margin)
         },

         pa = {
           data <- ifelse(data > 0, 1, 0)
         },

         chi.square = {
           if (missing(margin)) margin <- 1
           if (margin == 2) data <- t(data)
           rs <- pmax.int(k, rowSums(data, na.rm = na.rm))
           cs <- pmax.int(k, colSums(data, na.rm = na.rm))
           tot <- sum(data, na.rm = na.rm)
           data <- sqrt(tot) * data/outer(rs, sqrt(cs))
           attr <- list(tot = tot, rsum = rs, csum = cs, margin = margin)
         },

         hellinger = {
           data <- sqrt(decostand(data, "total", margin = margin, na.rm = na.rm))
           attr <- attr(data, "parameters")
         },

         log = {
           if (!isTRUE(all.equal(as.integer(data), as.vector(data)))) {
             minpos <- min(data[data > 0], na.rm = TRUE)
             data <- data/minpos
             rlang::warn("non-integer data: divided by smallest positive value")
           } else {
             minpos <- 1
           }
           data[data > 0 & !is.na(data)] <- log(data[data > 0 & !is.na(data)], base = logbase) +
             1
           attr <- list(logbase = logbase, minpos = minpos)
         },

         alr = {
           if (missing(margin)) margin <- 1
           if (margin == 1) data <- t(.calc_alr(t(data), ...)) else data <- .calc_alr(data,
                                                                                      ...)
           attr <- attr(data, "parameters")
           attr$margin <- margin
         },

         clr = {
           if (missing(margin)) margin <- 1
           if (margin == 1) data <- .calc_clr(data, ...) else data <- t(.calc_clr(t(data),
                                                                                  ...))
           attr <- attr(data, "parameters")
           attr$margin <- margin
         },

         rclr = {
           if (missing(margin)) margin <- 1
           if (margin == 1) data <- .calc_rclr(data, ...) else data <- t(.calc_rclr(t(data),
                                                                                    ...))
           attr <- attr(data, "parameters")
           attr$margin <- margin
         })

  if (any(is.nan(data)))
    rlang::warn("Result contains NaN, perhaps due to impossible mathematical operation")
  if (wasDataFrame)
    data <- dplyr::as_tibble(data)
  attr(data, "parameters") <- attr
  attr(data, "standardize") <- method

  return(data)
}


#' @title Calculate dissimilarity between communities
#'
#' @description
#' The function computes dissimilarity indices that are useful for or popular with
#' community ecologists. All indices use quantitative data, although they would be
#' named by the corresponding binary index, but you can calculate the binary index
#' using an appropriate argument. If you do not find your favourite index here, you
#' can see if it can be implemented using designdist. Gower, Bray–Curtis, Jaccard
#' and Kulczynski indices are good in detecting underlying ecological gradients
#' (Faith et al. 1987). Morisita, Horn–Morisita, Binomial, Cao and Chao indices
#' should be able to handle different sample sizes
#' (Wolda 1981, Krebs 1999, Anderson & Millar 2004), and Mountford (1962) and
#' Raup-Crick indices for presence–absence data should be able to handle unknown
#' (and variable) sample sizes. Most of these indices are discussed by Krebs (1999)
#' and Legendre & Legendre (2012), and their properties further compared by Wolda
#' (1981) and Legendre & De Cáceres (2012). Aitchison (1986) distance is equivalent
#' to Euclidean distance between CLR-transformed samples ("clr") and deals with
#' positive compositional data. Robust Aitchison distance by Martino et al. (2019)
#' uses robust CLR ("rlcr"), making it applicable to non-negative data including
#' zeroes (unlike the standard Aitchison).
#'
#' @param data A data frame or matrix containing the species abundance data. The
#'   rows represent sites (or samples), and the columns represent species.
#'   The data can be in raw or transformed format (if `to_community = TRUE`).
#' @param to_community A logical indicating whether the input data should be
#'   transformed into community data (site in row and species in column).
#'   Default is `FALSE`.
#' @param site_column The name of the column representing the site/sample identifiers
#'   (only used if `to_community = TRUE`).
#' @param species_column The name of the column representing species identifiers
#'   (only used if `to_community = TRUE`).
#' @param size_column The name of the column representing size or abundance counts
#'   of each species at each site (optional, used if `to_community = TRUE`).
#' @param method A character string indicating the distance measure to use for
#'   calculating beta diversity. The available methods are:
#'   \code{"manhattan"}, \code{"euclidean"}, \code{"canberra"}, \code{"bray"},
#'   \code{"kulczynski"}, \code{"gower"}, \code{"morisita"}, \code{"horn"},
#'   \code{"mountford"}, \code{"jaccard"}, \code{"raup"}, \code{"binomial"},
#'   \code{"chao"}, \code{"altGower"}, \code{"cao"}, \code{"mahalanobis"},
#'   \code{"clark"}, \code{"chisq"}, \code{"chord"}, \code{"hellinger"},
#'   \code{"aitchison"}, \code{"robust.aitchison"}. The default is \code{"bray"}.
#' @param binary A logical indicating whether to transform the data to presence/absence
#'   (binary data) before calculating dissimilarities. Default is `FALSE`.
#' @param diag A logical indicating whether to include the diagonal in the output
#'   dissimilarity matrix. Default is `FALSE` (diagonal values are omitted).
#' @param upper A logical indicating whether to return only the upper triangular
#'   part of the dissimilarity matrix. Default is `FALSE`.
#' @param na.rm A logical indicating whether to remove `NA` values from the data
#'   before calculating dissimilarities. Default is `FALSE`. If `FALSE`, an error is raised
#'   if there are any missing values in the data.
#' @param ... Additional arguments passed to other functions, such as transformation
#'   functions for data scaling or standardization.
#'
#' @return A distance matrix (of class `dist`) containing the pairwise dissimilarities
#'   between sites. The dissimilarities are calculated according to the chosen distance
#'   metric, and various attributes (e.g., method, size, labels) are attached to the
#'   result.
#'
#' @inherit vegan::vegdist details
#'
#' @inherit vegan::vegdist note
#'
#' @inherit vegan::vegdist author
#'
#' @inherit vegan::vegdist references
#'
#'
#' @export

mm_dissimilarity <- function(data,
                              to_community = FALSE,
                              site_column,
                              species_column,
                              size_column = NULL,
                              method = "bray",
                              binary = FALSE,
                              diag = FALSE,
                              upper = FALSE,
                              na.rm = FALSE,
                              ...){

  if (to_community) {

    site_column <- rlang::ensym(site_column)
    species_column <- rlang::enquo(species_column)
    size_column <- tryCatch({ rlang::ensym(size_column) }, error = function(e)NULL)

    if (!is.null(size_column)) {
      data <- transform_index_data(data = data,
                                   site_column = !!site_column,
                                   species_column = !!species_column,
                                   to_community = to_community,
                                   size_column = size_column
      )

    }else{
      data <- transform_index_data(data = data,
                                   site_column = !!site_column,
                                   species_column = !!species_column,
                                   to_community = to_community
      )
    }


    site_name <- data %>% dplyr::pull(site_column)
    data <- data %>% dplyr::select(-site_column) %>%
      as.matrix()
    rownames(data) <- site_name
  }else{
    data <- as.matrix(data)
  }


  ZAP <- 1e-15
  if (!is.na(pmatch(method, "euclidian"))){
    method <- "euclidean"
  }
  METHODS <- c("manhattan", "euclidean", "canberra", "bray",
               "kulczynski", "gower", "morisita", "horn", "mountford",
               "jaccard", "raup", "binomial", "chao", "altGower", "cao",
               "mahalanobis", "clark", "chisq", "chord", "hellinger",
               "aitchison", "robust.aitchison")
  method <- pmatch(method, METHODS)
  inm <- METHODS[method]
  if (is.na(method))
    rlang::abort("Invalid distance method", call = NULL)
  if (method == -1)
    rlang::abort("Ambiguous distance method", call = NULL)

  if (!na.rm && anyNA(data))
    rlang::abort("Missing values are not allowed if 'na.rm = FALSE'", call = NULL)
  if (!(is.numeric(data) || is.logical(data)))
    rlang::abort("Input data must be numeric", call = NULL)
  if (!method %in% c(1, 2, 6, 16, 18) && any(rowSums(data, na.rm = TRUE) == 0))
    rlang::warn(sprintf("Empty rows: their dissimilarities may be meaningless in method %s", dQuote(inm)))
  if (!method %in% c(1, 2, 3, 6, 16, 19, 20) && any(data < 0, na.rm = TRUE))
    rlang::warn(sprintf("Results may be meaningless because data have negative
                        entries in method %s", dQuote(inm)))
  if (method %in% c(11, 18) && any(colSums(data) == 0, na.rm = TRUE))
    rlang::warn(sprintf("Data have empty species which influence the results in method %s",
                        dQuote(inm)))
  if (method == 6)
    data <- mm_standardize(data, "range", 2, na.rm = TRUE, ...)
  if (method == 16)
    data <- veganMahatrans(scale(data, scale = FALSE), na.rm = na.rm)
  if (method == 18)
    data <- mm_standardize(data, "chi.square", na.rm = na.rm)
  if (method == 21)
    data <- mm_standardize(data, "clr", ...)
  if (method == 22)
    data <- mm_standardize(data, "rclr")
  if (binary)
    data <- mm_standardize(data, "pa")
  N <- nrow(data)
  if (method %in% c(7, 13, 15) && !identical(all.equal(data, round(data)), TRUE))
    rlang::warn(sprintf("Results may be meaningless with non-integer data
                        in method %s", dQuote(inm)))
  d <- .Call(vegan:::do_vegdist, data, as.integer(method))
  d[d < ZAP] <- 0
  if (any(is.na(d)))
    rlang::warn("Missing values in results")
  attr(d, "maxdist") <- if (method %in% c(3, 4, 5, 7, 8, 10, 11, 13, 17))
    1
  else if (method %in% c(19, 20))
    sqrt(2)
  else if (method == 9)
    log(2)
  else NA

  attr(d, "Size") <- N
  attr(d, "Labels") <- dimnames(data)[[1]]
  attr(d, "Diag") <- diag
  attr(d, "Upper") <- upper
  attr(d, "method") <- paste(if (binary)
    "binary ", METHODS[method], sep = "")
  attr(d, "call") <- match.call()
  class(d) <- "dist"


  return(d)
}

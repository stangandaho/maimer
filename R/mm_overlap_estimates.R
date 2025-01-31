#'Estimates of coefficient of overlapping
#' @inheritParams mm_plot_overlap
#' @inheritParams overlap::overlapEst
#'
#' @examples
#'
#'set.seed(42)
#' species_A <- runif(100, 1.2, 2 * pi)
#' species_B <- runif(100, 0.23, 2 * pi)
#' mm_overlap_estimates(species_A, species_B)
#' mm_overlap_estimates(species_A, species_B, type = "Dhat4")
#'
#'@export

mm_overlap_estimates <- function(A,
                                 B,
                                 kmax = 3,
                                 adjust=c(0.8, 1, 4),
                                 n_grid = 128,
                                 type=c("all", "Dhat1", "Dhat4", "Dhat5")
                                 ) {

  out <- overlap::overlapEst(A,
                             B,
                             kmax = 3,
                             adjust=c(0.8, 1, 4),
                             n.grid = n_grid,
                             type=type)

  return(out)
}

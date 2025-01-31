#'Estimates of coefficient of overlapping
#' @inheritParams mm_plot_overlap
#' @inheritParams overlap::overlapEst
#'
#' @examples
#'# Get example data:
#' data(simulatedData)
#'
#' # Use defaults:
#' mm_overlap_estimates(tigerObs, pigObs)
#' #     Dhat1     Dhat4     Dhat5
#' 0.2908618 0.2692011 0.2275000
#'
#' mm_overlap_estimates(tigerObs, pigObs, type="Dhat4")
#' #    Dhat4
#'#    0.2692011
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

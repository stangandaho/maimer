#' Pendjari National Park and Surrounding Areas
#'
#' A dataset containing spatial boundaries of Pendjari National Park and its surrounding hunting zones in Benin.
#'
#' @format A tibble with 3 rows and 2 columns:
#' \describe{
#'   \item{NOM}{The name of the protected area or hunting zone.}
#'   \item{geometry}{The spatial geometry of the area, stored in decimal degrees (EPSG:4326).}
#' }
#'
#' @details
#' The dataset includes the following areas:
#' \itemize{
#'   \item{Pendjari Hunting Zone}
#'   \item{Konkombri Hunting Zone}
#'   \item{Pendjari National Park}
#' }
#'
#' @examples
#' # Load the dataset
#' data("pendjari")
#'
#' # Plot the data
#' library(sf)
#' plot(pendjari, main = "Pendjari National Park and Surrounding Areas")
#' legend("topright", legend = pendjari$NAME, fill = c("gray10", "gray50", "gray90"))
#'
"pendjari"

#' Pendjari national park and surrounding areas
#'
#' A dataset containing spatial boundaries of Pendjari National Park and its surrounding hunting zones in Benin.
#'
#' @format A tibble with 3 rows and 2 columns:
#'   - `NOM`: The name of the protected area or hunting zone.
#'   - `geometry`: The spatial geometry of the area, stored in decimal degrees (EPSG:4326).
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


#' @title Camera trap data package example
#'
#' @description
#' Data and metadata from an example study exported from the Agouti camera trap
#' data management platform in camtrap-DP format. Metadata includes study name,
#' authors, location and other details. Data is held in element data, itself a
#' list holding dataframes deployments, media and observations.
#' See [https://tdwg.github.io/camtrap-dp](https://camtrap-dp.tdwg.org/) for details.
#'
#' @author Marcus Rowcliffe
#'
#' @format A list holding study data and metadata.
"camtrapdp"

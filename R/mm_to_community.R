#' Convert Data to a Community Matrix
#'
#' The function transforms input data into a community matrix where
#' rows represent sites, columns represent species, and values indicate the count
#' or abundance of each species at each site.
#'
#' @param data A data frame containing the input data.
#' @param site_column The column in the data frame representing site identifiers. Can be specified as a string or unquoted column name.
#' @param species_column The column in the data frame representing species identifiers. Can be specified as a string or unquoted column name.
#' @param size_column (Optional) The column representing the size or abundance of the species at each site. If not provided, counts of species occurrences are calculated.
#' @param values_fill (Optional) A value to fill missing cells in the resulting community matrix. Defaults to `NULL`.
#'
#' @details
#' The function creates a site-by-species matrix suitable for ecological analysis.
#' If `size_column` is not provided, the function counts occurrences of each
#' species per site. If `size_column` is provided, its values are used as the
#' measure for species abundance.
#'
#' @return A tibble where rows represent sites, columns represent species, and values represent the count or abundance of each species.
#'
#' @examples
#' # Example data
#' df <- dplyr::tibble(
#'   site = c("A", "A", "B", "B", "C"),
#'   species = c("sp1", "sp2", "sp1", "sp3", "sp2"),
#'   abundance = c(5, 2, 3, 1, 4)
#' )
#'
#' # Convert to community matrix with counts
#' mm_to_community(df, site_column = site, species_column = species)
#'
#' # Convert to community matrix with abundance
#' mm_to_community(df, site_column = site, species_column = species, size_column = abundance)
#'
#' # Fill missing cells with 0
#' mm_to_community(df, site_column = site, species_column = species, values_fill = 0)
#'
#' @import dplyr
#' @import tidyr
#' @export

mm_to_community <- function(data,
                         site_column,
                         species_column,
                         size_column,
                         values_fill = NULL){

  if (!hasArg(site_column) | !hasArg(species_column)) {
    has_ <- c(hasArg(site_column), hasArg(species_column))
    agr_name <- c("site_column", "species_column")[!has_]
    stop(sprintf("'%s' must be provided if make_matrix is TRUE",
                 paste0(agr_name, collapse = " ,")))

  }

  # Convert site_column and species_column to symbols, handling both quoted and unquoted input
  site_column <- dplyr::ensym(site_column)
  species_column <- dplyr::ensym(species_column)

  # If size_column is provided, capture it with enquo; otherwise, leave it as NULL
  if (!hasArg(size_column)) {
    data <- data %>%
      dplyr::count(!!site_column, !!species_column) %>%
      tidyr::pivot_wider(id_cols = !!site_column,
                         names_from = !!species_column,
                         values_from = n,
                         values_fill = values_fill)
  } else {
    size_column <- dplyr::ensym(size_column)
    print(size_column)
    data <- data %>%
      tidyr::pivot_wider(id_cols = !!site_column,
        names_from = !!species_column,
        values_from = !!size_column,
        values_fill = values_fill,
        values_fn = sum)
  }

  return(data)
}

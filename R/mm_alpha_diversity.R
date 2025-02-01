#' @title Alpha diversity index
#'
#' @description
#' Calculate index diversity within a particular area or ecosystem;
#' usually expressed by the number of species (i.e., species richness)
#' in that ecosystem.
#'
#' @param data A data frame containing species observation data.
#' @param site_column The column name in `data` that represents the site or location where species were recorded.
#' @param index A character vector specifying the diversity index to calculate.
#' Accepted values are `"shannon"`, `"simpson"`, `"invsimpson"`, `"evenness"`, and `"pielou"`.
#' Multiple indices can be computed simultaneously by providing a vector.
#' @param species_column The column(s) in `data` representing species or taxa.
#' This can be a single column name, a range of column indices (e.g., `2:5`),
#' or a selection helper (e.g., `dplyr::starts_with("sp_")`).
#' @param to_community Logical; if `TRUE`, the function first transforms `data`
#' into a community matrix format where sites are rows and species are columns
#' before computing indices. Default is `TRUE`.
#' @param size_column (Optional) The column in `data` containing the count or abundance of individuals per species.
#' If `NULL`, the function assumes each row represents one individual.
#' @param margin An integer specifying whether diversity calculations should be
#' performed by row (`margin = 1`) or by column (`margin = 2`). Default is `1` (row-wise).
#'
#'
#' @return A tibble with diversity index values for each site.
#' The first column corresponds to `site_column`, followed by one or more columns containing
#' the computed diversity indices, depending on the values specified in the `index` argument.
#'
#'
#' @details
#' **Simpson diversity index**
#'
#' Simpson (1949) introduced a diversity index that quantifies the likelihood
#' of two randomly chosen individuals belonging to the same species.
#' This probability increases as diversity decreases; in a scenario with
#' no diversity (only one species), the probability reaches 1.
#' Simpson's Index is computed using the following formula:
#'
#' \deqn{D = \sum_{i=1}^{S} \left( \frac{n_{i}}{N} \right)^2}
#'
#' where \eqn{n_{i}} is the number of individuals in species *i*,
#' N = total number of individuals of all species, and \eqn{\frac{n_{i}}{N} = pi}
#' (proportion of individuals of species *i*), and S = species richness.
#' The value of Simpson’s *D* ranges from 0 to 1, with 0 representing infinite
#' diversity and 1 representing no diversity, so the larger the value of D,
#' the lower the diversity. For this reason, Simpson’s index is often as its
#' complement (*1-D*). Simpson's Dominance Index is the inverse of the
#' Simpson's Index (\eqn{1/D}).
#'
#'
#' **Shannon-Weiner Diversity Index**
#'
#' Shannon-Weiner Diversity Index is a measure of diversity that takes into
#' account both species richness and evenness, introduced by Claude Shannon in 1948.
#' Commonly referred to as Shannon's Diversity Index, it is based on the concept
#' of uncertainty. For instance, in a community with very low diversity, there is a high level of
#' certainty (or low uncertainty) about the identity of a randomly
#' selected organism. Conversely, in a highly diverse community, the
#' uncertainty increases, making it harder to predict which species a
#' randomly chosen organism will belong to (low certainty or high uncertainty).
#'
#'
#' \deqn{H = -\sum_{i=1}^{S} p_{i} * \ln p_{i}}
#'
#'
#' where \eqn{p_{i}} = proportion of individuals of species *i*, and *ln* is the natural logarithm,
#' and  S = species richness. The value of H ranges from 0 to Hmax.
#' Hmax is different for each community and depends on species richness.
#' (Note: Shannon-Weiner is often denoted H' ).
#'
#'
#' **Pielou or Evenness diversity index**
#'
#' Species evenness refers to the relative abundance of each species within an
#' environment. For example, if there are 40 foxes and 1000 dogs, the community
#' is uneven because one species dominates. However, if there are 40 foxes and
#' 42 dogs, the community is much more even, as the species are more balanced
#' in number. The degree of evenness in a community can be quantified using
#' Pielou's evenness index (Pielou, 1966):
#'
#' \deqn{J=\frac{H}{H_{\max }}}
#'
#' The value of J ranges from 0 to 1. Higher values indicate higher levels of
#' evenness. At maximum evenness, J = 1. J and D can be used as measures of
#' species dominance (the opposite of diversity) in a community.
#' Low J indicates that 1 or few species dominate the community.
#'
#' @references
#' Pielou, E.C. (1966). The measurement of diversity in different types of
#' biological collections. Journal of Theoretical Biology, 13, pp. 131–144.
#' [doi:10.1016/0022-5193(66)90013-0](doi:10.1016/0022-5193(66)90013-0).
#'
#' Simpson, E.H. (1949). Measurement of diversity. Nature, 163, pp. 688.
#' [doi:10.1038/163688a0](doi:10.1038/163688a0)
#'
#' Shannon, C.E. (1948). A mathematical theory of communication. The Bell System
#' Technical Journal, 27, pp. 379-423.[https://doi.org/10.1002/j.1538-7305.1948.tb01338.x](
#' https://doi.org/10.1002/j.1538-7305.1948.tb01338.x)
#'
#' @examples
#' cam_data <- read.csv(system.file('penessoulou_season1.csv', package = 'maimer'))
#'
#' # Transform data to community format and compute diversity indices
#' alpha1 <- cam_data %>%
#'   mm_alpha_diversity(
#'     to_community = TRUE,
#'     size_column = number,
#'     site_column = camera,
#'     species_column = species,
#'     index = c("shannon", "evenness", "invsimpson")
#'   )
#'
#' # Alternative method using a manually transformed community matrix
#' alpha2 <- cam_data %>%
#'   mm_to_community(site_column = camera, species_column = species,
#'                   size_column = number, values_fill = 0) %>%
#'   mm_alpha_diversity(
#'     to_community = FALSE,
#'     site_column = camera,
#'     species_column = 2:11,
#'     index = c("shannon", "evenness", "invsimpson")
#'   )
#' alpha2
#' # Compare results
#' all(alpha1 == alpha2) # TRUE
#'
#' @export
#'
mm_alpha_diversity <- function(data,
                             site_column,
                             index = "shannon",
                             species_column,
                             to_community = TRUE,
                             size_column = NULL,
                             margin = 1) {

  site_column <- as.character(rlang::ensym(site_column))
  species_column <- rlang::enquo(species_column)


  size_column <- tryCatch({ rlang::ensym(size_column) }, error = function(e)NULL)

  INDEX <- c("shannon", "simpson", "invsimpson", "evenness", "pielou")
  if (!all(index %in% INDEX)) {
    msg <- sprintf("Accepted index are: %s", paste0(INDEX, collapse = ", "))
    rlang::abort(msg, call = NULL)
  }


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
  data <- data %>% dplyr::select(-site_column)
  index_list <- list()

  for (i in index) {

    if (i == "simpson") {
      # simpson
      simpson = apply(data, margin, function(x){
        round(sum((x/sum(x, na.rm = TRUE))**2), 3)
      })
      ind_value <- 1 - simpson
      index_list[[i]] <- ind_value

    }else if(i == "invsimpson"){
      # simpson inverse
      simpson = apply(data, margin, function(x){
        round(sum((x/sum(x, na.rm = TRUE))**2), 3)
      })
      ind_value <- 1/simpson
      index_list[[i]] <- ind_value

    }else if (i == "shannon") {
      # shannon
      ind_value <- apply(data, margin, function(x){
        pi <- (x/sum(x, na.rm = TRUE))
        -sum(pi*log(pi), na.rm = TRUE)
      })
      index_list[[i]] <- ind_value

    }else if (i %in% c("evenness", "pielou")) {
      shannon <- apply(data, margin, function(x){
        pi <- (x/sum(x, na.rm = TRUE))
        -sum(pi*log(pi), na.rm = TRUE)
      })
      ind_value <- shannon/log(ncol(data))
      index_list[[i]] <- ind_value
    }
  }


 out_tbl <- dplyr::bind_cols(dplyr::tibble(!!site_column := site_name),
                             dplyr::bind_cols(index_list))

 return(out_tbl)

}


##
transform_index_data <- function(data,
                             site_column,
                             species_column,
                             to_community = TRUE,
                             size_column = NULL) {


  site_column <- rlang::enquo(site_column)
  species_column <- rlang::enquo(species_column)


  if (to_community) {
    if (!is.null(size_column)) {
      size_column <- dplyr::ensym(size_column)

      data <- data %>%
        mm_to_community(site_column = !!site_column,
                        species_column = !!species_column,
                        size_column = !!size_column, values_fill = 0)
    }else{

      data <-data %>%
        mm_to_community(site_column = !!site_column,
                        species_column = !!species_column,
                        values_fill = 0)
    }


  }else{
    data <- data %>%
      dplyr::select(!!site_column, !!species_column)
  }

  return(data)
}

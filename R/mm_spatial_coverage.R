#' Calculate Observed Spatial Coverage of Species
#'
#' This function calculates the *Observed Spatial Coverage* of a species like *Home Range*,
#' but based on camera trap data.
#' The term home range is typically associated with dynamic movement data, such as
#' those recorded by radio-tracking or GPS devices, which provide continuous or
#' near-continuous tracking of an individual animal's movements. Since camera traps
#' are static and only capture presence/absence or activity within their specific
#' locations, the concept of home range might not fully apply.
#'
#' @param data A data frame containing species occurrence records, including site, longitude, latitude, and optionally size (abundance).
#' @param site_column Column name specifying the site identifier.
#' @param size_column Optional column specifying the abundance of species at each site.
#' Defaults to NULL, in which case counts per site are used.
#' @param longitude Column name specifying the longitude of observation sites.
#' @param latitude Column name specifying the latitude of observation sites.
#' @param crs A vector of length two specifying the coordinate reference systems: `c(crs1, crs2)`.
#'  - `crs1` represents the current CRS of the data (e.g., 4326 for latitude/longitude).
#'  - `crs2` represents the CRS to transform into (e.g., "EPSG:32631", a UTM EPSG code) for accurate distance calculations.
#'  If `crs2` is NULL, no transformation is applied. Defaults to `c(4326, NULL)`
#' @param study_area An optional simple feature (sf) polygon representing the study area. If provided, the raster extends to cover the area.
#' @param resolution Numeric value specifying the spatial resolution (grid size) for rasterization.
#' @param spread_factor A scale factor for the half-normal distribution.
#' Higher values create a more spread-out distribution, while lower values make
#' it more concentrated. The value must be in ]0; 1]
#'
#' @details The function applies a half-normal kernel to model species abundance over space,
#' using the scale rate to control the spread of the distribution:
#'
#' \deqn{\bar{K}(x) = \frac{\sum w * \text{e}^{(-0.5 * (\frac{x}{\sigma})^2)}}{N}}
#'
#' where:
#' - \eqn{\bar{K}(x)} is the mean abundance kernel across all sites,
#' - \eqn{w} is the species abundance at each site,
#' - \eqn{\sigma} is the standard deviation of the spatial distance (scaled by spread_factor),
#' - \eqn{N} is the total number of sites.
#'
#'
#' @return A list containing:
#' - **Coverage raster**: A raster object representing species abundance across space.
#' - **Coverage stats**: A tibble with spatial coverage statistics, including area (km²), average abundance, maximum abundance, and standard deviation.
#'
#' @examples
#' library(dplyr)
#' cam_data <- system.file("penessoulou_season2.csv", package = "maimer") %>%
#'   read.csv() %>%
#'   dplyr::filter(Species == "Erythrocebus patas")
#'
#' spc <- mm_spatial_coverage(
#'   data = cam_data,
#'   site_column = Camera,
#'   crs = "EPSG:32631", ,
#'   resolution = 30,
#'   spread_factor = 0.4,
#'   size_column = Count,
#'   longitude = Longitude,
#'   latitude = Latitude
#' )
#'
#' ## Abundance stats
#' spc[[2]] %>%
#'   dplyr::select(-1)
#'
#' ## Plot spatial coverage
#' library(ggplot2)
#' spc_vect <- terra::as.polygons(spc[[1]]) %>%
#'   sf::st_as_sf()
#'
#' ggplot() +
#'   geom_sf(data = spc_vect, aes(fill = Abundance), color = NA) +
#'   theme_minimal() +
#'   scale_fill_viridis_c()
#'
#' @export


mm_spatial_coverage <- function(data,
                                site_column,
                                size_column = NULL,
                                longitude,
                                latitude,
                                crs = c(4326, NULL),
                                study_area = NULL,
                                resolution,
                                spread_factor = .1) {


  # Coerce column to tidy evaluation
  site_column <- tryCatch(rlang::enquo(site_column), error = function(e)NULL)
  size_column <- tryCatch(rlang::enquo(size_column), error = function(e)NULL)
  longitude <- tryCatch(rlang::enquo(longitude), error = function(e)NULL)
  latitude <- tryCatch(rlang::enquo(latitude), error = function(e)NULL)

  if (is.null(site_column)) {rlang::abort("Site column must be provided.")}
  if (is.null(longitude)) {rlang::abort("Longitude column is required.")}
  if (is.null(latitude)) {rlang::abort("Latitude column is required.")}

  # Remove NA coordinates
  data <- data %>%
    dplyr::filter(!is.na(!!longitude), !is.na(!!latitude)) %>%
    dplyr::group_by(!!site_column, !!longitude, !!latitude)

    # Create size column
  if (!rlang::quo_is_null(size_column)) {
    data <- data %>%
      dplyr::summarise(size = sum(!!size_column, na.rm = TRUE), .groups = "drop") %>%
      dplyr::ungroup()
  }else{
    data <- data %>%
      dplyr::summarise(size = dplyr::n(), .groups = "drop") %>%
      dplyr::ungroup()
  }

  # create sf object with original CRS
  sf_data <- data %>%
    sf::st_as_sf(coords = c(rlang::as_name(longitude), rlang::as_name(latitude)), crs = crs[1])

  # If transformation is needed, convert to crs2
  if (length(crs) > 1) {
    if (!is.null(crs[2])) {
      sf_data <- sf_data %>%
        sf::st_transform(crs = crs[2])
      crs <- crs[2]
    }
  }


  # Create reference raster

  ref_rast <- terra::rast(terra::vect(sf_data), res = resolution)

  # Extend a little bit the area
  if (!is.null(study_area)) {
    if (!any("sf" %in% class(study_area))) {
      rlang::abort("Area of study must be simple feature (sf) object")
    }

    if (!all(sf::st_geometry_type(study_area) == "POLYGON")) {
      rlang::abort("Area of study must be a polygon")
    }

    study_area <- study_area %>%
      sf::st_transform(crs = crs)

    bbox <- sf::st_bbox(study_area)

    ref_rast <- terra::extend(x = ref_rast, y = terra::ext(bbox))
  }


  accum_rast <- list()
  if (spread_factor > 1) {rlang::warn("Spread factor coerced to 1")}
  if (spread_factor <= 0) {rlang::abort("Spread factor must be more than 0")}

  # rasterize sf_data
  for (pt in 1:nrow(sf_data)) {
    sub_sf_data <- sf_data[pt, ]

    # Rasterize sub_sf_data to it own raster
    sf_rasterized <- terra::rasterize(x = sub_sf_data, y = ref_rast, field = 1) %>%
      terra::distance()

    # Apply Half-normal	exp(-y^2/2*sigma^2)
    param <- terra::global(sf_rasterized, fun = "std", na.rm = TRUE)[[1]]*spread_factor

    # Apply weight to kernel. Weight is abundance of species (number of individual)
    weighted <- sub_sf_data$size
    kernel_weighted <- weighted * exp(-0.5 * (sf_rasterized/param)^2)

    # Add to list
    accum_rast[[pt]] <- round(kernel_weighted)
  }

  average_abundance <- terra::mean(terra::rast(accum_rast), na.rm = TRUE)
  names(average_abundance) <- "Abundance"

  ## Selected only area where abundance is more than 1
  average_abundance[average_abundance < 1] <- NA
  coverage_rast <- average_abundance

  ## Convert to simple feature
  abun_vect <- terra::as.polygons(average_abundance, values = T) %>%
    sf::st_as_sf()

  ## Some statistic of abundance
  average_abun <- mean(abun_vect$Abundance, na.rm = TRUE)
  max_abun <- max(abun_vect$Abundance, na.rm = TRUE)
  sd_abun <- sd(abun_vect$Abundance, na.rm = TRUE)

  coverage_stats <- abun_vect %>%
    dplyr::summarise(`Polygon` = sf::st_union(geometry)) %>%
    dplyr::mutate(!!paste0("Spatial coverage (", "km\u00B2", ")") := as.numeric(sf::st_area(Polygon))/1000**2,
                  `Average abundance` = round(average_abun, 2),
                  `Maximum abundance` = round(max_abun),
                  `Standard Deviation` = round(sd_abun, 2)) %>%
    dplyr::as_tibble()

  return(list("Coverage raster" = coverage_rast, "Coverage stats" = coverage_stats))
}


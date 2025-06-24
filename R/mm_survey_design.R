#' Create a survey design for camera trap deployment
#'
#' @description
#' This function designs a survey for deploying camera traps within a specified study area.
#' It supports various sampling methods, including random, regular, and clustered sampling,
#' with options for minimum distance constraints and padding around the study area.
#'
#' @param study_area An `sf` polygon representing the area where the survey will be conducted.
#' @param method A character string specifying the sampling method. Options include:
#'   - "random": Randomly distributes camera trap sites within the study area.
#'   - "regular": Creates a regularly spaced grid of sites.
#'   - "regular_cluster": Generates regularly spaced clusters within which sites are sampled..
#'   - "random_cluster": Creates randomly clusters within which sites are sampled.
#'   - "mask":  Uses existing features in the `study_area` object to define
#'   sampling areas, with user-defined site allocation.
#' @param total_site An integer specifying the number of sites to be sampled per
#' cluster (for "regular_cluster" and "random_cluster") or the total number of
#' sites (for "random" and "regular" methods). For the "mask" method, this can
#' be a single value (applied to all features) or a vector specifying the number
#' of sites per feature in `study_area`.
#' @param total_cluster An integer defining the number of clusters (required for "random_cluster").
#' @param type_in A character string indicating the within-cluster sampling type. Options:
#'   - "regular": Places sites in a structured grid within each cluster or feature (for "mask" method).
#'   - "random": Distributes sites randomly within each cluster or feature (for "mask" method).
#' @param min_distance A numeric value specifying the minimum allowed distance
#' between sampled sites (applied only for the random methods).
#' @param distance A numeric vector specifying the distance (x and y spacing)
#' between grid cells for regular sampling methods. If a single value is provided,
#' it is used for both dimensions.
#' @param padding A numeric value defining the buffer distance to exclude areas
#' near the edge of the study area.
#' @param nest_padding A numeric value defining an additional buffer applied
#' within each cluster or mask feature to avoid placing sites near the edges of those units.
#' @param set_seed An optional integer for setting the random seed to ensure reproducibility.
#' @param verbose A logical indicating whether to display warnings and messages (default: `TRUE`).
#'
#' @return An `sf` object containing the sampled points within the study area.
#'
#' @note
#' The function ensures that the study area has a projected coordinate reference
#' system (CRS) before proceeding.
#' If a geographic CRS is detected, an error is raised.
#'
#' @examples
#' library(ggplot2)
#' # Load example dataset
#' data("pendjari")
#'
#' # Transform study area to a projected coordinate system
#' pendjari_trans <- pendjari %>%
#'   sf::st_transform(crs = "EPSG:32631")
#'
#' # Random sampling method with 15 sites, ensuring a minimum distance of 5000 meters between sites
#' random_sdes <- mm_survey_design(study_area = pendjari_trans, method = "random", verbose = TRUE,
#'                                 total_site = 15, min_distance = 5000, padding = 2000,
#'                                 set_seed = 123)
#'
#' # Regular sampling method using a grid with cell sizes of 4000m x 6000m
#' regular_sdes <- mm_survey_design(study_area = pendjari_trans, method = "regular", verbose = TRUE,
#'                                  distance = c(4000, 6000), padding = 2500, set_seed = 123)
#'
#' # Random-cluster sampling: 8 clusters, each containing 5 sites, ensuring a
#' # minimum site distance of 2000 meters
#' rand_c_sdes <- mm_survey_design(study_area = pendjari_trans,
#'                                 method = "random_cluster", verbose = TRUE,
#'                                 total_cluster = 8, total_site = 5,
#'                                 distance = c(7000, 3000), min_distance = 2000,
#'                                 padding = 2000, nest_padding = 500, set_seed = 123)
#'
#' # Random-cluster sampling with regularly distributed sites: 22 clusters, each
#' # with 8 regularly spaced sites
#' rand_c_reg_sdes <- mm_survey_design(study_area = pendjari_trans,
#'                                     method = "random_cluster", verbose = TRUE,
#'                                     total_cluster = 22, total_site = 8, type_in = "regular",
#'                                     distance = c(6000, 3000),
#'                                     padding = 1000, nest_padding = 0, set_seed = 123)
#'
#' # Regular-cluster sampling: Grid with 3 sites per cluster, ensuring a minimum
#' # distance of 2000 meters between sites
#' reg_c_sdes <- mm_survey_design(study_area = pendjari_trans,
#'                                method = "regular_cluster", verbose = TRUE,
#'                                total_site = 3, distance = c(7000, 6000),
#'                                min_distance = 2000, padding = 2000, set_seed = 123)
#'
#' # Regular-cluster sampling with regularly distributed sites within clusters
#' reg_c_reg_sdes <- mm_survey_design(study_area = pendjari_trans,
#'                                    method = "regular_cluster", verbose = TRUE,
#'                                    total_site = 3, distance = c(7000, 6000), type_in = "regular",
#'                                    padding = 1000, set_seed = 123)
#' # A plot with
#' ggplot()+
#'   geom_sf(data = pendjari_trans)+
#'   geom_sf(data = reg_c_reg_sdes)
#'
#' # Mask-based sampling: Sites are sampled within existing features of the study area
#' mask_sdes <- mm_survey_design(study_area = pendjari_trans,
#'                               method = "mask", verbose = TRUE,
#'                               total_site = 13, distance = c(7000, 6000),
#'                               min_distance = 2000, nest_padding = 2000, set_seed = 123)
#'
#' # Mask-based sampling with regularly spaced sites per feature
#' mask_regular_sdes <- mm_survey_design(study_area = pendjari_trans,
#'                                       method = "mask", verbose = TRUE, type_in = "regular",
#'                                       total_site = c(8, 2, 13), distance = c(7000, 6000),
#'                                       min_distance = 2000, nest_padding = 1000, set_seed = 123)
#'
#' mask_regular_sdes
#'
#' @export
mm_survey_design <- function(study_area,
                             method = "random",
                             total_site,
                             total_cluster,
                             type_in = "random",
                             min_distance = NULL,
                             distance = NULL,
                             padding = 10,
                             nest_padding = 0,
                             set_seed = NULL,
                             verbose = TRUE
                             ) {


  # Valid study area
  valid_study_area(study_area)
  # Valid CRS
  if (crs_type(study_area) == "Geographic") {
    sa_name <- deparse(substitute(study_area))
    msg <- paste0("Transform data from longitude/latitude (geographic) to projected system. Eg. ",
                  sa_name, " %>% st_transform(crs = 'EPSG:32631'). You can use https://epsg.io/ to
                  find the suitable crs. If the area of interest spans several
                  UTM zones, consider to use `EPSG:6933`")
    rlang::abort(msg)
  }
  # Set distance
  if (length(distance) == 1) {distance <- c(distance, distance)}
  # Set seed
  if (!is.null(set_seed)) { set.seed(set_seed) }
  # Padding study area
  if(padding < 0){padding <- abs(padding)}
  padding_study_area <- study_area %>% sf::st_buffer(dist = - padding)
  # Ensure methods
  avail_method <- c("random", "regular", "regular_cluster","random_cluster", "mask")
  if (!method %in% avail_method) {rlang::abort(sprintf("Method `%s` is not accepted.
                                                       It must be one of: %s.",
                                                       method, paste0(avail_method,
                                                                      collapse = ", ")))}
  # Ensure type in
  avail_type <- c("random", "regular")
  if (!type_in %in% avail_type) {rlang::abort(sprintf("Type `%s` is not accepted.
                                                      It must be one of: %s.",
                                                      type_in, paste0(avail_type,
                                                                      collapse = ", ")))}
  # Total site
  if (method != "mask" & hasArg(total_site)) {
    total_site <- total_site[1]
    study_area <- study_area %>% sf::st_union() %>% sf::st_as_sf()
    }

  ## METHOD IMPLEMENTATION
  sur_des <- switch (method,
    "random" = {

      designed <- random_sampling(study_area = study_area, total_site = total_site,
                                  min_distance = min_distance, padding = padding, verbose = verbose)
    },

    "regular" = {

      if (!is.null(distance)) {

        ## Warning if total_size if provide
        if (verbose) {
          if (hasArg(total_site) ) {
            rlang::warn("`total_site` is ignored if distance is used.")
          }
        }

        if(padding <= 0){padding <- 1}
        designed <- suppressWarnings(
          sf::st_make_grid(
            x = study_area, cellsize = c(distance[1], distance[2])) %>%
            sf::st_as_sf() %>%
            sf::st_cast(to = "POINT") %>%
            sf::st_intersection(y = study_area %>% sf::st_buffer(dist = - padding))
        )


      }else{
        designed <- sf::st_sample(x = padding_study_area,
                                  size = total_site, type = "regular")
      }

    },

    "regular_cluster" = {
      cluster_grid <- regular_grid(study_area, distance = distance, padding = padding)
      cluster_grid <- cluster_grid %>%
        dplyr::mutate(grid_area = as.numeric(sf::st_area(.))) %>%
        dplyr::filter(grid_area >= (distance[1]*distance[2])) %>%
        dplyr::select(-grid_area)

      if (type_in == "random") {

        cl <- clustering(cluster = cluster_grid, total_site = total_site,
                         min_distance = min_distance, nest_padding = nest_padding,
                         verbose = verbose)
        designed <- cl[[1]]
        print(designed)

        if (verbose) {
          if (any(unlist(cl[[2]]))) {
            rlang::warn(sprintf("Cluster size doesn't allow to have %s sites
                                per cluster. Try to adjust distance and min_distance",
                                total_site))
          }
        }
      }else{
        designed <- lapply(1:nrow(cluster_grid), function(feature){
          cell <- cluster_grid[feature, ] %>% sf::st_buffer(dist = -nest_padding)
          sf::st_sample(x = cell, size = total_site, type = "regular") %>%
            sf::st_as_sf()%>%
            dplyr::slice(1:total_site)

        }) %>% dplyr::bind_rows()
      }

    },

    "random_cluster" = {
      cluster_grid <- regular_grid(study_area, distance = distance, padding = padding)
      cluster_grid <- cluster_grid %>%
        dplyr::mutate(grid_area = as.numeric(sf::st_area(.))) %>%
        dplyr::filter(grid_area >= (distance[1]*distance[2])) %>%
        dplyr::select(-grid_area)

      if (total_cluster > nrow(cluster_grid)) {
        rlang::abort(sprintf("Not possible to sample %s clusters in %s available.
                             Try to reduce distance dimension.", total_cluster, nrow(cluster_grid)))
      }
      cluster_grid <- cluster_grid[sample(1:nrow(cluster_grid), size = total_cluster), ]

      if (type_in == "random") {
        cl <- clustering(cluster = cluster_grid, total_site = total_site,
                         min_distance = min_distance, nest_padding = nest_padding, verbose = verbose)

        designed <- cl[[1]]

        if (verbose) {
          if (any(unlist(cl[[2]]))) {
            rlang::warn(sprintf("Cluster size doesn't allow to have %s sites per
                                cluster. Try to adjust distance and min_distance",
                                total_site))
          }
        }
      }else{
        designed <- lapply(1:nrow(cluster_grid), function(feature){
          cell <- cluster_grid[feature, ]%>% sf::st_buffer(dist = -nest_padding)
          sf::st_sample(x = cell, size = total_site, type = "regular") %>%
            sf::st_as_sf() %>%
            dplyr::slice(1:total_site)

        }) %>% dplyr::bind_rows()
      }
    },

    "mask" = {
      if (length(total_site) == 1 | length(total_site) < nrow(study_area)) {
        remaining <- nrow(study_area) - length(total_site)
        total_site <- c(total_site, rep(total_site[1], remaining))
      }
      if (type_in == "random") {
        designed <- lapply(1:nrow(study_area), function(feature){
          random_sampling(study_area = study_area[feature, ],
                          total_site = total_site[feature],
                          min_distance = min_distance, padding = nest_padding,
                          verbose = verbose)
        }) %>% dplyr::bind_rows()
      }else{
        designed <- lapply(1:nrow(study_area), function(feature){
          sf::st_sample(x = study_area[feature, ]%>% sf::st_buffer(dist = -nest_padding),
                        size = total_site[feature],
                        type = "regular") %>%
            sf::st_as_sf() %>%
            dplyr::slice(1:total_site[feature])
        }) %>% dplyr::bind_rows()

      }

    }

    ##########
  )

  ## Verbose
  if (verbose & method != "mask") {
    if ("sf" %in% class(designed)) {
      pts_on <- sf::st_point_on_surface(designed) %>%
        sf::st_as_sf()
      if (hasArg(total_site) && nrow(pts_on) < total_site) {
        rlang::warn(sprintf("Sampling size (%s) differ from the requested size
                            (%s).", nrow(pts_on), total_site))
      }
    }
  }

  designed <- designed %>% sf::st_as_sf() %>% dplyr::rename(geometry = x)

  return(designed)

}


#########
#' Sampling with minimum distance
#'
#' @description
#' Sampling with minimum distance between points
#' @keywords internal
#' @noRd
#'
random_sampling <- function(study_area,
                            total_site,
                            min_distance = NULL,
                            padding = 5,
                            verbose = TRUE,
                            cluster_error = FALSE,
                            ...) {

  padding_study_area <- study_area %>% sf::st_buffer(dist = -padding)
  if (sf::st_is_empty(padding_study_area)) {
    if (cluster_error) {
      rlang::abort("At least a cluter is set to empty polygon.", call = NULL)
    }else{
      rlang::abort("Study area set to empty! Reduce the padding.", call = NULL)
    }
  }

  if (!hasArg(total_site) | is.null(total_site)) {
    rlang::abort("Provide `total_site`", call = NULL)}
  total_site <- round(total_site)

  if (is.null(min_distance)) {

    sampled_points <- suppressWarnings({
      sf::st_sample(x = padding_study_area,
                    size = total_site, type = "random", exact = TRUE) %>%
        sf::st_as_sf()
    })
  }else{
    sa_grid_vec <- regular_grid(study_area = study_area, distance = min_distance,
                                padding = padding)
    if (nrow(sa_grid_vec) == 0) {
      rlang::abort("`min_distance` provided cannot produce a suitable space for
                   sampling.", call = NULL)
    }

    single_random_point <- sample(x = 1:total_site, size = 1)
    sample_in_cell <- lapply(1:nrow(sa_grid_vec), function(feature){
      if(!sf::st_is_empty(sa_grid_vec[feature, ])){
        suppressWarnings({
          sf::st_sample(x = sa_grid_vec[feature, ], size = total_site) %>%
            sf::st_as_sf() %>%
            dplyr::slice(single_random_point)
        })
      }

    }) %>%
      dplyr::bind_rows()


    if (total_site > nrow(sample_in_cell)) {
      total_site <- nrow(sample_in_cell)
      if (verbose) {
        rlang::warn(sprintf("Maximum number of points could not exced %s with
                            minimum distance of %s.",
                            nrow(sample_in_cell), min_distance))
      }
      sampled_points <- sample_in_cell[sample(x = 1:nrow(sample_in_cell),
                                              size = total_site), ]
    }else{
      sampled_points <- sample_in_cell[sample(x = 1:nrow(sample_in_cell),
                                              size = total_site), ]
    }

    return(sampled_points)

  }

}


#' Regular grid
#'
#' @description
#' Create regular grid with terra
#' @keywords internal
#' @noRd
#'

regular_grid <- function(study_area, distance, padding){
  if (length(distance) == 1) {distance <- c(distance, distance)}
  padding_study_area <- study_area %>% sf::st_buffer(dist = - padding)

  sa_grid <- terra::rast(x = terra::vect(study_area), res = c(distance[1], distance[2]))
  terra::values(sa_grid) <- 1

  skip_col <- 1:terra::ncol(sa_grid) %% 2 != 0
  skip_row <- 1:terra::nrow(sa_grid) %% 2 != 0

  sa_grid[, skip_col] <- NA
  sa_grid[skip_row, ] <- NA

  sa_grid_vec <- suppressWarnings(sf::st_as_sf(terra::as.polygons(sa_grid)) %>%
                                    sf::st_cast(to = "POLYGON") %>%
                                    sf::st_intersection(x = ., y = padding_study_area))

  return(sa_grid_vec)
}

#' Clustering
#'
#' @description
#' Sample points within feature
#' @keywords internal
#' @noRd
#'
clustering <- function(cluster,
                       total_site,
                       min_distance,
                       nest_padding = 0,
                       verbose){
  designed <- list(); less_than_requested <- list()
  for (feature in 1:nrow(cluster)) {
    single_grid <- cluster[feature, ]; options(warn = -1)

    single_cluster <- random_sampling(study_area = single_grid, total_site = total_site,
                      padding = nest_padding, min_distance = min_distance,
                      verbose = verbose, cluster_error = TRUE)

    #if (all(c(length(single_cluster) != 0, !grepl("error", single_cluster)))) {
      single_cp <- single_cluster %>% dplyr::mutate(cluster = paste0("cluster_", feature))

      ## Handle for warning
      pts_on <- sf::st_point_on_surface(single_cp) %>% sf::st_as_sf()

      if (total_site > nrow(pts_on)) {less_than_requested[[feature]] <- TRUE}
      designed[[feature]] <- single_cp
    #}

  }
  designed <- designed %>% dplyr::bind_rows(); options(warn = 1)

  return(list(designed, less_than_requested))
}

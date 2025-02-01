#' Interactive Camera Trap Location Adjustment
#'
#' This function launches a shiny application that allows users to visualize
#' and manually adjust the geographic coordinates of camera trap locations.
#' Users can drag points on an interactive map to update the positions
#' of camera traps, and the updated dataset is saved to the global environment.
#'
#' @param data A data frame containing the camera trap data to be processed.
#' @param longitude A string representing the column name for longitude in the
#' dataset.
#' @param latitude A string representing the column name for latitude in
#' the dataset.
#' @param location_name A string representing the column name for the
#' location name or unique identifier for each camera trap point.
#' @param coord_system A string specifying the coordinate system of
#' the input data. Choices are `"geographic"` for longitude and latitude,
#' or `"projected"` for projected coordinates.
#' @param crs An integer representing the coordinate reference system (CRS)
#' in EPSG format. Required when `coord_system = "projected"`.
#' @param new_data_name A string specifying the name of the new dataset with
#' updated coordinates to be created in the global environment.
#'
#' @return A shiny application is launched to display the map and allow
#' manual coordinate adjustments.
#'   The modified dataset is saved to the global environment under the name
#'   provided in `new_data_name`.
#'
#' @examples
#' \dontrun{
#'   library(dplyr)
#'   # Example dataset
#'   camera_traps <- tibble::tibble(
#'     trap_id = c("Trap1", "Trap2", "Trap3"),
#'     lon = c(36.8, 36.9, 37.0),
#'     lat = c(-1.4, -1.5, -1.6)
#'   )
#'
#'   # Launch the application
#'   mm_check_location(
#'     data = camera_traps,
#'     longitude = "lon",
#'     latitude = "lat",
#'     location_name = "trap_id",
#'     coord_system = "geographic",
#'     new_data_name = "updated_camera_traps"
#'   )
#'   # After adjustments, the updated dataset will be available in the global
#'   environment as `updated_camera_traps`.
#' }
#'
#' @import shiny
#' @import leaflet
#' @import dplyr
#' @import sf
#' @export

mm_check_location <- function(data,
                              longitude,
                              latitude,
                              location_name,
                              coord_system = c("geographic", "projected"),
                              crs,
                              new_data_name) {

  data_copy <- data
  coord_system <- match.arg(coord_system, choices = c("geographic", "projected"))
  lon_ <- paste0(dplyr::ensym(longitude))
  lat_ <- paste0(dplyr::ensym(latitude))

  data <- data %>%
    dplyr::mutate("obs" = paste0(!!dplyr::ensym(lon_), !!dplyr::ensym(lat_))) %>%
    #dplyr::distinct("obs", .keep_all = TRUE) %>%
    #dplyr::select(-"obs") %>%
    dplyr::filter(!is.na(!!dplyr::ensym(lon_)) & !is.na(!!dplyr::ensym(lat_)))


  if (coord_system == "projected") {
    if(!methods::hasArg(crs))stop("Specify the crs")
    coord <- data %>%
      sf::st_as_sf(coords = c(lon_, lat_), crs = crs) %>%
      sf::st_transform(crs = 4326) %>%
      sf::st_coordinates() %>%
      as.data.frame()

  }else{
    coord <- data %>%
      dplyr::rename("X" = lon_, "Y" = lat_)
  }

  coord <- coord %>% dplyr::select("X", "Y")

  # data to use
  if (paste0(dplyr::ensym(location_name)) == "") {
    stop("location_name can not be empty")
  }
  plc_ <- paste0(dplyr::ensym(location_name))
  data <- data %>%
    dplyr::select(- dplyr::all_of(c(lon_, lat_))) %>%
    dplyr::bind_cols(coord) %>%
    # add location_name column
    dplyr::rename("location_name" = plc_)


  # shiny
  ui <- leafletOutput("map", width = "100%", height = "100vh")#,  # The map output
  server <- function(input, output, session) {
    # Render the leaflet map with a draggable marker
    output$map <- renderLeaflet({
      leaflet::leaflet() %>%
        leaflet::addTiles(attribution = "Maimer") %>%
        leaflet::addProviderTiles("OpenStreetMap.Mapnik", group = "OSM") %>%
        leaflet::addProviderTiles("OpenStreetMap.France", group = "OSM France") %>%
        leaflet::addProviderTiles("Esri.WorldImagery", group = "Natural") %>%
        leaflet::addProviderTiles("OpenTopoMap", group = "OpenTopoMap") %>%
        leaflet::addLayersControl(
          baseGroups = c("OSM", "OSM France", "Natural", "OpenTopoMap"),
          options = leaflet::layersControlOptions(collapsed = T)
        ) %>%
        leaflet::addMarkers(lng = data$X,
                            lat = data$Y,
                            layerId = data$location_name,
                            options = leaflet::markerOptions(draggable = TRUE),
                            popup = data$location_name) %>%
        leaflet::setView(lng = data$X[1], lat = data$Y[1], zoom = 8)
    })

    # Capture the updated coordinates after marker drag event
    shiny::observeEvent(input$map_marker_dragend, {
      new_coords <- input$map_marker_dragend  # Get the coordinates from the input object
      # Update the coordinates in the reactive variable
      updated_coords <- dplyr::tibble(X = new_coords$lng,
                                      Y = new_coords$lat,
                                      location_name = new_coords$id)


      # Update data replacing new coordinate
      coord <- coord %>% dplyr::mutate(location_name = data[["location_name"]])
      coord[coord[["location_name"]] == new_coords$id, "X"] <- new_coords$lng
      coord[coord[["location_name"]] == new_coords$id, "Y"] <- new_coords$lat


      coord <- coord %>%
        dplyr::rename_with(~ c(plc_, lon_, lat_), c("location_name", "X", "Y"))

      data_to_return <- data_copy %>%
        dplyr::select(- dplyr::all_of(c(lon_, lat_))) %>%
        dplyr::left_join(y = coord, by = plc_)

      assign(new_data_name, data_to_return, pos = ".GlobalEnv")
    })
  }
  # shiny end

  # Run the application
  return(shiny::shinyApp(ui = ui, server = server))
}

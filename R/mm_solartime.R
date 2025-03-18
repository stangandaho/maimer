#' Transform time to solar time anchored to sunrise and sunset
#'
#' This function converts local time to solar time based on the sunrise and sunset
#' times for a given location. Solar time is a timekeeping system where the day
#' is defined by the position of the sun in the sky, with sunrise marking the start
#' of the day and sunset marking the end.
#'
#' @param data A data frame containing the date, longitude, and latitude columns.
#' If `NULL`, the function will use the `date`, `longitude`, and `latitude`
#' parameters directly. Default is `NULL`.
#' @param date A vector of date-time values or a column name in `data` representing
#' the date-time values to be converted to solar time. This can be a character
#' vector or a `POSIXlt` object.
#' @param longitude A numeric vector or a column name in `data` representing the
#' longitude of the location(s). Longitude should be in decimal degrees.
#' @param latitude A numeric vector or a column name in `data` representing the
#' latitude of the location(s). Latitude should be in decimal degrees.
#' @param crs A coordinate reference system (CRS) string or object specifying the
#' current CRS of the input coordinates. If provided, the function will transform
#' the coordinates to longitude and latitude (WGS84). This is useful when the input
#' coordinates are in a projected system (e.g., UTM). Default is `NULL`.
#' @param time_zone A numeric vector representing the time zone offset(s) from UTC
#' (in hours). If `data` is provided, this should match the number of unique
#' locations in the data.
#' @param ... Additional arguments passed to `as.POSIXlt` for date parsing.
#' @param try_formats A vector of date-time formats to try when parsing the `date` parameter. Default includes common date-time formats.
#'
#' @return A tibble with the following columns:
#' \itemize{
#'   \item `input`: The original date-time values.
#'   \item `clock`: The local clock time.
#'   \item `solar`: The calculated solar time.
#' }
#' If `data` is provided, the tibble will also include the longitude and latitude columns.
#'
#' @details
#' The function calculates solar time by first determining the sunrise and sunset times for the given location(s) and date(s). It then uses these times to anchor the solar time calculation. The solar time is computed by transforming the local clock time based on the position of the sun in the sky.
#'
#' If `data` is provided, the function will process each unique location in the
#' data and return a tibble with the solar time for each date-time value. If `data`
#' is `NULL`, the function will process the `date`, `longitude`, and `latitude`
#' parameters directly.
#' @references
#' Rowcliffe, M. (2023). activity: Animal Activity Statistics.
#' R package version 1.3.4. https://CRAN.R-project.org/package=activity
#'
#' @examples
#' library(dplyr)
#'
#' read.csv(system.file("penessoulou_season1.csv", package = "maimer")) %>%
#'  dplyr::filter(species == "Erythrocebus patas") %>%
#'  # Select independent events based on a given threshold
#'  maimer::mm_independence(species_column = species,
#'                           datetime = datetimes, threshold = 60*5, # 5 minutes
#'                           format = "%Y-%m-%d %H:%M:%S",
#'                           only = TRUE) %>%
#'  # Transform Time to Solar Time
#'  mm_solartime(data = ., date = datetime, longitude = longitude, latitude = latitude,
#'                crs = "EPSG:32631", time_zone = 1)
#'
#' @export

mm_solartime <- function (data = NULL,
                          date,
                          longitude,
                          latitude,
                          crs = NULL,
                          time_zone, ...,
                          try_formats = c("%Y-%m-%d %H:%M:%OS", "%Y/%m/%d %H:%M:%OS",
                                         "%Y:%m:%d %H:%M:%OS", "%Y-%m-%d %H:%M",
                                         "%Y/%m/%d %H:%M", "%Y:%m:%d %H:%M",
                                         "%Y-%m-%d", "%Y/%m/%d", "%Y:%m:%d")){


  if (!is.null(data)) {
    date_str <- as.character(substitute(date))
    lon_str <- as.character(substitute(longitude))
    lat_str <- as.character(substitute(latitude))
    cols <- c(date_str, lat_str, lon_str)
    # Confirm column presence
    missed_col_error(data = data, cols)

    if (!is.null(crs)) {
      data <- to_lon_lat(data = data, longitude = lon_str, latitude = lat_str, crs = crs)
    }

    data <- data %>% dplyr::filter(!is.na(!!dplyr::sym(lon_str)), !is.na(!!dplyr::sym(lat_str))) %>%
      dplyr::mutate(!!dplyr::sym(lon_str) := round(!!dplyr::sym(lon_str), 5),
                    !!dplyr::sym(lat_str) := round(!!dplyr::sym(lat_str), 5))



    longitude <- data[[lon_str]]; latitude <- data[[lat_str]]
    unique_location <- unique(paste(longitude, latitude))

    # Set time_zone number to length of unique_location
    if(length(time_zone) < length(unique_location)){
      remaining <- length(unique_location) - length(time_zone)
      time_zone <- c(time_zone, rep(time_zone, remaining))
    }

    completed <- tibble()
    for (loc in 1:length(unique_location)) {
      single_loc <- strsplit(unique_location[loc], split = "\\s")[[1]]
      longitude <- round(as.numeric(single_loc[1]), 5)
      latitude <- round(as.numeric(single_loc[2]), 5)
      flt <- data[[lon_str]] == longitude & data[[lat_str]] == latitude
      dta <- data[flt, ]

      if(nrow(dta) < 1)next
      date <- dta %>% dplyr::pull(!!dplyr::sym(date_str))

      date <- as.POSIXlt(date, tryFormats = try_formats, ...)
      posdat <- list(latitude, longitude, time_zone[loc])

      suntimes <- mm_wrap(mm_get_suntimes(date, latitude, longitude, time_zone[loc])[, -3] * pi/12)
      tm <- mm_get_time(date)
      list(input = date, clock = tm, solar = mm_transtime(tm, suntimes))
      completed <- dplyr::bind_rows(completed, tibble(!!dplyr::sym(lon_str) := longitude,
                                                      !!dplyr::sym(lat_str) := latitude,
                                                      !!dplyr::sym(date_str) := date, clock = tm,
                                                      solar = mm_transtime(tm, suntimes)))
    }

    return(completed)
  }


  date <- as.POSIXlt(date, tryFormats = try_formats, ...)
  posdat <- list(latitude, longitude, time_zone)
  if (!all(unlist(lapply(posdat, inherits, "numeric"))) |
      any(unlist(lapply(posdat, length)) != length(date) & unlist(lapply(posdat, length)) != 1)){
    rlang::abort("latitude, longitude and time_zone must all be numeric scalars, or vectors the same length as date")}
  suntimes <- mm_wrap(mm_get_suntimes(date, latitude, longitude, time_zone)[, -3] * pi/12)
  tm <- mm_get_time(date)

  return(tibble(input = date, clock = tm, solar = mm_transtime(tm, suntimes)))
}


#' @noRd
mm_get_suntimes <- function(date,
                           latitude,
                           longitude,
                           offset, ...,
                           tryFormats = c("%Y-%m-%d %H:%M:%OS",
                                          "%Y/%m/%d %H:%M:%OS", "%Y:%m:%d %H:%M:%OS", "%Y-%m-%d %H:%M",
                                          "%Y/%m/%d %H:%M", "%Y:%m:%d %H:%M", "%Y-%m-%d", "%Y/%m/%d",
                                          "%Y:%m:%d")){
    nlat <- length(latitude)
    nlon <- length(longitude)
    ndat <- length(date)
    if ((nlat > 1 & nlat != ndat) | (nlon > 1 & nlon != ndat))
      stop("latitude and longitude must have length 1 or the same length as date")
    d <- as.POSIXlt(date, tryFormats = tryFormats, ...)$yday + 1
    R <- 6378
    epsilon <- 23.45 * pi/180
    L <- latitude * pi/180
    lon_h <- 24 * longitude/360
    r <- 149598000
    theta <- 2 * pi/365.25 * (d - 80)
    zs <- r * sin(theta) * sin(epsilon)
    rp <- sqrt(r^2 - zs^2)
    acos_term <- (R - zs * sin(L))/(rp * cos(L))
    t0 <- suppressWarnings(1440/(2 * pi) * acos(acos_term))
    that <- t0 + 5
    n <- 720 - 10 * sin(4 * pi * (d - 80)/365.25) + 8 * sin(2 *pi * d/365.25)
    sunrise <- (n - that)/60 - lon_h + offset
    sunset <- (n + that)/60 - lon_h + offset
    daylength <- ifelse(acos_term < -1, 24, ifelse(acos_term > 1, 0, sunset - sunrise))
    data.frame(sunrise = sunrise, sunset = sunset, daylength = daylength)
}


#' @noRd
mm_wrap <- function (x, bounds = c(0, 2 * pi)) {
  bounds[1] + (x - bounds[1])%%diff(bounds)
  }

#' @noRd
mm_get_time <- function (x, scale = c("radian", "hour", "proportion"), ...,
          tryFormats = c("%Y-%m-%d %H:%M:%OS", "%Y/%m/%d %H:%M:%OS",
                         "%Y:%m:%d %H:%M:%OS", "%Y-%m-%d %H:%M", "%Y/%m/%d %H:%M",
                         "%Y:%m:%d %H:%M", "%Y-%m-%d", "%Y/%m/%d", "%Y:%m:%d")){
  scale <- match.arg(scale)
  x <- as.POSIXlt(x, tryFormats = tryFormats, ...)
  res <- x$hour + x$min/60 + x$sec/3600
  if (scale == "radian")
    res <- res * pi/12
  if (scale == "proportion")
    res <- res/24
  if (all(res == 0, na.rm = T))
    warning("All times are 0: may be just strptime default?")
  res
}

#' @noRd
cmean <- function(x, ...){
  X <- mean(cos(x), ...)
  Y <- mean(sin(x), ...)
  mm_wrap(atan(Y/X) + ifelse(X<0,pi,0))
}

#' @noRd
mm_transtime <- function (date, anchor,
                          mnanchor = NULL,
                          type = c("average", "equinoctial","single")){
  if (!all(date >= 0 & date <= 2 * pi, na.rm = TRUE))
    warning("some date values are <0 or >2*pi, expecting radian data")
  if (max(date, na.rm = TRUE) < 1)
    warning("max(date) < 1, expecting radian data")
  if (!all(anchor >= 0 & anchor <= 2 * pi, na.rm = TRUE))
    warning("some anchor values are <0 or >2*pi, expecting radian values")
  if (is.null(ncol(anchor)))
    anchor <- matrix(anchor, ncol = 1)
  if (!all(apply(anchor, 2, is.numeric)))
    stop("anchor must be a numeric vector, matrix or data.frame")
  nr <- nrow(anchor)
  if (length(date) != nr)
    stop("date and anchor have different lengths")
  type <- match.arg(type)
  nc <- ncol(anchor)
  if (is.null(mnanchor))
    mnanchor <- apply(anchor, 2, cmean, na.rm = TRUE)
  if (type == "single") {
    if (nc > 1)
      warning("only one column needed for anchor; additional columns ignored")
  }
  else {
    if (nc == 1)
      stop("double anchoring requires a two-column matrix or data.frame for anchor")
    if (!is.vector(mnanchor) | length(mnanchor) != 2)
      stop("if provided, mnanchor must be a 2-element vector for double anchoring")
    if (nc > 2)
      warning("only two columns needed for anchor; additional columns ignored")
  }
  if (type == "single") {
    res <- mm_wrap(mnanchor[1] + date - anchor[, 1])
  }
  else {
    difs <- mm_wrap(cbind(date, date) - anchor)
    flip <- difs[, 1] > difs[, 2]
    a1 <- ifelse(flip, anchor[, 2], anchor[, 1])
    a2 <- ifelse(flip, anchor[, 1], anchor[, 2])
    relpos <- mm_wrap(date - a1)/mm_wrap(a2 - a1)
    interval <- switch(type, equinoctial = pi, average = ifelse(flip,
                                                                mm_wrap(mnanchor[1] - mnanchor[2]), mm_wrap(mnanchor[2] -
                                                                                                        mnanchor[1])))
    baseline <- switch(type, equinoctial = ifelse(flip, pi *
                                                    3/2, pi/2), average = ifelse(flip, mnanchor[2], mnanchor[1]))
    res <- mm_wrap(baseline + interval * relpos)
  }
  res
}


#' @noRd
to_lon_lat <- function(data, longitude, latitude, crs){

  data <- data %>%
    dplyr::filter(!is.na(!!dplyr::sym(longitude)), !is.na(!!dplyr::sym(latitude))) %>%
    sf::st_as_sf(coords = c(longitude, latitude), crs = crs) %>%
    sf::st_transform(crs = 4326)


  xy <- data.frame(sf::st_coordinates(data))
  colnames(xy) <-  c(longitude, latitude)

  data <- data %>%
    sf::st_drop_geometry() %>%
    dplyr::bind_cols(xy)

  data
}

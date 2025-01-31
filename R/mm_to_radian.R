#' Convert time to radians
#'
#' This function converts time values into radians, which is often used in circular statistics and time-of-day analyses.
#'
#' @param data A data frame containing a column with time values. Optional. If `NULL`, the `times` parameter is treated as a standalone vector.
#' @param times A column name in the `data` or a vector of time values to be converted. Time values should be in a format recognized by `as.POSIXct()`.
#' @param format A string specifying the format of the time values, using the standard POSIX formatting syntax. Default is `"%H:%M:%S"`.
#' @param time_zone A string specifying the time zone for interpreting the time values. Default is `"UTC"`.
#'
#' @return If `data` is provided, the function returns the input data frame with an additional column named `time_radian`.
#' If `data` is not provided, the function returns a numeric vector of time values converted to radians.
#'
#' @details
#' This function converts time values into radians based on a 24-hour clock:
#' - A full day (24 hours) corresponds to \eqn{2\pi} radians.
#' - The fractional time of the day is calculated as:
#' \deqn{\text{Fraction of the day} = \frac{\text{hours}}{24} + \frac{\text{minutes}}{1440} + \frac{\text{seconds}}{86400}}
#'
#' For example, for a time of 23 hours, 6 minutes, and 12 seconds:
#' \deqn{\text{Fraction of the day} = \frac{23}{24} + \frac{6}{1440} + \frac{12}{86400}}
#'
#' To convert this fraction into radians:
#' \deqn{\text{Radians} = \text{Fraction of the day} \times 2\pi}
#'
#' @examples
#' \dontrun{
#'   # Convert a standalone vector of time values
#'   times <- c("00:00:00", "06:00:00", "12:00:00", "18:00:00")
#'   mm_to_radian(times = times, format = "%H:%M:%S")
#'
#'   # Convert a column of time values in a data frame
#'   data <- data.frame(times = c("00:00:00", "06:00:00", "12:00:00", "18:00:00"))
#'   mm_to_radian(data = data, times = times, format = "%H:%M:%S")
#' }
#'
#' @import dplyr
#' @export

mm_to_radian <- function(data,
                         times,
                         format = "%H:%M:%S",
                         time_zone = "UTC"
                         ){


  if (hasArg(data)) {
    tm_ <- paste0(dplyr::ensym(times))
    if(!tm_ %in% colnames(data)){
      stop(sprintf("%s not in data", tm_))
    }
    times <- data[[tm_]]
  }

  times <- format(times, format = format,  usetz = FALSE)


  lf <- as.POSIXct(strptime(times,
                            format = format,
                            tz = time_zone))

  rg <- as.POSIXct(strptime("0", format = "%S", tz = time_zone))
  time_radian <- (as.numeric(lf) - as.numeric(rg))/3600 * (pi/12)

  if (hasArg(data)) {
    data[["time_radian"]] <- time_radian
    tr <- data
  }else{
    tr <- time_radian
  }

  return(tr)
}

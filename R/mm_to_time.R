#' Convert Radian to Time
#'
#' This function converts an angle in radians (representing a fraction of a full circle)
#' into a time in the format '%H:%M:%S'. The conversion assumes that the radian value
#' represents a fraction of a 24-hour day (i.e., 0 radians is midnight
#' and \eqn{2\pi} radians is the next midnight).
#'
#' @param radian A numeric value or vector representing an angle in radians.
#' The value must lie within the range \eqn{[0, 2\pi]}, where 0 corresponds to midnight
#' (00:00:00) and \eqn{2\pi} corresponds to the next midnight (24:00:00).
#'
#' @return A character string representing the time in the format '%H:%M:%S'.
#'
#' @examples
#' # Convert 1.6 radians to time
#' mm_to_time(1.6)
#' # Output: "06:06:42"
#'
#' @seealso
#'   [mm_to_radian()]
#'
#' @importFrom base floor ceiling
#'
#' @export
mm_to_time <- function(radian) {

  if (!class(radian) %in% c("numeric", "integer")) {
    stop("radian can't be ", class(radian))
  }

  if (all(radian < 0 | radian > 2*pi)) {
    stop(sprintf("%f is out of [0, 2\u03C0]", radian))
  }

  fraction_of_day <- radian/(2*pi)
  hour_decimal <- fraction_of_day*24
  hour_part <- floor(hour_decimal)

  minute_decimal <- (hour_decimal - hour_part)*60
  minute_part <- floor(minute_decimal)

  second_decimal <- (minute_decimal - minute_part)*60
  second <- ceiling(second_decimal)

  timing <- sprintf("%02d:%02d:%02d", hour_part, minute_part, second)

  return(timing)
}


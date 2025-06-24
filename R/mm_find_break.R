#' Detect time gaps in a datetime series
#'
#' Identifies breaks in a sequence of datetime observations based on a specified time threshold.
#'
#' @param data A data frame containing the datetime column.
#' @param datetime The datetime column.
#' @param format Optional. A character string specifying the datetime format, passed to \code{as.POSIXlt}.
#' @param threshold A numeric value indicating the minimum gap to be considered a break (default is 10).
#' @param time_unit The unit for the threshold. Supported values include "secs", "mins", "hours", "days", and "weeks".
#'
#' @return A tibble with columns \code{start}, \code{end}, and \code{duration}
#' showing the start and end of each break and its length.
#'
#' @examples
#' pene <- read.csv(maimer:::table_files()[1])
#'
#' set_cam <- pene %>%
#'   dplyr::filter(camera == "CAMERA 3") %>%
#'   dplyr::mutate(datetimes = as.POSIXlt(datetimes, format = "%Y-%m-%d %H:%M:%OS")) %>%
#'   dplyr::arrange(datetimes)
#'
#' drtn <- mm_find_break(data = pene, datetime = "datetimes", threshold = 5, time_unit = "days")
#' drtn
#' @export
mm_find_break <- function(data,
                          datetime,
                          format,
                          threshold = 10,
                          time_unit = "hours") {

  # A range of time unit possibilities
  time_units <- c("s" = "secs", "secs" = "secs", "seconds" = "secs", "sec" = "secs", "second" = "secs",
                  "m" = "mins", "min" = "mins", "mins" = "mins", "minute" = "mins", "minutes" = "mins",
                  "h" = "hours", "hour" = "hours", "hours" = "hours",
                  "d" = "days", "day" = "days", "days" = "days",
                  "w" = "weeks", "week" = "weeks", "weeks" = "weeks")
  if (! time_unit %in% unique(names(time_units))) {
    rlang::abort(message = sprintf("Time unit '%s' not supported!", time_unit))
  }
  time_unit <- time_units[[time_unit]]

  # Format to try of 'format' is not provided
  try_format = c("%Y-%m-%d %H:%M:%OS", "%Y/%m/%d %H:%M:%OS",
                 "%Y:%m:%d %H:%M:%OS", "%Y-%m-%d %H:%M",
                 "%Y/%m/%d %H:%M", "%Y:%m:%d %H:%M",
                 "%Y-%m-%d", "%Y/%m/%d", "%Y:%m:%d")

  datetime_series <- data %>% dplyr::pull({{ datetime }})
  # Ensure datetime_series is POSIXct

  # Coerce to POSIXlt if necessary
  if (!inherits(datetime_series, "POSIXt")) {
    datetime_series <- as.POSIXlt(datetime_series, format = format, tryFormats = try_format)
  }

  # Sort the datetime_series to ensure proper order
  datetime_series <- sort(datetime_series)

  # Compute time differences between consecutive times
  time_diffs <- diff(datetime_series)

  # Identify breaks where the difference exceeds the threshold
  breaks_idx <- which(as.numeric(time_diffs, units = time_unit) > threshold)

  # Create a data frame of breaks
  if (length(breaks_idx) == 0) {
    return(data.frame(
      break_start = as.POSIXct(character(0)),
      break_end = as.POSIXct(character(0)),
      gap_duration_secs = numeric(0)
    ))
  }

  break_starts <- datetime_series[breaks_idx]
  break_ends <- datetime_series[breaks_idx + 1]
  gap_durations <- as.numeric(break_ends - break_starts, units = time_unit)

  breaks_df <- dplyr::tibble(
    start = break_starts,
    end = break_ends,
    duration = as.difftime(gap_durations, units = time_unit)
  )

  return(breaks_df)
}


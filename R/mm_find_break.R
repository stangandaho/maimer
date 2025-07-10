#' Detect time gaps in a datetime series
#'
#' Identifies breaks in a sequence of datetime observations based on a specified time threshold.
#'
#' @param data A data frame containing the datetime column.
#' @param datetime_column The datetime column.
#' @param format Optional. A character string specifying the datetime format, passed to \code{as.POSIXlt}.
#' @param threshold A numeric value indicating the minimum gap to be considered a break (default is 10).
#' @param time_unit The unit for the threshold. Supported values include "secs", "mins", "hours", "days", and "weeks".
#'
#' @return A tibble with columns `start`, `end`, and `duration`
#' showing the start and end of each break and its length.
#'
#' @examples
#' library(dplyr)
#' pene <- read.csv(maimer:::table_files()[1])
#'
#' set_cam <- pene %>%
#'   dplyr::filter(camera == "CAMERA 3")
#'
#' mm_find_break(data = pene, datetime_column = "datetimes",
#' threshold = 5, time_unit = "days")
#'
#' @export
mm_find_break <- function(data,
                          datetime_column,
                          format,
                          threshold = 10,
                          time_unit = "hours") {

  # Check if the right time unit is provided
  time_unit <- time_unit_parser(time_unit)

  datetime_series <- data %>% dplyr::pull({{ datetime_column }})
  # Ensure datetime_series is POSIXct

  # Coerce to POSIXlt if necessary
  if (!inherits(datetime_series, "POSIXt")) {
    datetime_series <- as.POSIXlt(datetime_series, format = format,
                                  tryFormats = try_formats)
  }

  # Sort the datetime_series to ensure proper order
  datetime_series <- sort(datetime_series)

  # Compute time differences between consecutive times
  time_diffs <- base::diff(datetime_series, units = time_unit)

  # Identify breaks where the difference exceeds the threshold
  breaks_idx <- which(time_diffs > threshold)

  # Create a data frame of breaks
  if (length(breaks_idx) == 0) {
    cli::cli_warn("No deployment met this {.strong {.emph {threshold}} {time_unit}} threshold")
    return(invisible())
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


time_unit_parser <- function(time_unit) {
  time_units <- c("s" = "secs", "secs" = "secs", "seconds" = "secs", "sec" = "secs", "second" = "secs",
                  "m" = "mins", "min" = "mins", "mins" = "mins", "minute" = "mins", "minutes" = "mins",
                  "h" = "hours", "hour" = "hours", "hours" = "hours",
                  "d" = "days", "day" = "days", "days" = "days",
                  "w" = "weeks", "week" = "weeks", "weeks" = "weeks")
  time_unit <- tolower(time_unit)
  if (! time_unit %in% unique(names(time_units))) {
    cli::cli_abort(message = sprintf("Time unit '%s' not supported!", time_unit),
                   call = NULL)
  }
  time_unit <- time_units[[time_unit]]
  return(time_unit)
}


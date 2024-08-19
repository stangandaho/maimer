#' Convert Time to Diel Fraction
#'
#' The `mm_to_diel` function converts a time string into a diel fraction,
#' which represents the time as a proportion of a 24-hour day. This is particularly
#' useful for analyzing timing and duration of activities throughout a day.
#'
#' @param times A vector of time `as.POSIXct`, `strptime` or `strings`. Each string can represent a time in the format "HH:MM:SS" or variations thereof. The time may also include additional information, such as dates, which will be ignored in the conversion process.
#' @param sep A character string specifying the separator used in the `times` input. The separator could be ":", "-", or any character that separates hours, minutes, and seconds.
#'
#' @return A numeric vector of diel fractions, with each value ranging from 0 to 1, where:
#' - `0` represents midnight (start of the day)
#' - `0.5` represents noon (midday)
#' - `1` represents the end of the day (just before the next midnight)
#'
#' @details
#' The function splits the `times` strings using the specified `sep` and any
#' whitespace, extracts the hours, minutes, and seconds, and then converts them
#' into a fraction of a 24-hour day (diel fraction). If more than three elements (e.g., "YYYY-MM-DD HH:MM:SS") are found, the function will only use the last three elements (assumed to be "HH:MM:SS").
#'
#' @examples
#' # Example usage
#' times <- c("12:34:56", "23:45:00", "2024-08-18 06:30:15")
#' sep <- ":"
#' diel_times <- mm_to_diel(times, sep)
#' print(diel_times)
#'
#' @note
#' The function assumes that the time strings are well-formed and will ignore empty or malformed components.
#'
#' @export

mm_to_diel <- function(times, sep) {

   tms <- trimws(as.character(times))
  x <- strsplit(tms, split = paste0(sep, "|\\s"))
  tms_vec <- list()
  for (i in 1:length(x)) {
    trim <- trimws(x[[i]]); trim <- trim[trim!= ""]
    if (length(trim) > 3) {
      i_ <- trim[ (length(trim) - 2):length(trim) ]

    }else{
      i_ <- trim
    }
    time_num <- suppressWarnings(as.numeric(i_))

    h <- time_num[1]; m <- time_num[2]/60; s <- time_num[3]/3600
    tms_vec[[i]] <- ifelse(all(is.na(c(h, m, s))), NA, sum(c(h, m, s), na.rm = TRUE)/24)
  }

  return(unlist(tms_vec))
}

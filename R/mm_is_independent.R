#' Evaluate Event Independence
#'
#' This function calculates the difference between times and evaluates whether
#' events are independent based on a given threshold. It is useful for checking
#' if certain events in a dataset are independent of each other based on time intervals.
#'
#' @param data A `data.frame`, `tbl_df`, or `tbl` containing the event data. This should include
#'   a column with datetime values. If `NULL`, the function will use the `deltatime` argument
#'   instead of the `data` argument.
#' @param datetime A `character` string specifying the name of the column in `data` that contains
#'   the datetime values. This argument is required if `data` is provided.
#' @param format A `character` string defining the format used to parse the datetime values in
#'   the `datetime` column. This argument is required if `data` is provided and should be
#'   in the format recognized by [strptime()].
#' @param deltatime A `numeric` vector of time differences between successive events. This argument
#'   is used if `data` is not provided. If `data` is provided, this argument is ignored.
#' @param threshold A `numeric` value representing the time difference threshold (in seconds) to
#'   determine whether events are independent. Events are considered independent if the time
#'   difference between them is greater than or equal to this threshold. The default is 30 minutes
#'   (1800 seconds).
#' @param only A `logical` value indicating whether to return only the rows of `data` that are
#'   identified as independent events. If `TRUE`, only independent events are returned. If `FALSE`,
#'   the entire data frame is returned with an additional column indicating the independence status.
#'   The default is `TRUE`.
#'
#' @return
#' - If `data` is provided and `only` is `TRUE`, a tibble of events identified as independent.
#' - If `data` is provided and `only` is `FALSE`, a tibble of the original data with additional columns
#'   indicating the `independent` status and `deltatime` differences.
#' - If `data` is not provided, a tibble of the `deltatime` values with `independent` status.
#'
#' @examples
#' # Example with a data frame
#' df <- data.frame(datetime = as.POSIXct(c("2024-08-01 10:00:00", "2024-08-01 10:15:00",
#'                                          "2024-08-01 10:45:00", "2024-08-01 11:00:00")),
#'                  value = c(1, 2, 3, 4))
#' result <- mm_is_independent(data = df, datetime = "datetime", format = "%Y-%m-%d %H:%M:%S")
#'
#' # Example with deltatime
#' deltatime <- c(1800, 1800, 3600)
#' result <- mm_is_independent(deltatime = deltatime)
#'
#' @export

mm_is_independent <- function(data = NULL,
                              datetime = "",
                              format = "",
                              deltatime = NULL,
                              threshold = 30*60,
                              only = TRUE) {

  if (any(class(data) %in% c("data.frame", "tbl_df", "tbl"))) {

    if (!hasArg(datetime)) {
      stop("`datetime`, cannot be missed")
    }

    if (!hasArg(format)) {
      stop("`format` cannot be missed")
    }

    data <- data %>% dplyr::arrange(!!dplyr::sym(datetime))
    deltatimes <- strptime(data[[datetime]], format = format)
    sq <- data[[datetime]]

  }else{
    deltatimes <- deltatime
  }

  # Error for incorrect format
  if (all(is.na(deltatimes))) {
    stop(sprintf("%s is ambiguous format", format))
  }
  # warning for ambiguous datetime
  if (!all(is.na(deltatimes))) {
    if (any(is.na(deltatimes))) {
      na_date <- sq[is.na(deltatimes)]
      warning(sprintf("The following datetime are ambiguous: %s", paste0(na_date, collapse = " ")))
    }
  }

  # Use deltatime argument if provided instead
  if (hasArg(deltatime) & !hasArg(data)) {
    deltatime_th <- which(deltatime >= threshold)
  } else{
    deltatime_th <- which(c(0, diff(deltatimes)) >= threshold)
  }

  if (length(deltatime_th) > 0) {
    index <- deltatime_th
    index_moins <- deltatime_th - 1
    all_index <- sort(c(index_moins, index))
    event <- 1:length(deltatimes) %in% all_index

    if (class(data) %in% c("data.frame", "tbl_df", "tbl")) {
      data[["independent"]] <- event
      data[["deltatime"]] <- c(0, diff(deltatimes))

      if (only) {
        dtr <- subset(data, independent == TRUE)
        dtr <- dtr[, which(colnames(dtr) != "independent")]
        return(dplyr::as_tibble(dtr))
      }else{
        return(dplyr::as_tibble(data))
      }

    }else{
      return(dplyr::as_tibble(data.frame(deltatime = deltatime, independent = event)))
    }

  }else{
    message(sprintf("Any independent event found with threshold of %s", threshold))
  }

}

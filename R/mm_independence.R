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
#'   the `datetime` column.
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
#' result <- mm_independence(data = df, datetime = "datetime", format = "%Y-%m-%d %H:%M:%S")
#' result
#'
#' @export

mm_independence <- function(data = NULL,
                           datetime,
                           format,
                           threshold = 30*60,
                           only = FALSE) {

  # Prevent all possible error
  if (!is.null(data)) {
    if (!any(class(data) %in% c("data.frame", "tbl_df", "tbl"))){
      stop("Wrong data provided")
    }

    dt_str_ <- ifelse(methods::hasArg(datetime), paste0(dplyr::ensym(datetime)), "datetime")

    if (!any(dt_str_ %in% colnames(data))) {
      stop(sprintf("%s not found in data", dt_str_))
    }
  }

  if (!hasArg(datetime)) {
    stop("datetime must be provided")
  }

  if (!hasArg(format)) {
    stop("format cannot be missed")
  }

  ## Get datetime and build new data
  if (hasArg(data)) {
    original_datetime <- data[[dt_str_]]
    dt_str_ <- paste0(dplyr::ensym(datetime))
    data[[dt_str_]] <- strptime(data[[dplyr::ensym(datetime)]], format = format)
    data <- data %>%
      dplyr::arrange(!!dplyr::sym(dt_str_)) %>%
      dplyr::rename(datetime = !!dplyr::sym(dt_str_)) %>%
      dplyr::as_tibble()

  }else{
    original_datetime <- datetime
    data <- dplyr::tibble('{datetime}' := strptime(datetime, format = format)) %>%
      dplyr::arrange(datetime)
  }

  # Error for incorrect format
  if (all(is.na(data$datetime))) {
    stop(sprintf("%s is ambiguous format", format))
  }

  # warning for ambiguous datetime
  if (!all(is.na(data$datetime))) {
    if (any(is.na(data$datetime))) {
      na_date <- original_datetime[is.na(data$datetime)]
      warning(sprintf("The following datetime are ambiguous: %s", paste0(na_date, collapse = ", ")),
              call. = FALSE)
    }
  }

  # Data with deltatime and event
  data$deltatime <- c(0, diff(data$datetime))
  data$event <- c(TRUE, diff(data$datetime) >= threshold)

  if (only) {
    data <- data %>%
      dplyr::filter(event == TRUE) %>%
      dplyr::select(-event)
  }

  return(data)
}

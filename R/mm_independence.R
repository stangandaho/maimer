#' Evaluate event independence
#'
#' This function calculates the difference between times and evaluates whether
#' events are independent based on a given threshold. It is useful for checking
#' if certain events in a dataset are independent of each other based on time intervals.
#'
#' @param data A `data.frame`, `tbl_df`, or `tbl` containing the event data. This should include
#'   a column with datetime values. If `NULL`, the function will use the `deltatime` argument
#'   instead of the `data` argument.
#' @param species_column An optional column name specifying the species grouping.
#'   If provided, independence will be assessed separately within each species group.
#' @param site_column An optional column name specifying the site/camera grouping.
#'   If provided, independence will be assessed separately within each site group.
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
#'
#' library(dplyr)
#'
#' # Load example dataset
#' cam_data <- read.csv(system.file("penessoulou_season1.csv", package = "maimer"))
#'
#' # Independence without considering species occurrence
#' indep1 <- cam_data %>%
#'   mm_independence(data = ., datetime = datetimes, format = "%Y-%m-%d %H:%M:%S",
#'                   only = TRUE)
#'
#' sprintf("Independent observations: %s", nrow(indep1))
#'
#' # Independence considering species occurrence
#' indep2 <- cam_data %>%
#'   mm_independence(data = ., datetime = datetimes, format = "%Y-%m-%d %H:%M:%S",
#'                   only = TRUE, species_column = "species")
#'
#' sprintf("Independent observations: %s", nrow(indep2))
#'
#' # Use a standalone vector of datetime values
#' dtime <- cam_data$datetimes
#' mm_independence(datetime = dtime, format = "%Y-%m-%d %H:%M:%S", only = TRUE)
#'
#' @export

mm_independence <- function(data = NULL,
                            species_column,
                            site_column,
                            datetime,
                            format,
                            threshold = 30*60,
                            only = FALSE) {

  # Prevent all possible error
  if (!is.null(data)) {
    if (!any(class(data) %in% c("data.frame", "tbl_df", "tbl"))){
      rlang::abort("Wrong data provided")
    }

    dt_str_ <- ifelse(methods::hasArg(datetime), as.character(dplyr::ensym(datetime)), "datetime")

    if (!any(dt_str_ %in% colnames(data))) {
      rlang::abort(sprintf("%s not found in data", dt_str_))
    }
  }

  if (!hasArg(datetime)) {
    rlang::abort("'datetime' must be provided")
  }

  if (!hasArg(format)) {
    rlang::abort("'format' cannot be missed")
  }

  ## Get datetime and build new data
  if (hasArg(data)) {

    dt_str_ <- as.character(dplyr::ensym(datetime))
    data$original_datetime <- data[[dt_str_]] # original_datetime to handle warning
    data[[dt_str_]] <- strptime(data[[dplyr::ensym(datetime)]], format = format)
    data <- data %>%
      dplyr::arrange(!!dplyr::sym(dt_str_)) %>%
      dplyr::rename(datetime = !!dplyr::sym(dt_str_)) %>%
      dplyr::as_tibble()

  }else{
    original_datetime <- datetime
    data <- dplyr::tibble(original_datetime = datetime,
                          datetime = strptime(datetime, format = format)) %>%
      dplyr::arrange(datetime)
  }

  # Error for incorrect format
  if (all(is.na(data$datetime))) {
    rlang::abort(sprintf("%s is ambiguous format", format))
  }

  # warning for ambiguous datetime
  if (!all(is.na(data$datetime))) {
    if (any(is.na(data$datetime))) {
      na_date <- data$original_datetime[is.na(data$datetime)]
      is_are <- ifelse(length(na_date) >= 2, "are", "is")
      rlang::warn(sprintf("The following datetime %s ambiguous: %s", is_are, paste0(na_date, collapse = ", ")))
    }
  }


  # Apply grouping by site and species if provided
  site_column <- tryCatch(as.character(dplyr::ensym(site_column)), error = function(e)NULL)
  species_column <- tryCatch(as.character(dplyr::ensym(species_column)), error = function(e)NULL)
  grouped_by <- c(site_column, species_column)
  grouped_by <- grouped_by[grouped_by != ""]

  if (length(grouped_by) > 0) {
    ## Confirm column presence
    missed_col_error(data = data, grouped_by)

    data <- data %>%
      dplyr::group_by(!!!rlang::syms(grouped_by)) %>%
      dplyr::arrange(datetime, .by_group = TRUE) %>%
      dplyr::mutate(deltatime = c(0, diff(datetime)),
                    event = c(TRUE, diff(datetime) >= threshold)) %>%
      dplyr::ungroup() %>%
      dplyr::select(-original_datetime)
  } else {
    data <- data %>%
      dplyr::arrange(datetime) %>%
      dplyr::mutate(deltatime = c(0, diff(datetime)),
                    event = c(TRUE, diff(datetime) >= threshold)) %>%
      dplyr::select(-original_datetime)
  }

  if (only) {
    data <- data %>%
      dplyr::filter(event == TRUE) %>%
      dplyr::select(-c(event, deltatime))
  }

  return(data)
}


#' Convert camera trap data to occupancy format
#'
#' This function transforms camera trap detection data into an occupancy format
#' suitable for analysis. It aggregates detections into user-defined time windows
#' and optionally converts counts into presence-absence (0/1) data.
#'
#' @param data A data frame containing camera trap detection records.
#' @param date_column The name of the column containing detection dates.
#' @param format a character string. If not specified when converting from a
#' character representation, it will try c("%Y-%m-%d", "%Y/%m/%d") one by one,
#' and give an error if none works. Otherwise, the processing is
#' via [strptime()] whose help page describes available conversion specifications.
#' @param site_column The name of the column identifying sampling sites.
#' @param species_column The name of the column containing species names.
#' Can be NULL if species information is not needed.
#' @param size_column The name of the column representing detection counts.
#' @param by_day An integer specifying the number of days per time window (default: `7`).
#' @param presence_absence Logical. If `TRUE`, converts counts to presence-absence
#' data (1 = detected, 0 = not detected). Default is `TRUE`.
#'
#' @return A wide-format data frame where rows represent sites (and optionally species),
#' and columns represent detection windows. Values indicate either detection counts
#' or presence-absence (0/1).
#'
#' @seealso [mm_to_community()]
#'
#'
#' @examples
#'
#' data <- data.frame(
#'   date = c("01-01-2023", "03-01-2023", "10-01-2023", "15-01-2023"),
#'   site = c("A", "A", "B", "B"),
#'   species = c("Tiger", "Tiger", "Deer", "Deer"),
#'   count = c(1, 2, 3, 1)
#' )
#'
#' occupancy_data <- mm_to_occupancy(
#'   data,
#'   date_column = date,
#'   site_column = site,
#'   species_column = species,
#'   size_column = count,
#'   by_day = 7,
#'   presence_absence = TRUE
#' )
#'
#' occupancy_data
#'
#'
#' @import dplyr tidyr rlang
#'
#' @export
mm_to_occupancy <- function(data,
                            date_column,
                            format = "%Y-%m-%d",
                            site_column,
                            species_column,
                            size_column,
                            by_day = 7,
                            presence_absence = TRUE
) {

  date_col <- site_col <- species_col <- size_col <- NULL
  try({date_col <- rlang::as_name(rlang::enquo(date_column))}, silent = TRUE)
  try({site_col <- rlang::as_name(rlang::enquo(site_column))}, silent = TRUE)
  try({species_col <- rlang::as_name(rlang::enquo(species_column))}, silent = TRUE)
  try({size_col <- rlang::as_name(rlang::enquo(size_column))}, silent = TRUE)

  if (is.null(size_col)) {rlang::abort("Size column is required to identify detection.")}
  if (is.null(site_col)) {rlang::abort("Site column must be provided.")}
  if (is.null(date_col)) {rlang::abort("Date column is required.")}

  # Select part of data
  cols <- c(date_col, site_col, species_col, size_col)
  data <- data %>%
    dplyr::select(dplyr::all_of(cols))

  # Select and convert date
  data[[date_col]] <- as.Date(as.character(data[[date_col]]), format = format)

  # Create date windows based on the user-defined interval
  min_date <- min(data[[date_col]])
  max_date <- max(data[[date_col]])
  date_windows <- seq(min_date, max_date, by = paste(by_day, "days"))

  # Create labels for the date windows
  window_labels <- paste(date_windows[-length(date_windows)],
                         date_windows[-1] - 1, sep = " to ")

  # Assign each date to a window
  data$window <- cut(data[[date_col]], breaks = date_windows,
                     labels = window_labels, right = FALSE)

  # Create a complete grid of all combinations of site, species, and window
  if(!is.null(species_col)){
    complete_grid <- expand.grid(
      site = unique(data[[site_col]]),
      species = unique(data[[species_col]]),
      window = window_labels,
      stringsAsFactors = FALSE
    ) %>% dplyr::rename("{site_col}" := site, "{species_col}" := species)

  }else{
    complete_grid <- expand.grid(
      site = unique(data[[site_col]]),
      window = window_labels,
      stringsAsFactors = FALSE
    ) %>% dplyr::rename("{site_col}" := site)
  }

  # Aggregate the data by site, species, and window
  formula_ <- paste(size_col, "~", paste0(c(cols[!cols %in% c(date_col, size_col)], "window"), collapse = " + "))
  formula_ <- as.formula(formula_)
  aggregated_data <- aggregate(formula_, data = data, sum)

  # Merge the aggregated data with the complete grid to ensure all combinations are present
  complete_data <- merge(complete_grid, aggregated_data, by = c(site_col, species_col, "window"), all.x = TRUE)
  complete_data[is.na(complete_data[[size_col]]), size_col] <- 0

  # Reshape the data to wide format
  wide_data <- complete_data %>%
    tidyr::pivot_wider(names_from = window, values_from = !!size_col, values_fill = 0)

  # Produce 0/1 data if accepted
  remaining_col <- cols[! cols %in% c(size_col, date_col)]
  window_cols <- colnames(wide_data)[! colnames(wide_data) %in% remaining_col]

  if (presence_absence) {
    wide_data[, window_cols] <- ifelse(wide_data[, window_cols] > 1, 1, 0)
  }

  return(wide_data)
}

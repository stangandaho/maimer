#' Plot Camera Trap Activity History
#'
#' Creates a ggplot showing camera trap deployment activity periods over time.
#' Each deployment is represented on the x-axis with vertical bars (like error bars)
#' showing active periods on the y-axis (time). Gaps in activity are automatically
#' detected and displayed as breaks in the vertical bars.
#'
#' @param data A data frame containing camera trap data with datetime information.
#' @param deployment_col Character. Column name containing deployment/camera identifiers.
#' @param datetime_col Character. Column name containing datetime information.
#' @param gap_threshold Numeric. Minimum gap duration to be considered a break in activity.
#' @param gap_time_unit Character. Time unit for gap threshold. Options: "secs", "mins",
#'   "hours", "days", "weeks". Default is "days".
#' @param datetime_format Character. Optional datetime format string for parsing.
#' @param min_records_per_period Numeric. Minimum number of records required to define
#'   an active period. Default is 1.
#' @param bar_width Numeric. Width of the activity bars. Default is 0.8.
#' @param bar_alpha Numeric. Transparency of activity bars (0-1). Default is 0.7.
#' @param gap_color Character. Color for gap indicators. Default is "red".
#' @param activity_color Character. Color for activity bars. Default is "steelblue".
#' @param show_gaps Logical. Whether to show gap indicators. Default is TRUE.
#' @param rotate_x_labels Logical. Whether to rotate x-axis labels. Default is TRUE.
#'
#' @return A ggplot object showing camera trap activity history.
#'
#' @examples
#' \dontrun{
#' # Load camera trap data
#' camtrap_data <- read_camtrap_dp("path/to/data")
#'
#' # Plot activity history from media data
#' plot_camtrap_activity(
#'   data = camtrap_data$media,
#'   deployment_col = "deploymentID",
#'   datetime_col = "timestamp",
#'   gap_threshold = 7,
#'   gap_time_unit = "days"
#' )
#'
#' # Customize the plot
#' p <- plot_camtrap_activity(camtrap_data$media, "deploymentID", "timestamp")
#' p +
#'   labs(title = "Camera Trap Activity History",
#'        subtitle = "Blue bars show active periods, red dots show gaps") +
#'   theme_minimal()
#' }
#'
#' @export
mm_plot_camtrap_activity <- function(data,
                                  deployment_col,
                                  datetime_col,
                                  gap_threshold = 5,
                                  gap_time_unit = "days",
                                  datetime_format = NULL,
                                  min_records_per_period = 1,
                                  bar_width = 0.8,
                                  bar_alpha = 0.7,
                                  gap_color = "red",
                                  activity_color = "steelblue",
                                  show_gaps = TRUE,
                                  rotate_x_labels = TRUE) {

  # Load required packages
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for this function.")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required for this function.")
  }
  if (!requireNamespace("lubridate", quietly = TRUE)) {
    stop("Package 'lubridate' is required for this function.")
  }

  # Input validation
  if (!deployment_col %in% names(data)) {
    stop("Column '", deployment_col, "' not found in data.")
  }
  if (!datetime_col %in% names(data)) {
    stop("Column '", datetime_col, "' not found in data.")
  }

  # Prepare data
  plot_data <- data %>%
    dplyr::select(deployment = !!rlang::sym(deployment_col),
                  datetime = !!rlang::sym(datetime_col)) %>%
    dplyr::filter(!is.na(deployment), !is.na(datetime))

  # Convert datetime if needed
  if (!inherits(plot_data$datetime, "POSIXt")) {
    if (!is.null(datetime_format)) {
      plot_data$datetime <- as.POSIXct(plot_data$datetime, format = datetime_format)
    } else {
      # Try common formats
      try_formats <- c("%Y-%m-%d %H:%M:%OS", "%Y/%m/%d %H:%M:%OS",
                       "%Y:%m:%d %H:%M:%OS", "%Y-%m-%d %H:%M",
                       "%Y/%m/%d %H:%M", "%Y:%m:%d %H:%M",
                       "%Y-%m-%d", "%Y/%m/%d", "%Y:%m:%d")
      plot_data$datetime <- as.POSIXct(plot_data$datetime, tryFormats = try_formats)
    }
  }

  # Remove rows with failed datetime parsing
  plot_data <- plot_data %>% dplyr::filter(!is.na(datetime))

  if (nrow(plot_data) == 0) {
    stop("No valid datetime data found after parsing.")
  }

  # Calculate activity periods for each deployment
  activity_periods <- plot_data %>%
    dplyr::group_by(deployment) %>%
    dplyr::arrange(datetime) %>%
    dplyr::summarise(
      n_records = dplyr::n(),
      first_record = min(datetime, na.rm = TRUE),
      last_record = max(datetime, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::filter(n_records >= min_records_per_period)

  if (nrow(activity_periods) == 0) {
    stop("No deployments meet the minimum records threshold.")
  }

  # Find gaps for each deployment
  gap_data <- NULL
  if (show_gaps) {
    gap_data <- plot_data %>%
      dplyr::group_by(deployment) %>%
      dplyr::group_modify(~ {
        gaps <- mm_find_break(
          data = .x,
          datetime = "datetime",
          threshold = gap_threshold,
          time_unit = gap_time_unit
        )
        return(gaps)
      }) %>%
      dplyr::ungroup()
  }

  # Calculate active periods (periods between gaps)
  active_periods <- plot_data %>%
    dplyr::group_by(deployment) %>%
    dplyr::group_modify(~ {
      calc_active_periods(.x, gap_threshold, gap_time_unit)
    }) %>%
    dplyr::ungroup()

  # Create factor for deployment ordering
  deployment_order <- activity_periods %>%
    dplyr::arrange(first_record) %>%
    dplyr::pull(deployment)

  active_periods$deployment <- factor(active_periods$deployment, levels = deployment_order)
  if (!is.null(gap_data) && nrow(gap_data) > 0) {
    gap_data$deployment <- factor(gap_data$deployment, levels = deployment_order)
  }

  # Create the base plot
  p <- ggplot2::ggplot() +
    ggplot2::geom_linerange(
      data = active_periods,
      ggplot2::aes(x = deployment, ymin = period_start, ymax = period_end),
      size = bar_width * 2,
      alpha = bar_alpha,
      color = activity_color
    ) +
    ggplot2::scale_y_datetime(
      name = "Date/Time",
      date_labels = "%Y-%m-%d",
      date_breaks = "1 month"
    ) +
    ggplot2::scale_x_discrete(
      name = "Deployment/Camera ID"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank()
    )

  # Add gap indicators if requested and gaps exist
  if (show_gaps && !is.null(gap_data) && nrow(gap_data) > 0) {
    p <- p +
      ggplot2::geom_point(
        data = gap_data,
        ggplot2::aes(x = deployment, y = start),
        color = gap_color,
        size = 2,
        shape = 17  # Triangle
      ) +
      ggplot2::geom_point(
        data = gap_data,
        ggplot2::aes(x = deployment, y = end),
        color = gap_color,
        size = 2,
        shape = 17  # Triangle
      )
  }

  # Rotate x-axis labels if requested
  if (rotate_x_labels) {
    p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  }

  return(p)
}

#' Calculate Active Periods for Camera Trap Data
#'
#' Internal helper function to calculate continuous active periods based on gap detection.
#'
#' @param data A data frame with datetime column for a single deployment.
#' @param gap_threshold Numeric. Gap threshold for detecting breaks.
#' @param gap_time_unit Character. Time unit for gap threshold.
#'
#' @return A tibble with period_start and period_end columns.
#' @keywords internal
calc_active_periods <- function(data, gap_threshold, gap_time_unit) {

  if (nrow(data) == 0) {
    return(dplyr::tibble(period_start = as.POSIXct(character(0)),
                         period_end = as.POSIXct(character(0))))
  }

  # Sort data by datetime
  data <- data %>% dplyr::arrange(datetime)

  # Find gaps
  gaps <- mm_find_break(
    data = data,
    datetime = "datetime",
    threshold = gap_threshold,
    time_unit = gap_time_unit
  )

  if (nrow(gaps) == 0) {
    # No gaps - single continuous period
    return(dplyr::tibble(
      period_start = min(data$datetime, na.rm = TRUE),
      period_end = max(data$datetime, na.rm = TRUE)
    ))
  }

  # Create periods based on gaps
  all_times <- sort(data$datetime)
  period_starts <- c(min(all_times))
  period_ends <- c()

  for (i in 1:nrow(gaps)) {
    # End of current period is just before gap start
    period_ends <- c(period_ends, gaps$start[i])
    # Start of next period is at gap end
    period_starts <- c(period_starts, gaps$end[i])
  }

  # Add final period end
  period_ends <- c(period_ends, max(all_times))

  # Create periods tibble
  periods <- dplyr::tibble(
    period_start = period_starts,
    period_end = period_ends
  ) %>%
    dplyr::filter(period_start < period_end)  # Remove invalid periods

  return(periods)
}

#' Create Activity Summary Statistics
#'
#' Calculates summary statistics for camera trap activity periods.
#'
#' @param data A data frame containing camera trap data.
#' @param deployment_col Character. Column name for deployment identifiers.
#' @param datetime_col Character. Column name for datetime information.
#' @param gap_threshold Numeric. Gap threshold for detecting activity breaks.
#' @param gap_time_unit Character. Time unit for gap threshold.
#' @param datetime_format Character. Optional datetime format string.
#'
#' @return A tibble with activity summary statistics for each deployment.
#'
#' @examples
#' \dontrun{
#' # Get activity summary
#' activity_stats <- summarise_camtrap_activity(
#'   data = camtrap_data$media,
#'   deployment_col = "deploymentID",
#'   datetime_col = "timestamp",
#'   gap_threshold = 7,
#'   gap_time_unit = "days"
#' )
#'
#' print(activity_stats)
#' }
#'
#' @export
summarise_camtrap_activity <- function(data,
                                       deployment_col,
                                       datetime_col,
                                       gap_threshold = 5,
                                       gap_time_unit = "days",
                                       datetime_format = NULL) {

  # Prepare data
  plot_data <- data %>%
    dplyr::select(deployment = !!rlang::sym(deployment_col),
                  datetime = !!rlang::sym(datetime_col)) %>%
    dplyr::filter(!is.na(deployment), !is.na(datetime))

  # Convert datetime if needed
  if (!inherits(plot_data$datetime, "POSIXt")) {
    if (!is.null(datetime_format)) {
      plot_data$datetime <- as.POSIXct(plot_data$datetime, format = datetime_format)
    } else {
      try_formats <- c("%Y-%m-%d %H:%M:%OS", "%Y/%m/%d %H:%M:%OS",
                       "%Y:%m:%d %H:%M:%OS", "%Y-%m-%d %H:%M",
                       "%Y/%m/%d %H:%M", "%Y:%m:%d %H:%M",
                       "%Y-%m-%d", "%Y/%m/%d", "%Y:%m:%d")
      plot_data$datetime <- as.POSIXct(plot_data$datetime, tryFormats = try_formats)
    }
  }

  # Calculate summary statistics
  summary_stats <- plot_data %>%
    dplyr::group_by(deployment) %>%
    dplyr::group_modify(~ {
      gaps <- mm_find_break(
        data = .x,
        datetime = "datetime",
        threshold = gap_threshold,
        time_unit = gap_time_unit
      )

      active_periods <- calc_active_periods(.x, gap_threshold, gap_time_unit)

      total_duration <- as.numeric(
        max(.x$datetime, na.rm = TRUE) - min(.x$datetime, na.rm = TRUE),
        units = "days"
      )

      active_duration <- sum(as.numeric(
        active_periods$period_end - active_periods$period_start,
        units = "days"
      ))

      gap_duration <- total_duration - active_duration

      dplyr::tibble(
        n_records = nrow(.x),
        first_record = min(.x$datetime, na.rm = TRUE),
        last_record = max(.x$datetime, na.rm = TRUE),
        total_duration_days = round(total_duration, 2),
        active_duration_days = round(active_duration, 2),
        gap_duration_days = round(gap_duration, 2),
        activity_rate = round(active_duration / total_duration * 100, 1),
        n_gaps = nrow(gaps),
        n_active_periods = nrow(active_periods),
        avg_gap_duration_days = if(nrow(gaps) > 0) {
          round(mean(as.numeric(gaps$duration, units = "days")), 2)
        } else { 0 }
      )
    }) %>%
    dplyr::ungroup()

  return(summary_stats)
}

#' Plot Activity Summary Dashboard
#'
#' Creates a multi-panel plot showing camera trap activity patterns and statistics.
#'
#' @param data A data frame containing camera trap data.
#' @param deployment_col Character. Column name for deployment identifiers.
#' @param datetime_col Character. Column name for datetime information.
#' @param gap_threshold Numeric. Gap threshold for detecting activity breaks.
#' @param gap_time_unit Character. Time unit for gap threshold.
#' @param datetime_format Character. Optional datetime format string.
#'
#' @return A ggplot object with activity dashboard.
#'
#' @examples
#' \dontrun{
#' # Create activity dashboard
#' dashboard <- plot_activity_dashboard(
#'   data = camtrap_data$media,
#'   deployment_col = "deploymentID",
#'   datetime_col = "timestamp"
#' )
#'
#' print(dashboard)
#' }
#'
#' @export
plot_activity_dashboard <- function(data,
                                    deployment_col,
                                    datetime_col,
                                    gap_threshold = 5,
                                    gap_time_unit = "days",
                                    datetime_format = NULL) {

  if (!requireNamespace("patchwork", quietly = TRUE)) {
    warning("Package 'patchwork' recommended for dashboard layout. Using single plot instead.")
    return(plot_camtrap_activity(data, deployment_col, datetime_col,
                                 gap_threshold, gap_time_unit, datetime_format))
  }

  # Main activity plot
  activity_plot <- plot_camtrap_activity(
    data = data,
    deployment_col = deployment_col,
    datetime_col = datetime_col,
    gap_threshold = gap_threshold,
    gap_time_unit = gap_time_unit,
    datetime_format = datetime_format
  ) +
    ggplot2::labs(title = "Camera Trap Activity History") +
    ggplot2::theme(axis.title.x = ggplot2::element_blank())

  # Get summary statistics
  stats <- summarise_camtrap_activity(
    data = data,
    deployment_col = deployment_col,
    datetime_col = datetime_col,
    gap_threshold = gap_threshold,
    gap_time_unit = gap_time_unit,
    datetime_format = datetime_format
  )

  # Activity rate plot
  rate_plot <- stats %>%
    ggplot2::ggplot(ggplot2::aes(x = reorder(deployment, -activity_rate), y = activity_rate)) +
    ggplot2::geom_col(fill = "steelblue", alpha = 0.7) +
    ggplot2::labs(
      title = "Activity Rate by Deployment",
      x = "Deployment ID",
      y = "Activity Rate (%)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  # Gap count plot
  gap_plot <- stats %>%
    ggplot2::ggplot(ggplot2::aes(x = reorder(deployment, -n_gaps), y = n_gaps)) +
    ggplot2::geom_col(fill = "coral", alpha = 0.7) +
    ggplot2::labs(
      title = "Number of Gaps by Deployment",
      x = "Deployment ID",
      y = "Number of Gaps"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  # Combine plots using patchwork
  combined_plot <- activity_plot / (rate_plot | gap_plot)

  return(combined_plot)
}

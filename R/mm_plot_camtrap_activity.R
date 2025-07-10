#' Plot camera trap activity over time
#'
#' Visualizes the activity history of camera trap deployments to show periods of data capture.
#' It also optionally highlights periods of inactivity (break/gap).
#'
#' @inheritParams mm_find_break
#' @param deployment_column Column name (unquoted) that identifies the deployment or camera ID.
#' @param activity_style A list controlling the appearance of active periods. Can include:
#'   - `linewidth`: Line width (default 0.8)
#'   - `color`: Color of activity bars (default `"steelblue"`)
#'   - `alpha`: Transparency (default 0.7)
#'   - `linetype`: Line type (default 1)
#' @param break_style A list controlling the appearance of gaps/inactive periods. Can include:
#'   - `linewidth`: Line width (default 0.8)
#'   - `color`: Color of gap bars (default `"#c90026"`)
#'   - `alpha`: Transparency (default 0.9)
#'   - `linetype`: Line type (default 1)
#' @param show_gaps Logical. If `TRUE` (default), shows vertical bars for detected gaps in deployment activity.
#' @param ylabel_format Character. Format for y-axis date-time labels. Default is `"%Y-%m-%d"`.
#' @param ybreak Character. Spacing for y-axis breaks, e.g., `"1 days"` or `"12 hours"`. Default is based on `time_unit`.
#'
#' @return A `ggplot2` object showing periods of activity (and optionally gaps) for each deployment.
#'
#' @examples
#' # Load example data and filter for one project
#' camtrap_data <- read.csv(maimer:::table_files()[1]) %>%
#'   dplyr::filter(project == "Last")
#'
#' # Plot with default styles
#' mm_plot_camtrap_activity(
#'   data = camtrap_data,
#'   deployment_column = camera,
#'   datetime_column = datetimes,
#'   threshold = 7,
#'   time_unit = "days"
#' )
#'
#' #' # Customize plot appearance
#' mm_plot_camtrap_activity(
#'   data = camtrap_data,
#'   deployment_column = camera,
#'   datetime_column = "datetimes",
#'   threshold = 15,
#'   time_unit = "days",
#'   ybreak = "3 days",
#'   activity_style = list(width = 1.1, color = "gray10")
#' )+
#'   ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
#' #'
#' @export
mm_plot_camtrap_activity <- function(data,
                                     deployment_column,
                                     datetime_column,
                                     threshold = 5,
                                     time_unit = "days",
                                     format = NULL,
                                     activity_style = list(width = 0.8, color = "steelblue", alpha = 0.7, linetype = 1),
                                     break_style = list(width = 0.8, color = "#c90026", alpha = 0.9, linetype = 1),
                                     show_gaps = TRUE,
                                     ylabel_format = "%Y-%m-%d",
                                     ybreak = paste(1, time_unit)
                                     ) {

  # Turn of warning when no gap is found
  options(warn = -1)

  # Prepare data
  plot_data <- data %>%
    dplyr::select({{deployment_column}}, {{datetime_column}}) %>%
    dplyr::rename(deployment = 1, datetime = 2) %>%
    dplyr::filter(!is.na(deployment), !is.na(datetime))

  # Convert datetime if needed
  if (!inherits(plot_data$datetime, "POSIXt")) {
    if (!is.null(format)) {
      plot_data$datetime <- as.POSIXct(plot_data$datetime, format = format)
    } else {
      # Try common formats
      plot_data$datetime <- as.POSIXct(plot_data$datetime, tryFormats = try_formats)
    }
  }

  # Remove rows with failed datetime parsing
  plot_data <- plot_data %>% dplyr::filter(!is.na(datetime))

  if (nrow(plot_data) == 0) {
    cli::cli_abort("No valid datetime data found after parsing.")
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
    )

  # Find gaps for each deployment
  gap_data <- lapply(unique(plot_data$deployment), function(x){
    fb <- maimer::mm_find_break(data = plot_data %>% dplyr::filter(deployment == x),
                  datetime_column = datetime,
                  threshold = threshold,
                  time_unit = time_unit)
    if (!is.null(fb)) {
      fb %>% dplyr::mutate(deployment = x)
    }

  }) %>%  dplyr::bind_rows()

  if (nrow(gap_data) <= 0) {
    gap_data <- NULL
  }

  # Calculate active periods (periods between gaps)
  active_periods <- lapply(unique(plot_data$deployment), function(x){
    ap <- calc_active_periods(data = plot_data %>% dplyr::filter(deployment == x),
                        threshold = threshold, time_unit = time_unit) %>%
      dplyr::mutate(deployment = x)

  }) %>% dplyr::bind_rows()

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
      mapping = ggplot2::aes(x = deployment, ymin = period_start, ymax = period_end),
      linewidth = ifelse(is.null(activity_style$linewidth), 0.8, activity_style$linewidth),
      alpha = ifelse(is.null(activity_style$alpha), 0.7, activity_style$alpha),
      color = ifelse(is.null(activity_style$color), "steelblue", activity_style$color),
      linetype = ifelse(is.null(break_style$linetype), 1, break_style$linetype)
    ) +
    ggplot2::scale_y_datetime(
      name = "Period",
      date_labels = ylabel_format,
      date_breaks = ybreak
    ) +
    ggplot2::scale_x_discrete(
      name = "Deployment"
    ) +
    ggplot2::theme_minimal()+
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  # Add gap indicators if requested and gaps exist
  if (show_gaps && !is.null(gap_data) && nrow(gap_data) > 0) {
    p <- p +
      ggplot2::geom_linerange(
        data = gap_data,
        mapping = ggplot2::aes(x = deployment, ymin = start, ymax = end),
        linewidth = ifelse(is.null(break_style$linewidth), 0.8, break_style$linewidth),
        alpha = ifelse(is.null(break_style$alpha), 0.9, break_style$alpha),
        color = ifelse(is.null(break_style$color), "#c90026", break_style$color),
        linetype = ifelse(is.null(break_style$linetype), 2, break_style$linetype)
      )

  }

  # Turn on warning when no gap is found
  options(warn = -1)
  return(p)
}


#' Calculate active periods for camera trap data
#'
#' Internal helper function to calculate continuous active periods based on gap detection.
#'
#' @inheritParams mm_find_break
#' @return A tibble with period_start and period_end columns.
#' @keywords internal
calc_active_periods <- function(data, threshold, time_unit) {

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
    threshold = threshold,
    time_unit = time_unit
  )

  if (is.null(gaps)) {
    # No gaps - single continuous period
    return(dplyr::tibble(
      period_start = min(data$datetime, na.rm = TRUE),
      period_end = max(data$datetime, na.rm = TRUE)
    ))
  }

  # Create periods based on gaps
  all_times <- sort(data$datetime)
  period_starts <- list(min(all_times))
  period_ends <- list()
  for (i in 1:nrow(gaps)) {
    # End of current period is just before gap start
    period_ends[[i]] <- gaps$start[i]

    # Start of next period is at gap end
    period_starts[[i+1]] <- gaps$end[i]
  }

  # Add final period end
  period_ends[[length(period_ends)+1]] <- max(all_times)

  # Convert back to proper datetime format
  period_starts <- as.POSIXct(unlist(period_starts), origin = "1970-01-01", tz = attr(all_times, "tzone"))
  period_ends <- as.POSIXct(unlist(period_ends), origin = "1970-01-01", tz = attr(all_times, "tzone"))

  # Create periods tibble
  periods <- dplyr::tibble(
    period_start = period_starts,
    period_end = period_ends
  ) %>%
    dplyr::filter(period_start < period_end)  # Remove invalid periods

  return(periods)
}

#' Create activity summary statistics
#'
#' Calculates summary statistics for camera trap activity periods.
#'
#' @inheritParams mm_find_break
#' @param deployment_column Character. Column name for deployment identifiers.
#'
#' @return A tibble with activity summary statistics for each deployment.
#'
#' @examples
#' # Get activity summary
#' camtrap_data <- read.csv(maimer:::table_files()[1]) %>%
#' dplyr::filter(project == "Last")
#'
#'   mm_summarise_camtrap_activity(data = camtrap_data,
#'                                 deployment_column = "camera",
#'                                 datetime_column = datetimes,
#'                                 threshold = 15,
#'                                 time_unit = "days")
#'
#' @export
mm_summarise_camtrap_activity <- function(data,
                                          deployment_column,
                                          datetime_column,
                                          threshold = 5,
                                          time_unit = "days",
                                          format = NULL) {


  # Prepare data
  plot_data <- data %>%
    dplyr::select({{deployment_column}}, {{datetime_column}}) %>%
    dplyr::rename(deployment = 1, datetime = 2) %>%
    dplyr::filter(!is.na(deployment), !is.na(datetime))

  # Convert datetime if needed
  if (!inherits(plot_data$datetime, "POSIXt")) {
    if (!is.null(format)) {
      plot_data$datetime <- as.POSIXct(plot_data$datetime, format = format)
    } else {
      plot_data$datetime <- as.POSIXct(plot_data$datetime, tryFormats = try_formats)
    }
  }

  # Calculate summary statistics
  summary_stats <- lapply(unique(plot_data$deployment), function(x){
    sm_df <- plot_data %>%
      dplyr::filter(deployment == x)


      gaps <- mm_find_break(
        data = sm_df,
        datetime_column = datetime,
        threshold = threshold,
        time_unit = time_unit
      )
      active_periods <- calc_active_periods(sm_df, threshold, time_unit)
      total_duration <- as.numeric(
        max(sm_df$datetime, na.rm = TRUE) - min(sm_df$datetime, na.rm = TRUE),
        units = "days"
      )

      active_duration <- sum(as.numeric(
        active_periods$period_end - active_periods$period_start,
        units = "days"
      ))

      gap_duration <- total_duration - active_duration

      dplyr::tibble(
        {{deployment_column}} := x,
        n_records = nrow(sm_df),
        first_record = min(sm_df$datetime, na.rm = TRUE),
        last_record = max(sm_df$datetime, na.rm = TRUE),
        total_duration = round(total_duration, 2),
        active_duration = round(active_duration, 2),
        break_duration = round(gap_duration, 2),
        activity_rate = round(active_duration / total_duration * 100, 1),
        n_breaks = if(!is.null(gaps))0 else nrow(gaps),
        n_active_periods = nrow(active_periods),
        avg_break_duration = if(!is.null(gaps) && nrow(gaps) > 0) {
          round(mean(as.numeric(gaps$duration)), 2)
        } else { 0 }
      )

  }) %>% dplyr::bind_rows()


  return(summary_stats)
}

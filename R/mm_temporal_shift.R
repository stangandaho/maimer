#' Calculate the temporal shift of one species' activity over two periods
#'
#' This function estimates and analyzes the temporal shift in the activity of a
#' species between two time periods using kernel density estimation. It computes
#' the activity distributions and determines the magnitude and direction of the shift.
#'
#' @param first_period A numeric vector representing activity times in radians for the first period.
#' @param second_period A numeric vector representing activity times in radians for the second period.
#' @param convert_time Logical. If `TRUE`, converts times to radians before analysis.
#' @inheritParams mm_plot_overlap
#' @param width_at Numeric. The fraction of maximum density at which the activity width is measured (default is 0.5).
#' @param format Character. Format of time input (default is "%H:%M:%S"). Used if `convert_time = TRUE`.
#' @param time_zone Character. Time zone for time conversion. Required if `convert_time = TRUE`.
#' @param plot Logical. If `TRUE`, generates a plot comparing the activity distributions of the two periods.
#' @param linestyle_1 List. Line style settings for the first period's density plot. Includes `linetype`, `linewidth`, and `color`.
#' @param linestyle_2 List. Line style settings for the second period's density plot. Includes `linetype`, `linewidth`, and `color`.
#' @param posestyle_1 List. Marker style settings for the first period's density range. Includes `shape`, `size`, `color`, and `alpha`.
#' @param posestyle_2 List. Marker style settings for the second period's density range. Includes `shape`, `size`, `color`, and `alpha`.
#' @param ... Additional arguments (currently unused).
#'
#' @return A list containing:
#'
#'  **A tibble with:**
#'   - `First period range`: The start and end times of active periods in the first dataset.
#'   - `Second period range`: The start and end times of active periods in the second dataset.
#'   - `Shift size (in hour)`: The absolute difference in activity duration between the two periods.
#'   - `Move`: A categorical description of the shift ("Forward", "Backward", "Expanded", "Contracted", etc.).
#'
#'  **`A plot`** (optional): A `ggplot2` object visualizing the density distributions if `plot = TRUE`.
#'
#'
#' @examples
#' library(ggplot2)
#' # Using radians as input
#'
#' first_period <- c(1.3, 2.3, 2.5, 5.2, 6.1, 2.3)  # Example timestamps for period 1
#' second_period <- c(1.8, 2.2, 2.5)  # Example timestamps for period 2
#' result <- mm_temporal_shift(first_period, second_period, plot = TRUE, xcenter = "noon",
#'                             linestyle_1 = list(color = "gray10", linetype = 1, linewidth = 1),
#'                             linestyle_2 = list(color = "#b70000", linetype = 5, linewidth = .5))
#' result
#'
#' # customize the graph associated result
#' result$plot+
#'   labs(color = "Periods")+
#'   theme(legend.position = "top")
#'
#' # Using time strings as input
#' first_period <- c("12:03:05", "13:10:09", "14:08:10", "14:18:30", "18:22:11")
#' second_period <- c("13:00:20", "14:20:10", "15:55:20", "16:03:01", "16:47:00")
#' result <- mm_temporal_shift(first_period, second_period,
#'                             convert_time = TRUE,
#'                             format = "%H:%M:%S",
#'                             time_zone = "UTC")
#'
#'
#' @import dplyr ggplot2
#' @export


mm_temporal_shift <- function(first_period,
                              second_period,
                              convert_time = FALSE,
                              xscale = 24,
                              xcenter = c("noon", "midnight"),
                              n_grid = 128,
                              kmax = 3,
                              adjust = 1,
                              width_at = 1/2,
                              format = "%H:%M:%S",
                              time_zone,
                              plot = TRUE,
                              linestyle_1 = list(),
                              linestyle_2 = list(),
                              posestyle_1 = list(),
                              posestyle_2 = list(),
                              ...) {

  if (convert_time) {
    first_period <- mm_to_radian(times = first_period, format = format, time_zone = time_zone)
    second_period <- mm_to_radian(times = second_period, format = format, time_zone = time_zone)
  }

  if (width_at < 0 | width_at > 1) {
    stop(sprintf("The width %f is out of [0, 1]", width_at))
  }


  check_density_input(first_period)
  check_density_input(second_period)
  xcenter <- match.arg(xcenter)
  isMidnt <- xcenter == "midnight"

  # Bandwidth calculation
  bwA <- overlap::getBandWidth(first_period, kmax = kmax) / adjust
  bwB <- overlap::getBandWidth(second_period, kmax = kmax) / adjust
  if (is.na(bwA) || is.na(bwB)) rlang::abort("Bandwidth estimation failed.", call = NULL)

  # Create a sequence of values for density estimation
  xsc <- if (is.na(xscale)) 1 else xscale / (2 * pi)
  xxRad  <- seq(0, 2 * pi, length.out = n_grid)
  if (isMidnt) xxRad  <- xxRad - pi

  xx <- xxRad * xsc

  # Kernel Density Estimation for period 1
  kde_times1 <- overlap::densityFit(first_period, xxRad, bwA)
  # Kernel Density Estimation for period 2
  kde_times2 <- overlap::densityFit(second_period, xxRad, bwB)

  # Calculate the range or spread of the KDE (e.g., width at half maximum)
  fwhm_range1 <- range(xx[kde_times1 > max(kde_times1)*width_at])
  times_min1 <- fwhm_range1[1]
  times_max1 <- fwhm_range1[2]
  width1 <- times_max1 - times_min1

  fwhm_range2 <- range(xx[kde_times2 > max(kde_times2)*width_at])
  times_min2 <- fwhm_range2[1]
  times_max2 <- fwhm_range2[2]
  width2 <- times_max2 - times_min2

  #Y
  ytime1 <- min(kde_times1[kde_times1 > max(kde_times1)*width_at])
  ytime2 <- min(kde_times2[kde_times2 > max(kde_times2)*width_at])

  # Compare the results
  fp = mm_to_time(abs(c(times_min1, times_max1))/xsc)
  fp_formated <- convert_to_hour(fp)

  sp = mm_to_time(abs(c(times_min2, times_max2))/xsc)
  sp_formated <- convert_to_hour(sp)

  temporal_shift <- list(
    `First period range` = paste0(fp, collapse = " - "),
    `Second period range` = paste0(sp, collapse = " - "),
    `Shift size (in hour)` = round(
      as.numeric(
        abs((fp_formated[2] - fp_formated[1]) - (sp_formated[2] - sp_formated[1]))
      ), 2
    )
  )

  if (times_max2 > times_max1 & times_min2 > times_min1) {
    shift <- "Forward"  # Entire range shifts forward
  } else if (times_max2 < times_max1 & times_min2 < times_min1) {
    shift <- "Backward"  # Entire range shifts backward
  } else if (times_max2 > times_max1 & times_min2 < times_min1) {
    shift <- "Unlarged"  # Range expands (spreads out)
  } else if (times_max2 < times_max1 & times_min2 > times_min1) {
    shift <- "Contracted"  # Range contracts (narrows)
  } else if (times_max2 == times_max1 & times_min2 == times_min1) {
    shift <- "Constant"  # No change in range
  } else if (times_max2 > times_max1 & times_min2 == times_min1) {
    shift <- "Forward Edge"  # Forward shift at max while min remains constant
  } else if (times_max2 == times_max1 & times_min2 < times_min1) {
    shift <- "Backward Edge"  # Backward shift at min while max remains constant
  } else if (times_max2 < times_max1 & times_min2 == times_min1) {
    shift <- "Contracted Edge (Max)"  # Max contracts while min remains constant
  } else if (times_max2 == times_max1 & times_min2 > times_min1) {
    shift <- "Contracted Edge (Min)"  # Min contracts while max remains constant
  } else if (times_max2 > times_max1 & times_min2 > times_min1) {
    shift <- "Forward and Expanded"  # Range shifts forward and expands
  } else if (times_max2 < times_max1 & times_min2 < times_min1) {
    shift <- "Backward and Contracted"  # Range shifts backward and contracts
  } else {
    shift <- "Undefined"  # Fallback case, unlikely to happen
  }

  temporal_shift[["Move"]] <- shift
  data <- dplyr::tibble(xx = xx, kde_times1 = kde_times1, kde_times2 = kde_times2)
  pose_data <- dplyr::tibble(times_min1 = times_min1,
                          times_max1 = times_max1,
                          times_min2 = times_min2,
                          times_max2 = times_max2,
                          ytime1 = ytime1,
                          ytime2 = ytime2
                          )


  # Plot KDEs for visualization
  linestyle_1 <- list(linetype = ifelse(!is.null(linestyle_1$linetype), linestyle_1$linetype, 1),
                    linewidth = ifelse(!is.null(linestyle_1$linewidth), linestyle_1$linewidth, 1),
                    color = ifelse(!is.null(linestyle_1$color), linestyle_1$color, "#c90026"))

  linestyle_2 <- list(linetype = ifelse(!is.null(linestyle_2$linetype), linestyle_2$linetype, 3),
                    linewidth = ifelse(!is.null(linestyle_2$linewidth), linestyle_2$linewidth, 1),
                    color = ifelse(!is.null(linestyle_2$color), linestyle_2$color, "gray10"))

  posestyle_1 <- list(shape = ifelse(!is.null(posestyle_1$shape), posestyle_1$shape, 19),
                      size = ifelse(!is.null(posestyle_1$size), posestyle_1$size, 3),
                      color = ifelse(!is.null(posestyle_1$color), posestyle_1$color, "#c90026"),
                      alpha = ifelse(!is.null(posestyle_1$alpha), posestyle_1$alpha, 1))

  posestyle_2 <- list(shape = ifelse(!is.null(posestyle_2$shape), posestyle_2$shape, 19),
                      size = ifelse(!is.null(posestyle_2$size), posestyle_2$size, 3),
                      color = ifelse(!is.null(posestyle_2$color), posestyle_2$color, "gray10"),
                      alpha = ifelse(!is.null(posestyle_2$alpha), posestyle_2$alpha, 1))

  data <- data %>%
    dplyr::rename(Time = xx, `Period 1` = kde_times1, `Period 2` = kde_times2) %>%
    tidyr::pivot_longer(cols = !Time, names_to = "Period", values_to = "Density")
  data$Period <- factor(data$Period, levels = c('Period 1', "Period 2"))

  p <- ggplot2::ggplot(data = data)+
    ggplot2::geom_line(ggplot2::aes(x = Time, y = `Density`, color = Period,
                                    linetype = Period, linewidth = Period))+
    ggplot2::guides(linetype = "none", linewidth = "none")+
    ggplot2::scale_color_manual(values = c(linestyle_1$color, linestyle_2$color))+
    ggplot2::scale_linetype_manual(values = c(linestyle_1$linetype, linestyle_2$linetype))+
    ggplot2::scale_linewidth_manual(values = c(linestyle_1$linewidth, linestyle_2$linewidth))+
    # Add items for first time
    ggplot2::geom_point(data = pose_data, ggplot2::aes(x = times_min1, y = ytime1),
                        size = posestyle_1$size, color = posestyle_1$color,
                        shape = posestyle_1$shape, alpha = posestyle_1$alpha)+
    ggplot2::geom_point(data = pose_data, ggplot2::aes(x = times_max1, y = ytime1),
                        size = posestyle_1$size, color = posestyle_1$color,
                        shape = posestyle_1$shape, alpha = posestyle_1$alpha)+
    ggplot2::geom_segment(data = pose_data, ggplot2::aes(x = times_min1, y = ytime1,
                                       xend = times_max1, yend = ytime1),
                          linetype = linestyle_1$linetype, linewidth = linestyle_1$linewidth,
                          color = linestyle_1$color)+
    # Add items for second time
    ggplot2::geom_point(data = pose_data, ggplot2::aes(x = times_min2, y = ytime2),
                        size = posestyle_2$size, color = posestyle_2$color,
                        shape = posestyle_2$shape, alpha = posestyle_2$alpha)+
    ggplot2::geom_point(data = pose_data, ggplot2::aes(x = times_max2, y = ytime2),
                        size = posestyle_2$size, color = posestyle_2$color,
                        shape = posestyle_2$shape, alpha = posestyle_2$alpha)+
    ggplot2::geom_segment(data = pose_data, ggplot2::aes(x = times_min2, y = ytime2,
                                       xend = times_max2, yend = ytime2),
                          linetype = linestyle_2$linetype, linewidth = linestyle_2$linewidth,
                          color = linestyle_2$color)

  # Add axis customization
  # Customizing x-axis labels to show correct time, even for negative times
  p <- p +
    ggplot2::scale_x_continuous(
      breaks = if(xcenter == "noon"){seq(0, 24, 2)}else{seq(-12, 12, 2)},
      labels = function(x) {
        x <- ifelse(x < 0, 24 + x, x)
        hours <- floor(x)
        sprintf("%02d:00", hours %% 24)
      })+
    ggplot2::theme_minimal()


  if (plot) {
    print(p)
    return(list(dplyr::bind_cols(temporal_shift), plot = p))
  }


  return(dplyr::bind_cols(temporal_shift))
}



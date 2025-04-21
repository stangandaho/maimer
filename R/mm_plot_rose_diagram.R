#' Plot a 24-hour rose diagram of daily activity
#'
#' This function generates a rose diagram (circular bar plot) to visualize
#' daily activity patterns over a 24-hour period. Each bar represents either
#' the absolute or relative frequency of observations within hourly intervals.
#' The plot also includes a segment indicating the mean activity time, and
#' an optional segment showing the 95% confidence interval of the activity period.
#'
#' @param data A data frame containing the time values. If `NULL`, `times` must be provided as a vector.
#' @param times A numeric vector of time values (in radians) or a column name from `data`.
#' @param frequencies Character. Use `"absolute"` to show counts or `"relative"` to show percentages. Default is `"absolute"`.
#' @param hide_lebels Logical. If `TRUE`, frequency value labels on top of bars are hidden. Default is `FALSE`.
#' @param label_position Numeric. Controls vertical position of the frequency value labels (if shown).
#' @param label_style A list of styles for labels. Accepts `color`, `size`, and `family`.
#' @param time_range Numeric. Width of the time bins in hours. Default is `1` (hourly bins).
#' @param ci_segment Logical or numeric. If `TRUE`, a segment representing the 95% confidence interval is added.
#' If numeric, this value sets the length of the CI ticks. Default is `TRUE`.
#' @param mean_segment Logical. If `TRUE`, a segment representing the mean time is added. Default is `TRUE`.
#' @param ring Logical or numeric vector. If `TRUE`, a default ring range is set.
#' If a numeric vector of length 2 is provided, sets custom inner and outer limits of the radial axis.
#' @param color Color of the bar border and segments. Default is `"gray20"`.
#' @param fill Fill color of the bars. Default is the same as `color`.
#' @param ci_style A list of styles for the confidence interval segment. Accepts `color`, `linetype`, and `linewidth`.
#' @param mean_style A list of styles for the mean segment. Accepts `color`, `linetype`, and `linewidth`.
#' @param start Numeric. The angle (in radians) where the polar plot starts. Default is `-0.12`.
#' @param width Numeric. Width of each bar. Default is `NULL`, which uses the default width from `geom_col()`.
#'
#' @return A `ggplot` object representing the rose diagram.
#'
#' @examples
#' set.seed(129)
#' library(dplyr)
#' library(ggplot2)
#'
#' rf <- runif(123, 0, max = 6)
#'
#' mm_plot_rose_diagram(data = NULL,
#'                      times = rf,
#'                      frequencies = "relative",
#'                      label_style = list(size = 4, color = 'red'),
#'                      label_position = 11,
#'                      time_range = 1,
#'                      mean_segment = TRUE,
#'                      ci_segment = 1,
#'                      ring = c(-5, 12),
#'                      color = 'gray20',
#'                      mean_style = list(linetype = 1, linewidth = .5, color = 'red'),
#'                      ci_style = list(linetype = 1, linewidth = .5, color = 'black')
#' )
#' @export


mm_plot_rose_diagram <- function(data = NULL,
                                 times,
                                 frequencies = "absolute",
                                 hide_lebels = FALSE,
                                 label_position = NULL,
                                 label_style = list(),
                                 time_range = 1,
                                 ci_segment = TRUE, # Confidence Interval Segment Length
                                 mean_segment = TRUE,
                                 ring = TRUE,
                                 color = "gray20",
                                 fill = color,
                                 ci_style = list(),
                                 mean_style = list(),
                                 start = -0.12,
                                 width = NULL
                                 ) {

  # Be sure column exists and set times value
  if (!is.null(data)) {
    get_column(data, as.character(rlang::ensym(times)))
    times <- data[[as.character(rlang::ensym(times))]]
  }else{
    times <- times
  }

  # Calculate time average and Confidence intervals
  mean_time <- round(mean(times, na.rm = TRUE), 3)
  ci <- confidence_interval(x = times)

  # Pass time to POSIXct
  timed <- strptime(times %>% mm_to_time(), format = "%H:%M:%S") %>%
    sort()
  # Create class for timed
  cuted <- cut(x = timed, breaks = paste0(round(time_range), " hour"),
               start.on.monday = FALSE,
               right = FALSE)

  ccount <- table(cuted)
  ori_time <- lapply(strsplit(names(ccount), split = "\\s"), function(x){
    x[2]
  }) %>% unlist()

  ddtf <- data.frame(day_time = ori_time, ori_freq = as.numeric(ccount))

  aday <- data.frame(day_time = paste0(sprintf("%02d", seq(0, 23, by = time_range)), ":00:00")) %>%
    dplyr::left_join(y = ddtf, by = "day_time") %>%
    dplyr::mutate(ori_freq = if_else(is.na(ori_freq), 0, ori_freq),
           radian = round(mm_to_radian(times = day_time), 3),
           day_time = substr(day_time, 1, 5))

  # Switch frequence
  if (frequencies != "absolute") {
    aday <- aday %>% dplyr::mutate(ori_freq = round((ori_freq/sum(ori_freq))*100, 1))
  }

  # PLOT
  # Set breaks and labels for x-axis
  hour_labels <- aday$day_time
  hour_breaks <- aday$radian
  scalor <- max(aday$ori_freq)*0.05
  mean_yend <- ci_yend <- max(aday$ori_freq) + scalor

  p <- ggplot2::ggplot(data = aday, mapping = aes(x = radian))+
    ggplot2::geom_col(mapping = aes(y = ori_freq), color = color, fill = fill, width = width)+
    ggplot2::scale_x_continuous(breaks = hour_breaks, labels = hour_labels)+
    ggplot2::labs(x = "", y = "")

  # if mean_segment
  if (mean_segment) {
    color <- ifelse(is.null(mean_style$color), 'gray20', mean_style$color)
    linetype <- ifelse(is.null(mean_style$linetype), 2, mean_style$linetype)
    linewidth <- ifelse(is.null(mean_style$linewidth), .6, mean_style$linewidth)
    p <- p+
      ggplot2::annotate(geom = 'segment', x = mean_time, xend = mean_time,
               y = min(aday$ori_freq), yend = mean_yend,
               color = color, linetype = linetype, linewidth = linewidth)
  }
  # if ci_segment
  if (ci_segment) {
    seg_len <- ifelse(is.numeric(ci_segment), ci_segment, max(aday$ori_freq)*0.02)

    color <- ifelse(is.null(ci_style$color), 'gray20', ci_style$color)
    linetype <- ifelse(is.null(ci_style$linetype), 2, ci_style$linetype)
    linewidth <- ifelse(is.null(ci_style$linewidth), .6, ci_style$linewidth)
    p <- p+
      ggplot2::annotate(geom = 'segment', x = ci[1], xend = ci[2], y = ci_yend,
               color = color, linetype = linetype, linewidth = linewidth)+
      ggplot2::annotate(geom = 'segment', x = ci[1],
               y = ci_yend - seg_len/2, yend = ci_yend + seg_len/2,
               color = color, linetype = linetype, linewidth = linewidth)+
      ggplot2::annotate(geom = 'segment', x = ci[2],
               y = ci_yend - seg_len/2, yend = ci_yend + seg_len/2,
               color = color, linetype = linetype, linewidth = linewidth)
  }

  # If text
  if (!hide_lebels) {
    lp <- if (!is.null(label_position)){label_position}else{mean_yend + 2*scalor}
    labels <- if(frequencies == "relative"){paste0(aday$ori_freq, "%")}else{paste0(aday$ori_freq)}
    color <- ifelse(is.null(label_style$color), 'gray20', label_style$color)
    size <- ifelse(is.null(label_style$size), 3, label_style$size)
    family <- ifelse(is.null(label_style$family), NA, label_style$family)
    p <- p +
      ggplot2::geom_text(mapping = aes(y = lp, label = labels),
                color = color, size = size, family = family)
  }

  # If an ring
  if (suppressWarnings(any(ring))) {
    if (any(c(is.na(ring[1]), is.na(ring[2])))) {
      rg1 <- -max(aday$ori_freq)/2; rg2 <- max(aday$ori_freq) + max(aday$ori_freq)*0.2
    }else{
      rg1 <- ring[1]; rg2 <- ring[2]
      seg_len <- ifelse(is.numeric(ci_segment), ci_segment, max(aday$ori_freq)*0.02)
      if (rg2 < ci_yend + seg_len/2) {
        rlang::abort(sprintf("Rigth limit scale must be greater than %s, not %s.", ci_yend + seg_len/2, rg2))
      }
    }
    # Convert to polar coordinates
    p <- p + ggplot2::coord_polar(start = start) + ylim(rg1, rg2)
  }else{
    p <- p + ggplot2::coord_polar(start = start)
  }


  # Final plot
  fplot <- p +
    ggplot2::theme(
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.minor.x = element_blank()
    )

  return(fplot)

}




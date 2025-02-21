#' Plot overlap between two species' activity patterns
#'
#' This function visualizes the temporal overlap between two species' activity patterns based on time-of-day data.
#' It uses kernel density estimation to estimate activity densities and highlights areas of overlap between the two species.
#'
#' @param time A numeric vector of time-of-day observations (in radians, 0 to \eqn{2\pi}).
#' @param xscale A numeric value to scale the x-axis. Default is `24` for representing time in hours.
#' @param xcenter A string indicating the center of the x-axis. Options are `"noon"` (default) or `"midnight"`.
#' @param n_grid An integer specifying the number of grid points for density estimation. Default is `128`.
#' @param kmax An integer indicating the maximum number of modes allowed in the activity pattern. Default is `3`.
#' @param adjust A numeric value to adjust the bandwidth of the kernel density estimation. Default is `1`.
#' @param rug A logical value indicating whether to include a rug plot of the observations. Default is `FALSE`.
#' @param linetype A numeric specifying the line types. Default is `c(1, 2)`.
#' @param linecol A string specifying the colors of the density lines for species A and B. Default is `c("gray10", "gray0")`.
#' @param linewidth A numeric value specifying the line widths for species A and B density lines. Default is `c(1, 1)`.
#' @param rug_lentgh A numeric value specifying the length of the rug ticks. Default is `0.018` (in normalized plot coordinates).
#' @param rug_color A string specifying the color of the rug ticks. Default is `"gray30"`.
#' @param extend A string specifying the color of the extended area beyond the activity period. Default is `"lightgrey"`.
#' @param extend_alpha A numeric value (0 to 1) for the transparency of the extended area. Default is `0.8`.
#' @param ... Additional arguments passed to the `geom_rug` function.
#'
#' @return A ggplot object representing the activity density curves of the species.
#'
#' @examples
#' \dontrun{
#'   # Generate random data for two species
#'   set.seed(42)
#'   A <- runif(100, 0, 2 * pi)
#'
#'   # Plot overlap with default settings
#'   mm_plot_density(A)
#'
#'   # Customize plot with specific colors and line types
#'    mm_plot_density(A, linecol = "gray10", linewidth = 0.8,
#'                    xcenter = "midnight", rug = T,
#'                    rug_color = 'red', extend_alpha = 0)
#'
#' }
#'
#' @import ggplot2
#' @import overlap
#' @export
#'
mm_plot_density <- function(time,
                            xscale = 24,
                            xcenter = c("noon", "midnight"),
                            n_grid = 128,
                            kmax = 3,
                            adjust = 1,
                            rug = FALSE,
                            linetype = 2,
                            linecol = "gray10",
                            linewidth = 1,
                            rug_lentgh = 0.018,
                            rug_color = "gray30",
                            extend = "lightgrey",
                            extend_alpha = 0.8,
                            ...
                        ) {

  suppressWarnings({

    # Input validation
    check_density_input(A)
    xcenter <- match.arg(xcenter)
    isMidnt <- xcenter == "midnight"

    # Bandwidth calculation
    bwA <- overlap::getBandWidth(A, kmax = kmax) / adjust
    if (is.na(bwA)) stop("Bandwidth estimation failed.")

    # Create a sequence of values for density estimation
    xsc <- if (is.na(xscale)) 1 else xscale / (2 * pi)

    if (is.null(extend)) {
      xxRad  <- seq(0, 2 * pi, length.out = n_grid)
    } else {
      xxRad  <- seq(-pi/4, 9 * pi/4, length.out = n_grid)
    }

    if (isMidnt) xxRad  <- xxRad - pi

    xx <- xxRad * xsc

    # Density estimation
    densA <- overlap::densityFit(A, xxRad, bwA)/xsc

    # Create polygon data for filled area (ensure it closes)
    poly_df <- data.frame(x = xx , y = densA)

    # Base ggplot object
    p <- ggplot2::ggplot()+
      ggplot2::geom_line(data = poly_df, mapping = ggplot2::aes(x = x, y = y),
                   color = linecol, linetype = linetype, linewidth = linewidth)+
      ggplot2::labs(x = "\nTime", y = "Density\n") +
      ggplot2::theme_minimal()+
      ggplot2::theme(
        axis.line = ggplot2::element_line(linewidth = 0.5, color = "gray10"),
        axis.text = ggplot2::element_text(size = 12),
        axis.title = ggplot2::element_text(size = 14),
        axis.ticks = ggplot2::element_line(linewidth = 0.2, color = "gray10")
      )

    # Add rug plot if requested
    if (rug) {
      if (isMidnt) {
        A <- ifelse(A < pi, A, A - 2 * pi)
      }
      p <- p + ggplot2::geom_rug(data = data.frame(A = A * xsc), ggplot2::aes(x = A),
                        length = ggplot2::unit(rug_lentgh, "npc"),
                        na.rm = TRUE, color = rug_color,
                        sides = "b", inherit.aes = FALSE, ...)
    }

    # Extend the plot if required
    if (!is.null(extend)) {
      if (isMidnt) {
        wrap <- c(-pi, pi) * xsc
      } else {
        wrap <- c(0, 2 * pi) * xsc
      }
      p <- p + ggplot2::annotate("rect", xmin = -Inf, xmax = wrap[1], ymin = -Inf, ymax = Inf,
                        fill = extend, alpha = extend_alpha) +
        ggplot2::annotate("rect", xmin = wrap[2], xmax = Inf, ymin = -Inf, ymax = Inf,
                 fill = extend, alpha = extend_alpha)

    }

    # Add axis customization
    # Customizing x-axis labels to show correct time, even for negative times
    p <- p +
      ggplot2::scale_x_continuous(
        breaks = if(xcenter == "noon"){seq(0, 24, 2)}else{seq(-12, 12, 2)},
        labels = function(x) {
          x <- ifelse(x < 0, 24 + x, x)
          hours <- floor(x)
          sprintf("%02d:00", hours %% 24)
        })


    return(p)

  })# suppress
}

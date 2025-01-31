#' Plot overlap between two species' activity patterns
#'
#' This function visualizes the temporal overlap between two species' activity patterns based on time-of-day data.
#' It uses kernel density estimation to estimate activity densities and highlights areas of overlap between the two species.
#'
#' @param A A numeric vector of time-of-day observations (in radians) for species A.
#' @param B A numeric vector of time-of-day observations (in radians) for species B.
#' @param xscale A numeric value to scale the x-axis. Default is `24` for representing time in hours.
#' @param xcenter A string indicating the center of the x-axis. Options are `"noon"` (default) or `"midnight"`.
#' @param n_grid An integer specifying the number of grid points for density estimation. Default is `128`.
#' @param kmax An integer indicating the maximum number of modes allowed in the activity pattern. Default is `3`.
#' @param adjust A numeric value to adjust the bandwidth of the kernel density estimation. Default is `1`.
#' @param rug A logical value indicating whether to include a rug plot of the observations. Default is `FALSE`.
#' @param overlap_color A string specifying the color of the overlap area. Default is `"gray40"`.
#' @param overlap_alpha A numeric value (0 to 1) for the transparency of the overlap area. Default is `0.8`.
#' @param linetype A vector of integers specifying the line types for species A and B density lines. Default is `c(1, 2)`.
#' @param linecol A vector of strings specifying the colors of the density lines for species A and B. Default is `c("gray10", "gray0")`.
#' @param linewidth A vector of numeric values specifying the line widths for species A and B density lines. Default is `c(1, 1)`.
#' @param overlap_only A logical value indicating whether to plot only the overlap region without individual density lines. Default is `FALSE`.
#' @param rug_lentgh A numeric value specifying the length of the rug ticks. Default is `0.018` (in normalized plot coordinates).
#' @param rug_color A string specifying the color of the rug ticks. Default is `"gray30"`.
#' @param extend A string specifying the color of the extended area beyond the activity period. Default is `"lightgrey"`.
#' @param extend_alpha A numeric value (0 to 1) for the transparency of the extended area. Default is `0.8`.
#' @param ... Additional arguments passed to the `geom_rug` function.
#'
#' @return A ggplot object representing the activity density curves and overlap between the two species.
#' If `overlap_only = TRUE`, only the overlap region is displayed.
#'
#' @details
#' This function calculates kernel density estimates for two sets of time-of-day data (`A` and `B`)
#' and visualizes their overlap. The time-of-day data should be expressed in radians (0 to 2π).
#'
#' Key features of the plot:
#' - The x-axis can be scaled to represent hours (default `xscale = 24`) or other units.
#' - The center of the x-axis can be adjusted to `"noon"` or `"midnight"`.
#' - A shaded polygon highlights the overlap region between the two density curves.
#' - Optionally includes a rug plot to indicate raw observations.
#' - Allows customization of line types, colors, widths, and transparency.
#' - Extended regions (beyond the primary activity period) can be highlighted for clarity.
#'
#' @examples
#' \dontrun{
#'   # Generate random data for two species
#'   set.seed(42)
#'   species_A <- runif(100, 0, 2 * pi)
#'   species_B <- runif(100, 0, 2 * pi)
#'
#'   # Plot overlap with default settings
#'   mm_plot_overlap(A = species_A, B = species_B)
#'
#'   # Customize plot with specific colors and line types
#'   mm_plot_overlap(A = species_A, B = species_B, overlap_color = "blue", linecol = c("red", "green"))
#'
#'   # Include rug plots and change transparency
#'   mm_plot_overlap(A = species_A, B = species_B, rug = TRUE, overlap_alpha = 0.5)
#' }
#'
#' @import ggplot2
#' @import overlap
#' @export
mm_plot_overlap <- function(A,
                            B,
                            xscale = 24,
                            xcenter = c("noon", "midnight"),
                            n_grid = 128,
                            kmax = 3,
                            adjust = 1,
                            rug = FALSE,
                            overlap_color = "gray40",
                            overlap_alpha = 0.8,
                            linetype = c(1, 2),
                            linecol = c("gray10", "gray0"),
                            linewidth = c(1, 1),
                            overlap_only = FALSE,
                            rug_lentgh = 0.018,
                            rug_color = "gray30",
                            extend = "lightgrey",
                            extend_alpha = 0.8,
                            ...
                        ) {

  suppressWarnings({

    # Input validation
    overlap:::checkInput(A)
    overlap:::checkInput(B)
    xcenter <- match.arg(xcenter)
    isMidnt <- xcenter == "midnight"

    # Bandwidth calculation
    bwA <- overlap::getBandWidth(A, kmax = kmax) / adjust
    bwB <- overlap::getBandWidth(B, kmax = kmax) / adjust
    if (is.na(bwA) || is.na(bwB)) stop("Bandwidth estimation failed.")

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
    densB <- overlap::densityFit(B, xxRad, bwB)/xsc
    densOL <- pmin(densA, densB)

    toPlot <- data.frame(x = xx , yA = densA, yB = densB)
    # Create polygon data for filled area (ensure it closes)
    poly_df <- rbind(
      data.frame(x = xx, y = densOL),
      data.frame(x = rev(xx), y = rep(0, length(densA)))
    )

    # Base ggplot object
    p <- ggplot2::ggplot(data = toPlot, ggplot2::aes(x = xx)) +
      ggplot2::geom_polygon(data = poly_df, mapping = ggplot2::aes(x = x, y = y),
                   color = NA, fill = overlap_color, alpha = overlap_alpha)+
      ggplot2::labs(x = "\nTime", y = "Density\n") +
      ggplot2::theme_minimal()+
      ggplot2::theme(
        axis.line = ggplot2::element_line(linewidth = 0.5, color = "gray10"),
        axis.text = ggplot2::element_text(size = 12),
        axis.title = ggplot2::element_text(size = 14),
        axis.ticks = ggplot2::element_line(linewidth = 0.2, color = "gray10")
      )

    # Add density line
    if (length(linewidth) == 1) {linewidth <- c(linewidth, linewidth)}

    if (!overlap_only) {
      p <- p +
        ggplot2::geom_line(mapping = ggplot2::aes(y = yA), color = linecol[1],
                  linewidth = linewidth[1], linetype = linetype[1])+
        ggplot2::geom_line(mapping = ggplot2::aes(y = yB), color = linecol[2],
                  linewidth = linewidth[2], linetype = linetype[2])
    }

    # Add rug plot if requested
    if (rug) {
      if (isMidnt) {
        A <- ifelse(A < pi, A, A - 2 * pi)
        B <- ifelse(B < pi, B, B - 2 * pi)
      }
      p <- p + ggplot2::geom_rug(data = data.frame(A = A * xsc), ggplot2::aes(x = A),
                        length = ggplot2::unit(rug_lentgh, "npc"),
                        na.rm = TRUE, color = rug_color,
                        sides = "b", inherit.aes = FALSE, ...)+
        ggplot2::geom_rug(data = data.frame(B = B * xsc), ggplot2::aes(x = B),
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
                 fill = extend, alpha = extend_alpha)+
        ggplot2::geom_polygon(data = poly_df, mapping = ggplot2::aes(x = x, y = y),
                              color = NA, fill = overlap_color, alpha = overlap_alpha)


      if (!overlap_only) {
        p <- p +
          ggplot2::geom_line(mapping = ggplot2::aes(y = yA), color = linecol[1],
                    linewidth = linewidth[1], linetype = linetype[1])+
          ggplot2::geom_line(mapping = ggplot2::aes(y = yB), color = linecol[2],
                    linewidth = linewidth[2], linetype = linetype[2])
      }

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

#' @title Plot Overlap Coefficient Matrix
#'
#' @description
#' Visualizes an overlap coefficient matrix.
#'
#' @param data A square matrix (e.g [mm_overlap_matrix()] output) representing overlap coefficients to be visualized.
#' @param side A character string indicating which triangle of the matrix to display.
#' Options are "lower" (default) or "upper".
#' @param show A character string specifying whether to display "shape" (default)
#' or "value" in the plot.
#' @param shape_type Numeric value specifying the type of shape to use in the plot.
#' Defaults to 21 (circle).
#' @param shape_size Numeric value controlling the stroke size of the shapes.
#' Defaults to 0.5.
#' @param text_size Numeric value specifying the size of the text when `show = "value"`.
#' Defaults to 6.
#' @param text_font Character string specifying the font family to use for text labels.
#' Defaults to `NA`.
#' @param excludes A vector of numeric values to exclude from the plot. Defaults to `NULL`.
#' @param color_scale A character string or vector of colors to define the gradient color
#' scale. Defaults to "gray3".
#' @param ... Additional arguments passed to the `guide_colorbar` function.
#'
#' @return A `ggplot` object representing the overlap coefficient matrix visualization.
#'
#' @examples
#'
#' library(ggplot2)
#' # Example overlap coefficient matrix
#' overlap_matrix <- matrix(c(1, 0.8, 0.7, 0.8, 1, 0.9, 0.7, 0.9, 1), ncol = 3)
#' colnames(overlap_matrix) <- rownames(overlap_matrix) <- c("A", "B", "C")
#'
#' # Plot lower triangle with shapes
#' mm_plot_overlap_coef(overlap_matrix, side = "lower", show = "shape")
#'
#' # Plot upper triangle with values
#' mm_plot_overlap_coef(overlap_matrix, side = "upper", show = "value")
#'
#' @import  ggplot2
#'
#' @export

mm_plot_overlap_coef <- function(data,
                           side = c("lower", "upper"),
                           show = c("shape", "value"),
                           shape_type = 21,
                           shape_size = 0.5,
                           text_size = 6,
                           text_font = NA,
                           excludes = NULL,
                           color_scale = "gray3",
                           ...) {

  show <- match.arg(show)
  side <- match.arg(side)

  # Determine whether to keep upper or lower triangle
  if (side == "upper") {
    data[!upper.tri(data, F)] <- NA
  } else {
    data[!lower.tri(data, F)] <- NA
  }

  # Reshape the matrix into a long format
  data_melt <- melt(data) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::mutate(value = round(value, 2))


  if (!is.null(excludes)) {
    data_melt <- data_melt %>% dplyr::filter(!value %in% excludes)
  }
  data_melt$Var2 <- factor(data_melt$Var2, levels = unique(data_melt$Var2))
  data_melt$Var1 <- factor(data_melt$Var1, levels = unique(data_melt$Var1))

  # Base plot
  p <- ggplot2::ggplot(data_melt, aes(Var2, Var1)) +
    ggplot2::theme_minimal() +
    theme(axis.title = ggplot2::element_blank())

  # Add either circles or values
  if (show == "shape") {
    p <- p + geom_point(aes(size = value, color = value, fill = value),
                        shape = shape_type, stroke = shape_size) +
      ggplot2::scale_size_continuous(range = c(1, 6)) +
      ggplot2::scale_color_gradientn(colors = color_scale) +
      ggplot2::scale_fill_gradientn(colors = color_scale) +
      ggplot2::guides(
        size = "none",
        fill = "none",
        color = ggplot2::guide_colorbar(title = "Value")  # Use colorbar to merge
      )

  } else {
    p <- p + ggplot2::geom_text(mapping = ggplot2::aes(label = value, color = value),
                       size = text_size,
                       family = text_font) +
      ggplot2::scale_color_gradientn(colors = color_scale)
  }

  # Add a continuous color bar for visual reference
  p <- p +
    ggplot2::guides(color = ggplot2::guide_colorbar( ... ))

  return(p)
}


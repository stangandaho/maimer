mm_matrix_plot <- function(data,
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
  if (side == "lower") {
    data[!upper.tri(data, F)] <- NA
  } else {
    data[!lower.tri(data, F)] <- NA
  }

  # Melt the matrix into a long format
  data_melt <- reshape2::melt(data) %>%
    dplyr::mutate(value = as.numeric(value)) %>%
    dplyr::filter(!is.na(value))

  if (!is.null(excludes)) {
    data_melt <- data_melt %>%
      dplyr::filter(! value %in% excludes)
  }

  # Base plot
  p <- ggplot(data_melt, aes(Var2, Var1)) +
    theme_minimal() +
    theme(axis.title = element_blank())

  # Add either circles or values
  if (show == "shape") {
    p <- p + geom_point(aes(size = value, color = value),
                        shape = shape_type, stroke = shape_size) +
      scale_size_continuous(range = c(1, 10), guide = "none") +
      scale_color_gradientn(colors = color_scale)
  } else {
    p <- p + geom_text(aes(label = round(value, 2),color = value),
                       size = text_size, family = text_font) +
      scale_color_gradientn(colors = color_scale)
  }

  # Add a continuous color bar for visual reference
  p <- p +
    guides(color = guide_colorbar( ... ))

  return(p)
}


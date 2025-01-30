# Function to calculate the temporal shift of species activity
mm_temporal_shift <- function(first_times,
                              second_times,
                              convert_time = FALSE,
                              xcenter = c("noon", "midnight"),
                              n_grid = 128,
                              kmax = 3,
                              adjust = 1,
                              width_at = 1/2,
                              format = "%H:%M:%S",
                              time_zone,
                              ...) {

  if (convert_time) {
    first_times <- mm_to_radian(times = first_times, format = format, time_zone = time_zone)
    second_times <- mm_to_radian(times = second_times, format = format, time_zone = time_zone)
  }

  if (width_at < 0 | width_at > 1) {
    stop(sprintf("The width %f is out of [0, 1]", width_at))
  }


  overlap:::checkInput(first_times)
  overlap:::checkInput(second_times)
  xcenter <- match.arg(xcenter)
  isMidnt <- xcenter == "midnight"

  # Bandwidth calculation
  bwA <- overlap::getBandWidth(first_times, kmax = kmax) / adjust
  bwB <- overlap::getBandWidth(second_times, kmax = kmax) / adjust
  if (is.na(bwA) || is.na(bwB)) stop("Bandwidth estimation failed.")

  # Create a sequence of values for density estimation
  xxRad  <- seq(0, 2 * pi, length.out = n_grid)
  if (isMidnt) xxRad  <- xxRad - pi

  xx <- xxRad
  print(maimer::mm_to_time(xx))

  # Kernel Density Estimation for period 1
  kde_times1 <- overlap::densityFit(first_times, xxRad, bwA)
  # Kernel Density Estimation for period 2
  kde_times2 <- overlap::densityFit(second_times, xxRad, bwB)
  densOL <- pmin(kde_times1, kde_times2)

  # Calculate the range or spread of the KDE (e.g., width at half maximum)
  fwhm_range1 <- range(xx[kde_times1 > max(kde_times1)*width_at])
  times_min1 <- fwhm_range1[1]
  times_max1 <- fwhm_range1[2]

  fwhm_range2 <- range(xx[kde_times2 > max(kde_times2)*width_at])
  times_min2 <- fwhm_range2[1]
  times_max2 <- fwhm_range2[2]

  #Y
  ytime1 <- min(kde_times1[kde_times1 > max(kde_times1)*width_at])
  ytime2 <- min(kde_times2[kde_times2 > max(kde_times2)*width_at])

  # Compare the results
  temporal_shift <- list(
    times_max1 = times_max1,
    times_min1 = times_min1,
    times_max2 = times_max2,
    times_min2 = times_min2
  )
  points(x = times_max1, y = ytime1)
  points(x = times_max2, y = ytime2)

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

  temporal_shift[["movement"]] <- shift
  data <- dplyr::tibble(xx = xx, kde_times1 = kde_times1, kde_times2 = kde_times2)
  pose_data <- dplyr::tibble(times_min1 = times_min1,
                          times_max1 = times_max1,
                          times_min2 = times_min2,
                          times_max2 = times_max2,
                          ytime1 = ytime1,
                          ytime2 = ytime2
                          )


  # Plot KDEs for visualization
  linetype1 <- 1; linetype2 <- 3
  p <- ggplot(data = data)+
    geom_line(aes(x = xx, y = kde_times1), linetype = linetype1)+
    geom_line(aes(x = xx, y = kde_times2), linetype = linetype2)+
    # Add items for first time
    geom_point(data = pose_data, aes(x = times_min1, y = ytime1))+
    geom_point(data = pose_data, aes(x = times_max1, y = ytime1))+
    geom_segment(data = pose_data, aes(x = times_min1, y = ytime1,
                                       xend = times_max1, yend = ytime1),
                 linetype = linetype1)+
    # Add items for second time
    geom_point(data = pose_data, aes(x = times_min2, y = ytime2))+
    geom_point(data = pose_data, aes(x = times_max2, y = ytime2))+
    geom_segment(data = pose_data, aes(x = times_min2, y = ytime2,
                                       xend = times_max2, yend = ytime2),
                 linetype = linetype2)

  print(p)


  return(temporal_shift)
}


# Example usage
period1_data <- c(1.3, 2.3, 2.5, 5.2, 6.1, 2.3)  # Example timestamps for period 1
period2_data <- c(1.8, 2.2, 3.3, 3.5, 4.3)  # Example timestamps for period 2

mm_temporal_shift(period1_data, period2_data,
                  width_at = 0.5, n_grid = 300,
                  kmax = 3)
maimer::mm_plot_overlap(period1_data, period2_data, kmax = 3, n_grid = 128)

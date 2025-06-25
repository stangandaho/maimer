library(testthat)
library(maimer)

test_that("function returns a ggplot object", {
  test_data <- data.frame(
    camera = rep(c("A", "B"), each = 5),
    datetimes = as.POSIXct(rep(seq(Sys.time(), by = "2 days", length.out = 5), 2))
  )

  p <- mm_plot_camtrap_activity(
    data = test_data,
    deployment = camera,
    datetime = datetimes,
    threshold = 3,
    time_unit = "days"
  )

  expect_s3_class(p, "gg")
})


test_that("returns ggplot with gaps when show_gaps = TRUE", {
  test_data <- data.frame(
    camera = c(rep("A", 5), rep("B", 5)),
    datetimes = as.POSIXct(c(
      seq(Sys.time(), by = "1 day", length.out = 3),
      seq(Sys.time() + 10 * 86400, by = "1 day", length.out = 2),
      seq(Sys.time(), by = "1 day", length.out = 5)
    ))
  )

  p <- mm_plot_camtrap_activity(
    data = test_data,
    deployment = camera,
    datetime = datetimes,
    threshold = 5,
    time_unit = "days",
    show_gaps = TRUE
  )

  expect_s3_class(p, "gg")
})


test_that("works with datetime as character and custom format", {
  test_data <- data.frame(
    camera = c("X", "Y"),
    datetimes = c("2024-01-01 12:00:00", "2024-01-02 14:00:00")
  )

  p <- mm_plot_camtrap_activity(
    data = test_data,
    deployment = camera,
    datetime = datetimes,
    format = "%Y-%m-%d %H:%M:%S"
  )

  expect_s3_class(p, "gg")
})


test_that("handles no gaps gracefully", {
  test_data <- data.frame(
    camera = rep("Z", 5),
    datetimes = as.POSIXct(seq(Sys.time(), by = "1 day", length.out = 5))
  )

  p <- mm_plot_camtrap_activity(
    data = test_data,
    deployment = camera,
    datetime = datetimes,
    threshold = 10,
    time_unit = "days"
  )

  expect_s3_class(p, "gg")
})


test_that("custom styles work without error", {
  test_data <- data.frame(
    camera = rep("Cam1", 6),
    datetimes = as.POSIXct(seq(Sys.time(), by = "1 day", length.out = 6))
  )

  p <- mm_plot_camtrap_activity(
    data = test_data,
    deployment = camera,
    datetime = datetimes,
    activity_style = list(width = 1.2, color = "green", alpha = 0.6),
    break_style = list(width = 0.5, color = "orange", alpha = 0.3)
  )

  expect_s3_class(p, "gg")
})


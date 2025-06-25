library(testthat)
library(maimer)


test_that("returns expected summary tibble", {
  test_data <- data.frame(
    camera = rep("A", 5),
    datetimes = as.POSIXct(seq(
      from = Sys.time(), by = "1 day", length.out = 5
    ))
  )

  result <- mm_summarise_camtrap_activity(
    data = test_data,
    deployment = camera,
    datetime = datetimes,
    threshold = 3,
    time_unit = "days"
  )

  expect_s3_class(result, "tbl_df")
  expect_true(all(c("deployment", "n_records", "activity_rate") %in% names(result)))
  expect_equal(nrow(result), 1)
})


test_that("correctly calculates total and active durations", {
  dt <- as.POSIXct(c(
    "2024-06-01", "2024-06-02", "2024-06-10", "2024-06-11"
  ))

  test_data <- data.frame(
    camera = rep("B", length(dt)),
    datetimes = dt
  )

  result <- mm_summarise_camtrap_activity(
    data = test_data,
    deployment = camera,
    datetime = datetimes,
    threshold = 5,
    time_unit = "days"
  )

  expect_equal(result$n_records, 4)
  expect_true(result$total_duration > result$active_duration)
  expect_true(result$break_duration > 0)
  expect_true(result$activity_rate < 100)
})


test_that("handles multiple deployments correctly", {
  test_data <- data.frame(
    camera = rep(c("X", "Y"), each = 5),
    datetimes = rep(seq(Sys.time(), by = "1 day", length.out = 5), 2)
  )

  result <- mm_summarise_camtrap_activity(
    data = test_data,
    deployment = camera,
    datetime = datetimes
  )

  expect_equal(nrow(result), 2)
})


test_that("works with datetime as character and custom format", {
  test_data <- data.frame(
    camera = c("X", "X", "X"),
    datetimes = c("2024-06-01 12:00:00", "2024-06-02 12:00:00", "2024-06-03 12:00:00")
  )

  result <- mm_summarise_camtrap_activity(
    data = test_data,
    deployment = camera,
    datetime = datetimes,
    format = "%Y-%m-%d %H:%M:%S"
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(result$n_records, 3)
})

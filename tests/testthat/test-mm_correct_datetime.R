library(maimer)
library(testthat)
library(dplyr)

test_that("mm_correct_datetime applies corrections correctly", {
  # Sample data
  data <- data.frame(
    id = 1:3,
    datetime = c("2023-01-01 12:00:00", "2023-01-01 13:00:00", "2023-01-01 14:00:00"),
    deployment = "CAM01",
    stringsAsFactors = FALSE
  )

  corrector <- data.frame(
    deployment = "CAM01",
    sign = "+",
    datetimes = "2023-01-01 12:05:00",
    stringsAsFactors = FALSE
  )

  # Apply function
  corrected <- mm_correct_datetime(data, datetime, deployment, corrector, format = "%Y-%m-%d %H:%M:%S")

  # Check structure
  expect_s3_class(corrected, "data.frame")
  expect_true(all(c("corrected_datetime", "correction_applied", "time_offset_seconds", "corrector_reference") %in% names(corrected)))

  # Check datetime correction logic (+5 minutes)
  expect_equal(as.numeric(difftime(corrected$corrected_datetime[1], corrected$datetime[1], units = "mins")), 5)
  expect_equal(unique(corrected$correction_applied), "+")
  expect_equal(unique(corrected$time_offset_seconds), 300)
})



test_that("mm_correct_datetime handles invalid inputs", {
  corrector <- data.frame(deployment = "CAM01", sign = "+", datetimes = "2023-01-01 12:00:00")

  expect_error(mm_correct_datetime("not_df", datetime, deployment, corrector))

  expect_error(mm_correct_datetime(data.frame(), datetime, deployment, "not_df"))

  # Missing required column
  bad_corrector <- data.frame(deployment = "CAM01", sign = "+")
  expect_error(mm_correct_datetime(data.frame(deployment = "CAM01",
                                              datetime = "2023-01-01"),
                                   datetime, deployment, bad_corrector))
})

test_that("mm_correct_datetime works with auto format detection", {
  data <- data.frame(
    id = 1,
    datetime = "2023/01/01 12:00",
    deployment = "CAM01"
  )
  corrector <- data.frame(
    deployment = "CAM01",
    sign = "-",
    datetimes = "2023/01/01 11:55"
  )

  result <- mm_correct_datetime(data, datetime, deployment, corrector)

  expect_s3_class(result$corrected_datetime, "POSIXct")
  expect_equal(as.numeric(difftime(result$datetime, result$corrected_datetime, units = "mins")), 5)
})

test_that("mm_correct_datetime handles invalid sign", {
  data <- data.frame(
    datetime = "2023-01-01 12:00:00",
    deployment = "CAM01"
  )
  corrector <- data.frame(
    deployment = "CAM01",
    sign = "x",
    datetimes = "2023-01-01 12:05:00"
  )

  expect_error(mm_correct_datetime(data, datetime, deployment, corrector, format = "%Y-%m-%d %H:%M:%S"),
               "Invalid sign")
})

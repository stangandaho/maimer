# tests/testthat/test-mm_to_diel.R

library(testthat)

test_that("mm_to_diel handles typical time formats correctly", {
  times <- c("12:00:00", "00:00:00", "23:59:59")
  sep <- ":"
  expected <- c(0.5, 0, (23 + 59/60 + 59/3600) / 24)
  result <- mm_to_diel(times, sep)

  expect_equal(result, expected)
})

test_that("mm_to_diel handles time with date correctly", {
  times <- c("2024-08-18 06:30:00", "2024-08-18 18:15:30")
  sep <- ":"
  expected <- c((6 + 30/60) / 24, (18 + 15/60 + 30/3600) / 24)
  result <- mm_to_diel(times, sep)

  expect_equal(result, expected)
})

test_that("mm_to_diel handles time with different separators", {
  times <- c("12-30-45", "08-15-00")
  sep <- "-"
  expected <- c((12 + 30/60 + 45/3600) / 24, (8 + 15/60) / 24)
  result <- mm_to_diel(times, sep)

  expect_equal(result, expected)
})

test_that("mm_to_diel handles missing seconds correctly", {
  times <- c("12:30", "08:15")
  sep <- ":"
  expected <- c((12 + 30/60) / 24, (8 + 15/60) / 24)
  result <- mm_to_diel(times, sep)

  expect_equal(result, expected)
})

test_that("mm_to_diel handles missing minutes and seconds correctly", {
  times <- c("12", "08")
  sep <- ":"
  expected <- c(12 / 24, 8 / 24)
  result <- mm_to_diel(times, sep)

  expect_equal(result, expected)
})

test_that("mm_to_diel handles empty or malformed time strings gracefully", {
  times <- c("12:00:00", "", "23:59:59", "invalid_time")
  sep <- ":"
  expected <- c(0.5, NA, (23 + 59/60 + 59/3600) / 24, NA)
  result <- suppressWarnings(mm_to_diel(times, sep))

  expect_equal(result, expected)
})

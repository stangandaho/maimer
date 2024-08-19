library(testthat)
library(dplyr)

# Example function definition here, assuming it's available as mm_is_independent

test_that("mm_is_independent handles data.frame input correctly", {
  # Test data
  df <- data.frame(datetime = as.POSIXct(c("2024-08-01 10:00:00", "2024-08-01 10:15:00",
                                           "2024-08-01 10:45:00", "2024-08-01 11:00:00")))

  # Test with threshold of 15 minutes (900 seconds)
  result <- mm_is_independent(data = df, datetime = "datetime", format = "%Y-%m-%d %H:%M:%S", threshold = 900)

  expect_equal(result, NULL)
})

test_that("mm_is_independent returns only independent events when 'only' is TRUE", {
  # Test data
  df <- data.frame(datetime = as.POSIXct(c("2024-08-01 10:00:00", "2024-08-01 10:15:00",
                                           "2024-08-01 10:45:00", "2024-08-01 11:00:00")))

  result <- mm_is_independent(data = df, datetime = "datetime",
                              format = "%Y-%m-%d %H:%M:%S", threshold = 10, only = TRUE)

  expect_true(all(class(result) %in% c("data.frame", "tbl_df", "tbl")))
})

test_that("mm_is_independent handles deltatime input correctly", {
  # Test deltatime vector
  deltatime <- c(900, 900, 1800, 600)

  result <- mm_is_independent(deltatime = deltatime, threshold = 900)
  expected <- data.frame(deltatime = deltatime, independent = c(TRUE, TRUE, TRUE, FALSE))

  expect_equal(result, as_tibble(expected))
})

test_that("mm_is_independent handles missing datetime and format with data", {
  expect_error(mm_is_independent(data = df), "`datetime`, cannot be missed")
  expect_error(mm_is_independent(data = df, datetime = "datetime"), "`format` cannot be missed")
})

test_that("mm_is_independent handles ambiguous datetime formats", {
  df <- data.frame(datetime = c("2024-08-01 10:00:00", "2024-08-01 10:15:00", "unknown datetime"),
                   value = c(1, 2, 3))

  expect_warning(mm_is_independent(data = df, datetime = "datetime", format = "%Y-%m-%d %H:%M:%S"),
                 "The following datetime are ambiguous: unknown datetime")
})

test_that("mm_is_independent returns all rows when 'only' is FALSE", {
  # Test data
  df <- data.frame(datetime = as.POSIXct(c("2024-08-01 10:00:00", "2024-08-01 10:15:00",
                                           "2024-08-01 10:45:00", "2024-08-01 11:00:00")),
                   value = c(1, 2, 3, 4))

  result <- mm_is_independent(data = df, datetime = "datetime",
                              format = "%Y-%m-%d %H:%M:%S", threshold = 20, only = FALSE)
  expected <- df %>%
    mutate(independent = c(FALSE, TRUE, TRUE, FALSE),
           deltatime = c(0, 15, 30, 15))

  expect_equal(result, as_tibble(expected))
})


test_that("mm_is_independent returns correct message when no independent events found", {
  df <- data.frame(datetime = as.POSIXct(c("2024-08-01 10:00:00", "2024-08-01 10:15:00",
                                           "2024-08-01 10:30:00")),
                   value = c(1, 2, 3))

  expect_message(mm_is_independent(data = df, datetime = "datetime",
                                   format = "%Y-%m-%d %H:%M:%S", threshold = 3600),
                 "Any independent event found with threshold of 3600")
})

test_that("mm_is_independent handles empty data.frame correctly", {
  df <- data.frame(datetime = as.POSIXct(character(0)), value = numeric(0))

  expect_error(mm_is_independent(data = df, datetime = "datetime",
                                 format = "%Y-%m-%d %H:%M:%S"))
})

test_that("mm_is_independent ambiguous date", {
  df <- data.frame(datetime = c("202408-01 10:00:00", "2024-08-01 10:15:00",
                                           "2024-08-01 10:30:00"),
                   value = c(1, 2, 3))

  expect_warning(mm_is_independent(data = df, datetime = "datetime",
                                   format = "%Y-%m-%d %H:%M:%S", threshold = 10))

})

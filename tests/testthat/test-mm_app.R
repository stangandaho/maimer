library(testthat)
library(shiny)

# Test suite for mm_app function
test_that("mm_app function exists and is exported", {
  expect_true(exists("mm_app"))
  expect_true(is.function(mm_app))
})

test_that("mm_app has correct function signature", {
  # Check that function takes no required parameters
  args <- formals(mm_app)
  expect_equal(length(args), 0)
})

test_that("system.file returns valid app directory path", {
  app_dir <- system.file("app", package = "maimer")

  # Skip test if package is not installed or app directory doesn't exist
  skip_if(app_dir == "", "maimer package app directory not found")

  expect_true(nzchar(app_dir))
  expect_true(dir.exists(app_dir))
})

test_that("required app files exist in app directory", {
  app_dir <- system.file("app", package = "maimer")
  skip_if(app_dir == "", "maimer package app directory not found")

  # Check for packages.R file
  packages_file <- file.path(app_dir, "packages.R")
  expect_true(file.exists(packages_file))

  # Check for typical Shiny app files (at least one should exist)
  ui_file <- file.path(app_dir, "ui.R")
  server_file <- file.path(app_dir, "server.R")
  app_file <- file.path(app_dir, "app.R")

  # At least one of these should exist for a valid Shiny app
  has_shiny_files <- file.exists(ui_file) || file.exists(server_file) || file.exists(app_file)
  expect_true(has_shiny_files,
              info = "At least one of ui.R, server.R, or app.R should exist")
})

test_that("packages.R file can be sourced without errors", {
  app_dir <- system.file("app", package = "maimer")
  skip_if(app_dir == "", "maimer package app directory not found")

  packages_file <- file.path(app_dir, "packages.R")
  skip_if(!file.exists(packages_file), "packages.R file not found")

  # Test that packages.R can be sourced without throwing errors
  expect_no_error({
    # Create isolated environment to avoid polluting global environment
    test_env <- new.env()
    source(packages_file, local = test_env)
  })
})

test_that("packages.R contains valid R code", {
  app_dir <- system.file("app", package = "maimer")
  skip_if(app_dir == "", "maimer package app directory not found")

  packages_file <- file.path(app_dir, "packages.R")
  skip_if(!file.exists(packages_file), "packages.R file not found")

  # Read and parse the file to check for syntax errors
  expect_no_error({
    code <- readLines(packages_file)
    # Check that file is not empty
    expect_true(length(code) > 0)

    # Try to parse the code
    parsed_code <- parse(text = paste(code, collapse = "\n"))
    expect_true(length(parsed_code) > 0)
  })
})

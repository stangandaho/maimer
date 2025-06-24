library(testthat)
library(dplyr)

# Test data setup
test_data <- data.frame(
    site = rep(c("Site1", "Site2", "Site3"), each = 10),
    species = c(
      # Site1: High diversity, even distribution
      rep(c("SpeciesA", "SpeciesB", "SpeciesC", "SpeciesD"), c(3, 3, 2, 2)),
      # Site2: Low diversity, uneven distribution
      rep(c("SpeciesA", "SpeciesB"), c(8, 2)),
      # Site3: Medium diversity
      rep(c("SpeciesA", "SpeciesB", "SpeciesC"), c(4, 3, 3))
    ),
    abundance = c(
      rep(c(1, 1, 1, 1), c(3, 3, 2, 2)),  # Site1
      rep(c(2, 1), c(8, 2)),               # Site2
      rep(c(1, 1, 1), c(4, 3, 3))          # Site3
    ),
    stringsAsFactors = FALSE
  )

community_matrix <- data.frame(
    site = c("Site1", "Site2", "Site3"),
    SpeciesA = c(3, 16, 4),
    SpeciesB = c(3, 2, 3),
    SpeciesC = c(2, 0, 3),
    SpeciesD = c(2, 0, 0),
    stringsAsFactors = FALSE
  )


# Tests for mm_alpha_diversity function
test_that("mm_alpha_diversity works with basic shannon index", {

  result <- mm_alpha_diversity(
    data = test_data,
    site_column = site,
    species_column = species,
    index = "shannon",
    to_community = TRUE
  )

  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 2)
  expect_equal(nrow(result), 3)
  expect_true("site" %in% names(result))
  expect_true("shannon" %in% names(result))
  expect_true(all(result$shannon >= 0))
  expect_true(all(is.finite(result$shannon)))
})

test_that("mm_alpha_diversity works with multiple indices", {

  result <- mm_alpha_diversity(
    data = test_data,
    site_column = site,
    species_column = species,
    index = c("shannon", "simpson", "invsimpson", "evenness"),
    to_community = TRUE
  )

  expect_equal(ncol(result), 5)  # site + 4 indices
  expect_true(all(c("site", "shannon", "simpson", "invsimpson", "evenness") %in% names(result)))

  # Check value ranges
  expect_true(all(result$shannon >= 0))
  expect_true(all(result$simpson >= 0 & result$simpson <= 1))
  expect_true(all(result$invsimpson >= 1))
  expect_true(all(result$evenness >= 0 & result$evenness <= 1))
})

test_that("mm_alpha_diversity works with size_column", {

  result <- mm_alpha_diversity(
    data = test_data,
    site_column = site,
    species_column = species,
    size_column = abundance,
    index = "shannon",
    to_community = TRUE
  )

  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 2)
  expect_true(all(result$shannon >= 0))
})

test_that("mm_alpha_diversity works with pre-computed community matrix", {

  result <- mm_alpha_diversity(
    data = community_matrix,
    site_column = site,
    species_column = 2:5,  # species columns
    index = "shannon",
    to_community = FALSE
  )

  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 2)
  expect_equal(nrow(result), 3)
  expect_true(all(result$shannon >= 0))
})

test_that("mm_alpha_diversity calculates simpson index correctly", {
  # Test with known values
  community_data <- data.frame(
    site = c("Site1", "Site2"),
    sp1 = c(10, 5),
    sp2 = c(10, 5),
    sp3 = c(0, 0)
  )

  result <- mm_alpha_diversity(
    data = community_data,
    site_column = site,
    species_column = 2:4,
    index = "simpson",
    to_community = FALSE
  )

  # Site1: equal abundance (10, 10, 0) -> Simpson = 1 - ((10/20)^2 + (10/20)^2) = 1 - 0.5 = 0.5
  expect_equal(result$simpson[1], 0.5)
  # Site2: equal abundance (5, 5, 0) -> Simpson = 1 - ((5/10)^2 + (5/10)^2) = 1 - 0.5 = 0.5
  expect_equal(result$simpson[2], 0.5)
})

test_that("mm_alpha_diversity calculates inverse simpson correctly", {
  community_data <- data.frame(
    site = c("Site1"),
    sp1 = c(5),
    sp2 = c(5)
  )

  result <- mm_alpha_diversity(
    data = community_data,
    site_column = site,
    species_column = 2:3,
    index = "invsimpson",
    to_community = FALSE
  )

  # Equal abundance -> Simpson D = 0.5, Inverse = 1/0.5 = 2
  expect_equal(result$invsimpson[1], 2.0)
})

test_that("mm_alpha_diversity calculates evenness correctly", {
  community_data <- data.frame(
    site = c("Site1"),
    sp1 = c(10),
    sp2 = c(10)
  )

  result <- mm_alpha_diversity(
    data = community_data,
    site_column = site,
    species_column = 2:3,
    index = "evenness",
    to_community = FALSE
  )

  # Perfect evenness with 2 species -> J = H/ln(S) = ln(2)/ln(2) = 1
  expect_equal(result$evenness[1], 1.0)
})

test_that("mm_alpha_diversity handles pielou index", {
  result <- mm_alpha_diversity(
    data = test_data,
    site_column = site,
    species_column = species,
    index = "pielou",
    to_community = TRUE
  )

  expect_true("pielou" %in% names(result))
  expect_true(all(result$pielou >= 0 & result$pielou <= 1))
})

test_that("mm_alpha_diversity handles single species correctly", {
  single_species_data <- data.frame(
    site = c("Site1", "Site1", "Site1"),
    species = c("SpeciesA", "SpeciesA", "SpeciesA")
  )

  result <- mm_alpha_diversity(
    data = single_species_data,
    site_column = site,
    species_column = species,
    index = c("shannon", "simpson", "evenness"),
    to_community = TRUE
  )

  # Single species should have zero diversity
  expect_equal(result$shannon[1], 0)
  expect_equal(result$simpson[1], 0)
  expect_true(is.nan(result$evenness[1]) || result$evenness[1] == 0)  # log(1) = 0, so 0/0 = NaN
})

test_that("mm_alpha_diversity handles empty sites", {
  empty_site_data <- data.frame(
    site = c("Site1", "Site2"),
    sp1 = c(0, 10),
    sp2 = c(0, 5)
  )

  result <- mm_alpha_diversity(
    data = empty_site_data,
    site_column = site,
    species_column = 2:3,
    index = "shannon",
    to_community = FALSE
  )

  # Empty site should have NaN or 0 diversity
  expect_true(is.nan(result$shannon[1]) || result$shannon[1] == 0)
  expect_true(result$shannon[2] > 0)
})

test_that("mm_alpha_diversity validates index parameter", {
  expect_error(
    mm_alpha_diversity(
      data = test_data,
      site_column = site,
      species_column = species,
      index = "invalid_index"
    ),
    "Accepted index are:"
  )

  expect_error(
    mm_alpha_diversity(
      data = test_data,
      site_column = site,
      species_column = species,
      index = c("shannon", "invalid_index")
    ),
    "Accepted index are:"
  )
})

test_that("mm_alpha_diversity works with different margin values", {
  # Test margin = 1 (default, row-wise)
  result <- mm_alpha_diversity(
    data = community_matrix,
    site_column = site,
    species_column = 2:5,
    index = "shannon",
    to_community = FALSE,
    margin = 1
  )

  expect_s3_class(result, "data.frame")
})

test_that("mm_alpha_diversity returns correct column names", {

  result <- mm_alpha_diversity(
    data = test_data,
    site_column = site,
    species_column = species,
    index = c("shannon", "simpson"),
    to_community = TRUE
  )

  expect_equal(names(result), c("site", "shannon", "simpson"))
})

test_that("mm_alpha_diversity preserves site names", {
  original_sites <- unique(test_data$site)

  result <- mm_alpha_diversity(
    data = test_data,
    site_column = site,
    species_column = species,
    index = "shannon",
    to_community = TRUE
  )

  expect_equal(sort(result$site), sort(original_sites))
})


test_that("mm_alpha_diversity handles NA values appropriately", {
  na_data <- data.frame(
    site = c("Site1", "Site1", "Site2"),
    sp1 = c(10, NA, 5),
    sp2 = c(5, 5, NA)
  )

  expect_no_error({
    result <- mm_alpha_diversity(
      data = na_data,
      site_column = site,
      species_column = 2:3,
      index = "shannon",
      to_community = FALSE
    )
  })

  expect_s3_class(result, "data.frame")
})


test_that("mm_alpha_diversity works with different column selection methods", {
  # Test with numeric range
  result1 <- mm_alpha_diversity(
    data = community_matrix,
    site_column = site,
    species_column = 2:5,
    index = "shannon",
    to_community = FALSE
  )

  # Test with explicit column names (if supported)
  expect_no_error({
    result2 <- mm_alpha_diversity(
      data = community_matrix,
      site_column = site,
      species_column = c(SpeciesA, SpeciesB, SpeciesC, SpeciesD),
      index = "shannon",
      to_community = FALSE
    )
  })

  expect_s3_class(result1, "data.frame")
})

rm(community_matrix, test_data)

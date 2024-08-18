
test_that("Create hierarchical subject in metadata", {
  # Define the URL of the image to be downloaded

  image_path <- file.path(system.file("img", package = "maimer"), "large.jpeg")

  null_output <- mm_create_hs(path = image_path, value = c("Species" = "Vulture"))
  testthat::expect_equal(null_output, "1 image files updated")

  # Complet HS to existing
  null_output2 <- mm_create_hs(path = image_path, value = c("Species" = "Vulture", "Sex" = "Female"))
  testthat::expect_equal(null_output2, "1 image files updated")

  # Wrong hirarchical subject format
  testthat::expect_error(mm_create_hs(path = image_path))
  testthat::expect_error(mm_create_hs(path = image_path, value = c("Species")))
  testthat::expect_error(mm_create_hs(path = image_path, value = c("Species" = "BBB", "Sex")))

  #unlink(image_path)
  unlink(paste0(image_path, "_original"))

})



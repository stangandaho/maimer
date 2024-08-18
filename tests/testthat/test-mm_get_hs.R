
test_that("Get hierarchical subject in metadata", {

  image_path <- file.path(system.file("img", package = "maimer"), "large.jpeg")
  mm_remove_hs(image_path)

  null_output <- mm_get_hs(path = image_path)
  testthat::expect_equal(null_output, NULL)

  mm_create_hs(path = image_path, value = c("Species" = "Vulture"))
  sh_out <- mm_get_hs(path = image_path)
  testthat::expect_equal(sh_out, "Species|Vulture")

  testthat::expect_error(mm_get_hs())
  #unlink(image_path)
  unlink(paste0(image_path, "_original"))
})

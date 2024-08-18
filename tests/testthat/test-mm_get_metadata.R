test_that("Get image metadata", {
  image_path <- file.path(system.file("img", package = "maimer"), "large.jpeg")
  image_dir <- dirname(image_path)

  metadata_df <- mm_get_metadata(path = image_path)
  testthat::expect_true(is.data.frame(metadata_df))

  # Test for folder path
  ## Creat folder to copy image into.
  if (!dir.exists(file.path(image_dir, "unitest/justme"))) {
    dir.create(path = file.path(image_dir, "unitest/justme"), recursive = T)
  }
  ## Repeat image copy
  for (i in 1:3) {
    file.copy(from = image_path, to = paste0(image_dir, "/unitest/justme/", i, basename(image_path)))
  }

  metadata_df2 <- mm_get_metadata(path = paste0(image_dir, "/unitest"), recursive = T)
  testthat::expect_true(nrow(metadata_df2) == 3)

  # no recursive, default
  testthat::expect_error(mm_get_metadata(path = paste0(image_dir, "/unitest"), recursive = F))

  # wrong file name
  testthat::expect_error(mm_get_metadata(path = image_path, save_file = T,
                                         file_name = "filename"))

  # wrong file name
  testthat::expect_error(maimer::mm_get_metadata(path = "no/file/path/image.jpeg"))
  testthat::expect_error(maimer::mm_get_metadata(path = image_url))

  mm_get_metadata(path = image_path, save_file = T)
  testthat::expect_true(file.exists(paste0(image_dir, "/metadata.csv")))
  unlink(file.path(image_dir, "metadata.csv"))

  mm_get_metadata(path = image_path, save_file = T, file_name = "mymetadata.csv")
  testthat::expect_true(file.exists(paste0(image_dir, "/mymetadata.csv")))

  unlink(file.path(image_dir, "mymetadata.csv"))
  unlink(paste0(image_path, "/unitest"), recursive = T)
  #unlink(image_path)

})

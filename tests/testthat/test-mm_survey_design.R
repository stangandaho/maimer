test_that("test survey design", {

  data("pendjari")
  ## transform
  pendjari_trans <- pendjari %>%
    sf::st_transform(crs = "EPSG:32631")

  random_sdes <- mm_survey_design(study_area = pendjari_trans, method = "random", verbose = T,
                                  total_site = 15, min_distance = 5000, padding = 2000,
                                  set_seed = 123)
  testthat::expect_true(inherits(random_sdes, "sf"))

  ##
  regular_sdes <- mm_survey_design(study_area = pendjari_trans, method = "regular", verbose = T,
                                   distance = c(6000, 10000), padding = 2500, set_seed = 123)
  testthat::expect_true(inherits(regular_sdes, "sf"))

  ##
  rand_c_sdes <- mm_survey_design(study_area = pendjari_trans,
                                  method = "random_cluster", verbose = T,
                                  total_cluster = 8, total_site = 3,
                                  distance = c(8000, 4000), min_distance = 1000,
                                  padding = 1000, nest_padding = 500, set_seed = 123)
  testthat::expect_true(inherits(rand_c_sdes, "sf"))

  ##
  rand_c_reg_sdes <- mm_survey_design(study_area = pendjari_trans,
                                      method = "random_cluster", verbose = T,
                                      total_cluster = 6, total_site = 4, type_in = "regular",
                                      distance = c(6000, 3000),
                                      padding = 1000, nest_padding = 0, set_seed = 123)
  testthat::expect_true(inherits(rand_c_reg_sdes, "sf"))

  ##
  reg_c_sdes <- mm_survey_design(study_area = pendjari_trans,
                                 method = "regular_cluster", verbose = T,
                                 total_site = 4, distance = c(10000, 6000),
                                 min_distance = 1500, padding = 2000, set_seed = 123)

  testthat::expect_true(inherits(reg_c_sdes, "sf"))

  ##
  reg_c_reg_sdes <- mm_survey_design(study_area = pendjari_trans,
                                     method = "regular_cluster", verbose = T,
                                     total_site = 2, distance = c(9000, 6000), type_in = "regular",
                                     padding = 1000, set_seed = 123)
  testthat::expect_true(inherits(reg_c_reg_sdes, "sf"))

  ##
  mask_sdes <- mm_survey_design(study_area = pendjari_trans,
                                method = "mask", verbose = T,
                                total_site = 5, distance = c(7000, 6000),
                                min_distance = 2000, nest_padding = 2000, set_seed = 123)

  testthat::expect_true(inherits(mask_sdes, "sf"))

  ##
  mask_regular_sdes <- mm_survey_design(study_area = pendjari_trans,
                                        method = "mask", verbose = T, type_in = "regular",
                                        total_site = c(8, 2, 13), distance = c(7000, 6000),
                                        min_distance = 2000, nest_padding = 2000, set_seed = 123)


  testthat::expect_true(inherits(mask_regular_sdes, "sf"))



})


context("test-save_map.R")

test_that("check save_map", {

  data_dir <- system.file("extdata", package = "collar")

  tmp <- collar::fetch_csv(paste0(data_dir, "/vectronics_2.csv"))

  dat <-
    tmp %>%
    dplyr::filter(!is.na(longitude)) %>%
    morph_gps(
      x = .,
      id_col = idcollar,
      dt_col = acquisitiontime,
      dt_format = "%Y-%m-%dT%H:%M:%S",
      lon_col =  longitude,
      lat_col = latitude
    )


  expect_error(
    save_map("a")
  )

  # test file produced
  tmp_dir <- tempdir()
  tmp_file <- "tmp.map"
  save_map(make_map(dat), file = paste0(tmp_dir, tmp_file))

  testthat::expect_true(
    file.exists(paste0(tmp_dir, tmp_file))
  )

})


test_that("Check save_map assertions", {

  expect_condition(
    save_map("a"),
    "x does not inherit from class leaflet",
    fixed = T
  )


})

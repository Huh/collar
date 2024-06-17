test_that("check save_map", {

  data_dir <- system.file("extdata", package = "collar")

  tmp <- collar::fetch_csv(paste0(data_dir, "/vectronics_2.csv"))

  tmp_file <- normalizePath(
    file.path(tempdir(), "tmp_map.html"),
    mustWork = FALSE
  )

  on.exit(unlink(tmp_file))

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
  save_map(make_map(dat), file = tmp_file)

  testthat::expect_true(
    file.exists(tmp_file)
  )

})


test_that("Check save_map assertions", {

  expect_condition(
    save_map("a"),
    "x does not inherit from class leaflet",
    fixed = T
  )


})

test_that("Check make_gpx", {

  data_dir <- system.file("extdata", package = "collar")

  tmp <- collar::fetch_csv(paste0(data_dir, "/vectronics_2.csv"))

  tmp_file <- normalizePath(file.path(tempdir(), "tmp.gpx"), mustWork = FALSE)
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
    make_gpx()
  )

  # test file produced
  make_gpx(dat, file = tmp_file)

  testthat::expect_true(
    file.exists(tmp_file)
  )

})

test_that("Check make_gpx assertions", {

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

  # no data
  expect_condition(
    make_gpx(),
    "argument \"x\" is missing, with no default"
  )

  # no column with name
  expect_error(
    make_gpx(dat, id_col = "A")
  )

  dat_na <-
    dplyr::mutate(dat, lon = NA) %>%
    dplyr::slice(1)

  expect_error(
    make_gpx(file = "./points.gpx", dat_na)
  )

})

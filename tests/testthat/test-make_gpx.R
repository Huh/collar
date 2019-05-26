context("test-make_gpx.R")

 test_that("Check make_gpx", {

  data_dir <- system.file("extdata", package = "collar")

  dat <- collar::fetch_csv(paste0(data_dir, "/vectronics_2.csv"))

  df <-
    dat %>%
    tidyr::drop_na(longitude) %>%
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

  expect_s3_class(
    make_gpx(df),
    "data.frame"
  )

  expect_that(
    nrow(make_gpx(df) == nrow(df))
  )

  # test file produced
  tmp_dir <- tempdir()
  tmp_file <- paste0(tmp_dir, "/map.html")
  make_gpx(df, file = tmp_file)

  testthat::test_that(
    file.exists(tmp_file)
  )

})

test_that("Check make_gpx assertions", {

  data_dir <- system.file("extdata", package = "collar")

  dat <- collar::fetch_csv(paste0(data_dir, "/vectronics_2.csv"))

  df <-
    dat %>%
    tidyr::drop_na(longitude) %>%
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
  expect_condition(
    make_gpx(df, id_col = "A"),
    "x does not have name A"
  )

  df_na <-
    dplyr::mutate(df, lon = NA) %>%
    dplyr::slice(1)

  expect_condition(
    make_gpx(df_na),
    "dplyr::pull(x, lon_col) contains 1 missing values"
  )

  df_na <-
    dplyr::mutate(df, lat = NA) %>%
    dplyr::slice(1)

  expect_condition(
    make_gpx(df_na),
    "dplyr::pull(x, lat_col) contains 1 missing values"
  )

})

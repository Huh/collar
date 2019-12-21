context("test-filter_date_range.R")


test_that("Check filter_date_range", {

  data_dir <- system.file("extdata", package = "collar")
  tmp <- collar::fetch_csv(paste0(data_dir, "/vectronics_2.csv"))

  dat <- tmp %>%
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
    filter_date_range()
  )

  expect_equal(
    nrow(filter_date_range(dat)),
    nrow(dat)
  )

  expect_lt(
    nrow(filter_date_range(dat, start = "2018-01-01")),
    nrow(dat)
  )

  expect_equal(
    max(filter_date_range(dat)$dt),
    max(dat$dt)
  )

  expect_s3_class(
    filter_date_range(dat),
    "data.frame"
  )

})


test_that("Check make_gpx assertions", {

  data_dir <- system.file("extdata", package = "collar")

  tmp <- collar::fetch_csv(paste0(data_dir, "/vectronics_2.csv"))

  dat <-
    tmp %>%
    tidyr::drop_na(longitude) %>%
    morph_gps(
      x = .,
      id_col = idcollar,
      dt_col = acquisitiontime,
      dt_format = "%Y-%m-%dT%H:%M:%S",
      lon_col =  longitude,
      lat_col = latitude
    )


  expect_condition(
    filter_date_range(dat, dt_col = "a"),
    "x does not have all of these name(s): 'a'",
    fixed = T
  )

  expect_error(
    filter_date_range(dat, dt_col = "lon")
  )

  expect_error(
    filter_date_range(dat, start = "a")
  )

  expect_error(
    filter_date_range(dat, end = "a")
  )

})

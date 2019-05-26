context("test-morph_gps.R")

test_that("Check morph_gps", {

  dat <-
    system.file("extdata/vectronics.csv", package = "collar") %>%
    collar::fetch_csv(.) %>%
    dplyr::mutate(dt_col = paste(utc_date, utc_time))

  dat_morph <-
    morph_gps(
      x = dat,
      id_col = collarid,
      dt_col = dt_col,
      dt_format = "%m/%d%Y %H:%M:%S",
      lon_col = `longitude[°]`,
      lat_col =`latitude[°]`
    )

  expect_error(
    morph_gps()
  )

  expect_error(
    morph_gps(x = "A")
  )

  expect_error(
    morph_gps(x = dat, id_col = "id")
  )

  expect_s3_class(
    dat_morph,
    "data.frame"
  )

  expect_true(ncol(dat_morph) == 4)

  expect_true(nrow(dat) == nrow(dat_morph))

})

test_that("Check morph_gps assertions", {

  dat <-
    system.file("extdata/vectronics.csv", package = "collar") %>%
    collar::fetch_csv(.) %>%
    dplyr::mutate(dt = paste(utc_date, utc_time))


  # no data
  expect_condition(
    morph_gps(),
    "x has an empty dimension"
  )

  # not a data frame
    expect_condition(
    morph_gps("a"),
    "x is not a data frame"
  )

  # meta not a list
  expect_condition(
    morph_gps(
      x = dat,
      id_col = collarid,
      dt_col = utc_date,
      dt_format = "%m/%d%Y %H:%M:%S",
      lon_col = `longitude[°]`,
      lat_col =`latitude[°]`,
      meta = "a"
    ),    "is.list(meta) | is.null(meta) is not TRUE"
  )

  # column names are qouted
  expect_condition(
    morph_gps(
      x = dat,
      id_col = NULL,
      dt_col = utc_date,
      dt_format = "%m/%d%Y %H:%M:%S",
      lon_col = `longitude[°]`,
      lat_col =`latitude[°]`
    ),
    "Column id_col must be unquoted. If special characters or spaces exist use back ticks (`A B`)."
  )

  expect_condition(
    morph_gps(
      x = dat,
      id_col = collarid,
      dt_col = NULL,
      dt_format = "%m/%d%Y %H:%M:%S",
      lon_col = `longitude[°]`,
      lat_col =`latitude[°]`
    ),
    "Column dt_col must be unquoted. If special characters or spaces exist use back ticks (`A B`)."
  )


  expect_condition(
    morph_gps(
      x = dat,
      id_col = collarid,
      dt_col = utc_date,
      dt_format = "%m/%d%Y %H:%M:%S",
      lon_col = NULL,
      lat_col =`latitude[°]`
    ),
    "Column lon_col must be unquoted. If special characters or spaces exist use back ticks (`A B`)."
  )

  expect_condition(
    morph_gps(
      x = dat,
      id_col = collarid,
      dt_col = utc_date,
      dt_format = "%m/%d%Y %H:%M:%S",
      lon_col = `longitude[°]`,
      lat_col = NULL
    ),
    "Column lat_col must be unquoted. If special characters or spaces exist use back ticks (`A B`)."
  )

})

context("test-make_map.R")

test_that("check make_map", {

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

  expect_error(
    make_map()
  )

})


test_that("check make_map assertions", {

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
    make_map("a"),
    "x does not inherit from class data.frame",
    fixed = T
  )

  expect_error(
    make_map(dat, id_col = "a")
  )

  expect_condition(
    dplyr::mutate(dat, lon = NA) %>%
    dplyr::slice(1) %>%
    make_map(),
    "x[, lon_col] contains 1 missing values",
    fixed = T
  )

  expect_condition(
    dplyr::mutate(dat, lat = NA) %>%
    dplyr::slice(1) %>%
    make_map(),
    "x[, lat_col] contains 1 missing values",
    fixed = T
  )

  dat_large <-
    dat %>%
    dplyr::slice(rep(1:dplyr::n(), each = 30))



})

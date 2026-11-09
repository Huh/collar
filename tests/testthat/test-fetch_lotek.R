offline <- function() {
  !RCurl::url.exists("www.google.com")
}

check_api <- function() {
  if (offline()) {
    testthat::skip("API not available - no internet")
  }
}

dt_format <- function(dt_in) {
  paste0(
    gsub(
      " ",
      "T",
      as.character(
        dt_in
      )
    ),
    "Z"
  )
}

check_data <- function(x, data_type) {
  testthat::expect_s3_class(x, "tbl_df")
  check_names <- switch(
    data_type,
    "alerts" = c(
      "DeviceID",
      "AlertType",
      "AlertDate",
      "CancelDate",
      "latitude",
      "longitude"
    ),
    "devices" = c("DeviceID"),
    "postions" = c(
      "UploadTimeStamp",
      "Latitude",
      "Longitude",
      "Altitude",
      "PDOP",
      "Temperature",
      "DeviceID",
      "RecDateTime"
    )
  )
  testthat::expect_true(
    all(
      rlang::has_name(x, check_names)
    )
  )
}

test_that("Check lotek login function", {

  check_api()

  # check that bad inputs return errors
  expect_error(lotek_login("baduser", "badpw"))
  expect_error(lotek_login(NA, "badpw"))
  expect_error(lotek_login("baduser", NA))
  expect_error(lotek_login(NA, NA))
  expect_error(lotek_login(NULL, "badpw"))
  expect_error(lotek_login("baduser", NULL))

  # check that GET API calls fail without login
  expect_error(fetch_lotek_alerts())
  expect_error(fetch_lotek_devices())
  expect_error(fetch_lotek_positions())

})

test_that("Check lotek authenticated functions (live, own credentials)", {
  # Lotek's public demo account no longer accepts logins, so this runs only
  # when credentials for a real account are set in the environment.
  usr <- Sys.getenv("COLLAR_LOTEK_USR")
  pwd <- Sys.getenv("COLLAR_LOTEK_PWD")
  skip_if(
    usr == "" || pwd == "",
    "Set COLLAR_LOTEK_USR and COLLAR_LOTEK_PWD to run live Lotek tests"
  )
  check_api()

  expect_true(lotek_login(usr, pwd))
  expect_type(lotek_token(), "character")

  tkn <- ltk_env$ltk$access_token
  lotek_refresh_token(force_refresh = TRUE)
  expect_false(tkn == ltk_env$ltk$access_token)

  check_data(fetch_lotek_alerts(), "alerts")

  dl <- fetch_lotek_devices()
  check_data(dl, "devices")

  dev <- as.character(dl[[1, "DeviceID"]])
  pos <- fetch_lotek_positions(device_id = dev)
  check_data(pos, "positions")
  expect_true(all(as.integer(pos$DeviceID) == as.integer(dev)))
  expect_identical(nrow(add_lotek_status(pos)), nrow(pos))

  lotek_logout()
  expect_error(fetch_lotek_alerts())
})

test_that("RxStatus decodes status code and satellite count (#87)", {
  # 0b1010110 = 86: 10 satellites (bits 3-6), status 6 (bits 0-2)
  rx <- c(86L, 0L, 7L, 127L, NA)
  expect_identical(lotek_status_code(rx), c(6L, 0L, 7L, 7L, NA))
  expect_identical(lotek_num_sats(rx), c(10L, 0L, 0L, 15L, NA))
  expect_identical(lotek_status_code(integer(0)), integer(0))
})

test_that("add_lotek_status labels each position type (#87)", {
  pos <- tibble::tibble(
    DeviceID = 1L,
    RxStatus = c(86L, 37L, NA),
    FixType = c(1L, 2L, 0L)
  )

  std <- add_lotek_status(pos)
  expect_identical(
    std$FixStatus,
    c("3-D least-squares", "2-D least-squares", NA)
  )
  expect_identical(std$NumSats, c(10L, 4L, NA))
  expect_identical(nrow(std), nrow(pos))

  swift <- add_lotek_status(pos, "Litetrack (Swift fix)")
  expect_identical(
    swift$FixTypeText,
    c("Standard fix", "Swift fix", "Time out")
  )
  expect_identical(swift$FixStatus, c("3-D least-squares", NA, NA))

  ird <- add_lotek_status(
    tibble::tibble(RxStatus = c(3L, 20L, 99L)),
    "Iridium Track"
  )
  expect_identical(ird$FixStatus, c("2D fix", "3D validated fix", NA))
})

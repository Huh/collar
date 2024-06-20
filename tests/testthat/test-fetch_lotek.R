offline <- function() {
  !RCurl::url.exists("www.google.com")
}

check_api <- function() {
  if (offline()) {
    skip("API not available - no internet")
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
  expect_s3_class(x, "tbl_df")
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
  expect_true(
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

  # # test valid login
  # lotek_login("demo", "PASSWORD09")
  #
  # # check object returned with valid inputs
  # expect_is(lotek_token(), "character")
  #
  # # check that refresh function works as expected
  # t <- ltk.env$ltk$access_token
  # lotek_refresh_token(force_refresh = TRUE)
  # expect_false(t == ltk.env$ltk$access_token)
  #
  # # check that token works for retrieving data
  # dl <- fetch_lotek_devices()
  #
  # # check that logging out revokes access
  # lotek_logout()
  # expect_error(fetch_lotek_alerts())

})

# test_that("Check lotek data functions", {

  # check_api()

  # lotek_login("demo", "PASSWORD09")
  #
  # # check data retrieval functions
  # alerts <- fetch_lotek_alerts()
  # check_data(alerts, "alerts")
  #
  # dl <- fetch_lotek_devices()
  # check_data(dl, "devices")
  #
  # # check position function
  # dev <- as.character(dl[1,"DeviceID"])
  # st <- lubridate::mdy("1/1/20")
  # end <- lubridate::mdy("2/1/20")
  #
  # pos <- fetch_lotek_positions(
  #   start_date = dt_format(st)
  # )
  # check_data(pos, "positions")
  # expect_true(min(pos$RecDateTime) >= st)
  #
  # pos <- fetch_lotek_positions(
  #   start_date = dt_format(st),
  #   end_date = dt_format(end)
  # )
  # check_data(pos, "positions")
  # expect_true(min(pos$RecDateTime) >= st)
  # expect_true(max(pos$RecDateTime) <= end)
  #
  # pos <- fetch_lotek_positions(
  #   device_id = dev
  # )
  # check_data(pos, "positions")
  # expect_equal(as.integer(dev), as.integer(pos[[1, "DeviceID"]]))
  # expect_equal(as.integer(dev), as.integer(pos[[nrow(pos), "DeviceID"]]))
  #
  # pos <- fetch_lotek_positions(
  #   st <- lubridate::now(tzone = "GMT") + months(1)
  # )
  # check_data(pos, "positions")
  # expect_equal(nrow(pos), 0)

# })

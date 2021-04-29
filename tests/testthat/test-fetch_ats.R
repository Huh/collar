offline <- function() {
  !RCurl::url.exists("www.google.com")
}

check_api <- function() {
  if (offline()) {
    skip("API not available - no internet")
  }
}

check_data <- function(x, data_type) {
  expect_s3_class(x, "tbl_df")
  check_names <- switch(
    data_type,
    "config" = c(
      "CollarSerialNumber",
      "Email",
      "Active",
      "Phone Num SMS",
      "FTP Url",
      "RestEndPoint"
    ),
    "events" = c(
      "CollarSerialNumber",
      "DateCT",
      "DateUTC",
      "DateLocal",
      "Birth",
      "Fawn0",
      "Fawn1",
      "Fawn2"
    ),
    "postions" = c(
      "CollarSerialNumber",
      "Year",
      "JulianDay",
      "Hour",
      "Minute",
      "Activity",
      "Temperature",
      "Latitude",
      "Longitude",
      "HDOP",
      "NumSats",
      "FixTime",
      "2D/3D",
      "DateOffset",
      "GmtOffset",
      "DateUTC",
      "DateLocal",
      "VITTemp",
      "VITLight",
      "VITComm",
      "Fawn0Comm",
      "Fawn1Comm",
      "Fawn2Comm",
      "TransDateUTC",
      "TransDateLocal"
    ),
    "transmissions" = c(
      "CollarSerialNumber",
      "DateCT",
      "DateUTC",
      "DateLocal",
      "NumberFixes",
      "BattVoltage",
      "Mortality",
      "BreakOff",
      "GpsOnTime",
      "SatOnTime",
      "SatErrors",
      "GmtOffset",
      "LowBatt",
      "Birth",
      "Fawn0",
      "Fawn1",
      "Fawn2",
      "Latitude",
      "Longitude",
      "CEPradius_km"
    )
  )
  expect_true(
    all(
      rlang::has_name(x, check_names)
    )
  )
}

test_that("Check ats login function", {

  check_api()

  # check that bad inputs return errors
  expect_error(ats_login("baduser", "badpw"))
  expect_error(ats_login(NA, "badpw"))
  expect_error(ats_login("baduser", NA))
  expect_error(ats_login(NA, NA))
  expect_error(ats_login(NULL, "badpw"))
  expect_error(ats_login("baduser", NULL))

  # check that GET API calls fail without login
  expect_error(fetch_ats_config())
  expect_error(fetch_ats_events())
  expect_error(fetch_ats_devices())
  expect_error(fetch_ats_positions())
  expect_error(fetch_ats_transmissions())

  # test valid login
  ok <- try(ats_login("mary", ".")) == TRUE

  if (ok) {
    # check that cookie works for retrieving data
    dl <- fetch_ats_devices()
  }

  # check that logging out revokes access
  ats_logout()
  expect_error(fetch_ats_devices())

})

test_that("Check ATS data functions", {

  check_api()

  # attempt login
  ok <- try(ats_login("mary", ".")) == TRUE

  if (ok) {
    # check data retrieval functions

    # check events
    events <- fetch_ats_events()
    check_data(events, "events")

    # check devices
    dl <- fetch_ats_devices()
    expect_type(dl, "character")

    # check configuration
    cfg <- fetch_ats_config()
    check_data(cfg, "config")

    # check transmission function
    dev <- dl[1]

    trans <- fetch_ats_transmissions(new = TRUE)
    check_data(trans, "transmissions")

    trans <- fetch_ats_transmissions(new = TRUE)
    check_data(trans, "transmissions")
    expect_equal(nrow(trans), 0)

    trans <- fetch_ats_transmissions(dev)
    check_data(trans, "transmissions")
    expect_equal(dev, unique(trans$CollarSerialNumber))

    # check position function

    pos <- fetch_ats_positions(device_id = dev)
    check_data(pos, "positions")
    expect_equal(dev, unique(pos$CollarSerialNumber))

    pos <- fetch_ats_positions(device_id = dev, n = 10)
    check_data(pos, "positions")
    expect_equal(nrow(pos), 10)

    pos <- fetch_ats_positions(device_id = dev, n = 5)
    check_data(pos, "positions")
    expect_equal(nrow(pos), 5)

  }

  # TODO test start and end dates when available

})

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

lotek_token_json <- function(expires = "Mon, 01 Jan 2100 00:00:00 GMT",
                             access = "abc") {
  list(
    access_token = access,
    token_type = "bearer",
    refresh_token = "refresh123",
    userName = "usr",
    .expires = expires
  )
}

lotek_position_json <- list(
  list(
    ChannelStatus = "A", UploadTimeStamp = "2026-11-01T10:05:00",
    Latitude = 44.5, Longitude = -110.5, Altitude = 2100, ECEFx = 1L,
    ECEFy = 2L, ECEFz = 3L, RxStatus = 86L, PDOP = 1.2, MainV = 3.6,
    BkUpV = 3.5, Temperature = 12, FixDuration = 30L,
    bHasTempVoltage = TRUE, DevName = "Elk 12", DeltaTime = 0,
    FixType = 1L, CEPRadius = 0L, CRC = 0L, DeviceID = 32763L,
    RecDateTime = "2026-11-01T10:00:00"
  )
)

# fake_response(), fake_json() and http_router() come from
# helper-mock-http.R, which testthat loads but lintr does not.
# nolint start: object_usage_linter.
mock_lotek <- function(env = parent.frame(), login_status = 200L,
                       refresh_status = 200L, positions_status = 200L,
                       expires = "Mon, 01 Jan 2100 00:00:00 GMT") {
  router <- http_router(list(
    "POST API/user/login" = function(args) {
      if (identical(args$body$grant_type, "refresh_token")) {
        if (refresh_status != 200L) {
          return(fake_json(list(error = "expired"), status = refresh_status))
        }
        return(fake_json(lotek_token_json(access = "refreshed")))
      }
      if (login_status != 200L) {
        return(fake_json(
          list(error = "invalid_grant", error_description = "bad password"),
          status = login_status
        ))
      }
      fake_json(lotek_token_json(expires = expires))
    },
    "GET API/alerts" = function(args) {
      fake_json(list(list(
        nDeviceID = 32763L, strAlertType = "Mortality",
        dtTimestamp = "2026-11-01T10:00:00", dtTimestampCancel = NULL,
        strVirtualFenceType = "", strReleaseCodeOrTransmitterID = "",
        latitude = 44.5, longitude = -110.5
      )))
    },
    "GET API/devices" = function(args) {
      fake_json(list(list(
        nDeviceID = 32763L, strSpecialID = "A1",
        dtCreated = "2020-01-01T00:00:00", strSatellite = "Iridium"
      )))
    },
    "GET API/positions/findByDate" = function(args) {
      if (positions_status != 200L) {
        return(fake_json(list(), status = positions_status))
      }
      fake_json(lotek_position_json)
    }
  ))
  local_mocked_bindings(RETRY = router$retry, .package = "httr", .env = env)
  withr::defer(lotek_logout(), envir = env)
  router
}
# nolint end

test_that("lotek_login stores the token and lotek_token formats it", {
  router <- mock_lotek()
  expect_true(lotek_login("usr", "pwd"))
  expect_identical(lotek_token(), "bearer abc")
  expect_identical(router$calls()[[1]]$body$Password, "pwd")
  expect_true(lotek_logout())
  expect_error(lotek_token(), "No login information available")
})

test_that("lotek_login reports the server's message on failure (#80)", {
  mock_lotek(login_status = 400L)
  expect_error(lotek_login("usr", "bad"), "Login failed.*bad password")
})

test_that("lotek_login still accepts the deprecated user and pw arguments", {
  router <- mock_lotek()
  warns <- capture_warnings(lotek_login(user = "usr", pw = "pwd"))
  expect_length(warns, 2)
  expect_match(warns[1], "'user'")
  expect_match(warns[2], "'pw'")
  expect_identical(router$calls()[[1]]$body$username, "usr")
})

test_that("an expired token is refreshed, and a failed refresh errors", {
  router <- mock_lotek(expires = "Thu, 01 Jan 2015 00:00:00 GMT")
  lotek_login("usr", "pwd")
  expect_identical(lotek_token(), "bearer refreshed")
  expect_identical(
    router$calls()[[2]]$body$refresh_token,
    "refresh123"
  )

  mock_lotek(refresh_status = 401L)
  lotek_login("usr", "pwd")
  expect_error(lotek_refresh_token(force_refresh = TRUE), "refresh failed")
})

test_that("fetch_lotek_alerts and fetch_lotek_devices rename and type", {
  mock_lotek()
  lotek_login("usr", "pwd")

  alerts <- fetch_lotek_alerts()
  check_data(alerts, "alerts")
  expect_s3_class(alerts$AlertDate, "POSIXct")
  expect_true(is.na(alerts$CancelDate))

  dev <- fetch_lotek_devices()
  check_data(dev, "devices")
  expect_identical(dev$SpecialID, "A1")
  expect_s3_class(dev$Created, "POSIXct")
})

test_that("fetch_lotek_positions builds the query and parses fixes", {
  router <- mock_lotek()
  lotek_login("usr", "pwd")

  pos <- fetch_lotek_positions(
    device_id = c(1, 2),
    start_date = "2026-01-01T00:00:00Z",
    end_date = "2026-02-01T00:00:00Z"
  )
  query <- router$calls()[[length(router$calls())]]$query
  expect_identical(query$deviceID, "1,2")
  expect_identical(query$from, "2026-01-01T00:00:00Z")
  expect_identical(query$to, "2026-02-01T00:00:00Z")
  expect_s3_class(pos$RecDateTime, "POSIXct")
  expect_true(pos$HasTempVoltage)
  expect_identical(add_lotek_status(pos)$FixStatus, "3-D least-squares")

  fetch_lotek_positions()
  query <- router$calls()[[length(router$calls())]]$query
  expect_identical(query$from, "1970-01-01T00:00:00Z")
})

test_that("fetch_lotek_positions returns an empty typed tibble on no data", {
  mock_lotek(positions_status = 404L)
  lotek_login("usr", "pwd")
  pos <- fetch_lotek_positions()
  expect_identical(nrow(pos), 0L)
  expect_s3_class(pos$RecDateTime, "POSIXct")
})

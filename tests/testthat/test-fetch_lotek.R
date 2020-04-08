context("test-fetch_lotek.R")

offline <- function() {
  !RCurl::url.exists("www.google.com")
}

check_api <- function() {
  if (offline()) {
    skip("API not available - no internet")
  }
}

tkn <- lotek_get_token("demo", "PASSWORD09")

dt_format1 <- function(dt) {
  strftime(
    dt,
    tz = "GMT",
    format = "%a, %d %b %Y %T",
    usetz = TRUE)
}

dt_format2 <- function(dt_in) {
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

check_pos <- function(x) {
  expect_is(x, "tbl_df")
  expect_true(
    all(
      rlang::has_name(x,
                      c("UploadTimeStamp",
                        "Latitude",
                        "Longitude",
                        "DeviceID",
                        "RecDateTime"
                      )
      )
    )
  )
}

test_that("Check lotek login functions", {

  check_api()

  # check that bad inputs return errors
  expect_error(lotek_get_token("baduser", "badpw"))
  expect_error(lotek_get_token(NA, "badpw"))
  expect_error(lotek_get_token("baduser", NA))
  expect_error(lotek_get_token(NA, NA))
  expect_error(lotek_get_token(NULL, "badpw"))
  expect_error(lotek_get_token("baduser", NULL))

  # check object returned with valid inputs
  expect_is(tkn, "list")
  expect_equal(length(tkn), 7)
  expect_true(
    all(
      rlang::has_name(tkn,
                      c("access_token",
                        "token_type",
                        "refresh_token",
                        ".expires"
                      )
      )
    )
  )

  # check that refresh function works as expected
  at1 <- tkn[["access_token"]]
  tkn[[".expires"]] <-
    dt_format1(
      lubridate::now(tzone = "GMT") +
        lubridate::dminutes(5)
    )
  dl <- lotek_get_device_list(tkn)
  at2 <- tkn[["access_token"]]
  expect_false(at1 == at2)

})

test_that("Check lotek data functions", {

  check_api()

  bad_tkn <- list(
    access_token = "XXX",
    token_type = "bearer",
    expires_in = 3599,
    refresh_token = "YYY",
    userName = "BadUser",
    `.issued` =
      dt_format1(
        lubridate::now(tzone = "GMT") -
          lubridate::dminutes(5)
      ),
    `.expires` =
      dt_format1(
        lubridate::now(tzone = "GMT") +
          lubridate::dminutes(55)
      )
  )

  # check for errors with bad input
  expect_error(lotek_get_alerts(NULL))
  expect_error(lotek_get_alerts(bad_tkn))
  expect_error(lotek_get_device_list(NULL))
  expect_error(lotek_get_device_list(bad_tkn))
  expect_error(lotek_get_positions(NULL))
  expect_error(lotek_get_positions(bad_tkn))

  # check functions that don't use parameters
  alerts <- lotek_get_alerts(tkn)
  expect_is(alerts, "tbl_df")
  expect_true(
    all(
      rlang::has_name(alerts,
                      c("DeviceID",
                        "AlertType",
                        "AlertDate",
                        "CancelDate",
                        "latitude",
                        "longitude"
                      )
      )
    )
  )

  dl <- lotek_get_device_list(tkn)
  expect_is(dl, "tbl_df")
  expect_true(rlang::has_name(dl, "DeviceID"))

  # check position function
  dev <- as.character(dl[1,"DeviceID"])
  end <- lubridate::now(tzone = "GMT") - months(1)
  st <- end - months(6)



  pos <- lotek_get_positions(
    tkn,
    start_date = dt_format2(st)
  )
  check_pos(pos)
  expect_true(min(pos$RecDateTime) >= st)

  pos <- lotek_get_positions(
    tkn,
    start_date = dt_format2(st),
    end_date = dt_format2(end)
  )
  check_pos(pos)
  expect_true(min(pos$RecDateTime) >= st)
  expect_true(max(pos$RecDateTime) <= end)

  pos <- lotek_get_positions(
    tkn,
    device_id = dev
  )
  check_pos(pos)
  expect_equal(as.integer(dev), as.integer(pos[[nrow(pos), "DeviceID"]]))

})


test_that("str_extract_na keeps length and returns NA for non-matches", {
  x <- c("Elk 12 (123456789)", "no id here", NA)
  expect_identical(
    str_extract_na(x, "(?<=\\()\\d{9}(?=\\))"),
    c("123456789", NA, NA)
  )
  expect_identical(str_extract_na(character(0), "x"), character(0))
})

trktg_login_page <- paste0(
  "<html><head><title>Login</title></head><body><form>",
  "<input name='__RequestVerificationToken' value='tok123'/>",
  "</form></body></html>"
)
trktg_home_page <- "<html><head><title>Dashboard</title></head><body/></html>"
trktg_device_page <- paste0(
  "<html><body>",
  "<label for='asset_123456'>Elk 12 (123456789)</label>",
  "<label for='asset_654321'>Elk 13 (987654321)</label>",
  "</body></html>"
)
trktg_position_page <- paste0(
  "<html><body><h3>Elk 12</h3><div><table>",
  "<tr><th>Date</th><th>Received</th><th>Address</th><th>Lat/Lng</th>",
  "<th>Speed</th><th>Heading</th><th>Altitude</th><th>Via</th>",
  "<th>Near place(s)</th><th>Inside geofence(s)</th><th>Extra</th>",
  "<th>I/O</th></tr>",
  "<tr><td>11/01/2026 10:00:00</td><td>11/01/2026 10:05:00</td>",
  "<td>Somewhere</td><td>44.5, -110.5</td><td>0</td><td>N</td>",
  "<td>2100</td><td>Sat</td><td></td><td></td><td></td><td></td></tr>",
  "</table></div></body></html>"
)

# fake_response(), fake_json() and http_router() come from
# helper-mock-http.R, which testthat loads but lintr does not.
# nolint start: object_usage_linter.
mock_trktg <- function(env = parent.frame(), login_title = "Dashboard",
                       position_page = trktg_position_page) {
  router <- http_router(list(
    "GET Login" = function(args) fake_response(trktg_login_page),
    "POST Login" = function(args) {
      fake_response(sub("Dashboard", login_title, trktg_home_page))
    },
    "GET account/logoff" = function(args) fake_response(trktg_home_page),
    "GET Reports/Position" = function(args) fake_response(trktg_device_page),
    "POST Reports/Position" = function(args) fake_response(position_page)
  ))
  local_mocked_bindings(RETRY = router$retry, .package = "httr", .env = env)
  router
}
# nolint end

test_that("trktg_login stores the verification token and logout clears it", {
  router <- mock_trktg()
  expect_true(trktg_login("usr", "pwd"))
  expect_identical(trktg_token(), "tok123")
  login_post <- router$calls()[[2]]
  expect_identical(login_post$body$UserName, "usr")
  expect_identical(login_post$body$`__RequestVerificationToken`, "tok123")

  expect_true(trktg_logout())
  expect_error(trktg_token(), "No login information available")
})

test_that("trktg_login fails when the site returns the login page again", {
  mock_trktg(login_title = "Please Login")
  expect_error(trktg_login("usr", "bad"), "Login failed")
})

test_that("fetch_trktg_devices parses ids from the asset labels", {
  mock_trktg()
  trktg_login("usr", "pwd")
  on.exit(trktg_logout())
  dev <- fetch_trktg_devices()
  expect_identical(dev$api_id, c("123456", "654321"))
  expect_identical(dev$trktg_id, c("123456789", "987654321"))
  expect_identical(dev$user_id, c("Elk 12", "Elk 13"))
})

test_that("fetch_trktg_positions parses the report into typed columns", {
  router <- mock_trktg()
  trktg_login("usr", "pwd")
  on.exit(trktg_logout())

  pos <- fetch_trktg_positions(time_zone = "UTC")
  expect_named(pos, names(trktg_empty_pos))
  expect_identical(nrow(pos), 1L)
  expect_identical(pos$device_id, "Elk 12")
  expect_s3_class(pos$Date, "POSIXct")
  expect_identical(format(pos$Date), "2026-11-01 10:00:00")
  expect_identical(pos$Speed, "0")
  post <- router$calls()[[length(router$calls())]]
  expect_identical(post$body$From, "1/1/1970 00:00")
  expect_true(post$body$IsForAllAssets)

  sel <- fetch_trktg_positions(
    device_id = "Elk 13",
    start_date = as.POSIXct("2026-01-05 08:00", tz = "UTC"),
    end_date = as.POSIXct("2026-02-01 00:00", tz = "UTC"),
    time_zone = "UTC"
  )
  post <- router$calls()[[length(router$calls())]]
  expect_identical(post$body$From, "1/5/2026 08:00")
  expect_identical(post$body$To, "2/1/2026 00:00")
  expect_identical(post$body$AssetIds, "654321")
  expect_false(post$body$IsForAllAssets)
  expect_identical(nrow(sel), 1L)

  geo <- fetch_trktg_positions(time_zone = "UTC", as_sf = TRUE)
  expect_s3_class(geo, "sf")
  expect_equal(unname(sf::st_coordinates(geo)[1, ]), c(-110.5, 44.5))
})

test_that("fetch_trktg_positions returns the empty template when no data", {
  mock_trktg(position_page = "<html><body><p>No data</p></body></html>")
  trktg_login("usr", "pwd")
  on.exit(trktg_logout())
  expect_identical(fetch_trktg_positions(), trktg_empty_pos)
})

test_that("Track Tag functions require a login", {
  expect_error(fetch_trktg_devices(), "No login information available")
  expect_error(fetch_trktg_positions(), "No login information available")
})

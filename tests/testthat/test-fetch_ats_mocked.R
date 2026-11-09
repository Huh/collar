# Mocked ATS responses: the formats follow what the parsers select, so these
# exercise the package's parsing and joining code without an ATS account.

# fake_response(), fake_json() and http_router() come from
# helper-mock-http.R, which testthat loads but lintr does not.
# nolint start: object_usage_linter.
ats_xml <- function(rows) {
  cells <- vapply(
    rows,
    function(r) {
      paste0(
        "<Table>",
        paste0("<", names(r), ">", unlist(r), "</", names(r), ">",
               collapse = ""),
        "</Table>"
      )
    },
    character(1)
  )
  fake_response(
    paste0("<NewDataSet>", paste(cells, collapse = ""), "</NewDataSet>"),
    type = "text/xml; charset=utf-8",
    method = "POST"
  )
}

ats_trans_rows <- list(
  list(
    collar = "012345", fecha = "2026-11-01 10:00:00", number = "2",
    battvol = "3.6", morty = "No", breakoff = "No", gpson = "10",
    saton = "5", saterror = "0", gmt = "6", lowbatt = "false",
    ev = "None", evc = "", lat = "44.5", long = "-110.5", cpe = "3"
  )
)

ats_pos_rows <- list(
  list(
    AtsSerialNum = "012345", Year = "26", JulianDay = "305", Hour = "8",
    Minute = "0", Activity = "1", Temperature = "12", Latitude = "44.5",
    Longitude = "-110.5", Hdop = "1.2", NumSats = "7", FixTime = "30",
    Dimension = "3"
  ),
  list(
    AtsSerialNum = "012345", Year = "26", JulianDay = "305", Hour = "9",
    Minute = "0", Activity = "1", Temperature = "11", Latitude = "44.6",
    Longitude = "-110.6", Hdop = "1.1", NumSats = "8", FixTime = "25",
    Dimension = "3"
  )
)

# ATS returns only the collars named in the request ("012345," or "012345_").
for_collars <- function(rows, requested, field) {
  ids <- strsplit(requested, "[,_]")[[1]]
  Filter(function(r) r[[field]] %in% ids, rows)
}

mock_ats <- function(env = parent.frame(), pos_rows = ats_pos_rows) {
  router <- http_router(list(
    "POST Servidor.ashx" = function(args) {
      switch(
        args$body$consulta,
        get_collars_user = fake_response(
          if (args$body$valor == "no_active") {
            "<collars><collar>099999</collar></collars>"
          } else {
            "<collars><collar>012345</collar></collars>"
          },
          type = "text/xml; charset=utf-8", method = "POST"
        ),
        download_trans_collars = ats_xml(
          for_collars(ats_trans_rows, args$body$collar, "collar")
        ),
        download_txt_collars = ats_xml(
          for_collars(pos_rows, args$body$collars, "AtsSerialNum")
        )
      )
    },
    "GET download_collar_config/Download_Collar_Config.aspx" = function(args) {
      fake_response(
        paste0(
          "CollarSerialNumber,Active,RestEndPoint,A,B,C,D,E,F,G,SMS LowBatt\n",
          "0012345,yes,no,yes,no,yes,no,yes,no,yes,no\n"
        ),
        type = "text/csv"
      )
    },
    "GET download_all_events/download_all_events.aspx?dw=all" = function(args) {
      fake_response(
        paste0(
          "CollarSerialNumber,DateSent,Event\n",
          "012345,11/01/2026 10:00:00 AM,Mort\n"
        ),
        type = "text/csv"
      )
    }
  ))
  local_mocked_bindings(RETRY = router$retry, .package = "httr", .env = env)
  local_mocked_bindings(has_internet = function() TRUE, .package = "curl",
                        .env = env)
  local_mocked_bindings(check_cookie = function(...) TRUE, .env = env)
  router
}
# nolint end

test_that("fetch_ats_devices combines active and inactive collars", {
  router <- mock_ats()
  expect_identical(fetch_ats_devices(), c("012345", "099999"))
  expect_identical(fetch_ats_devices("active"), "012345")
  expect_warning(
    devs <- fetch_ats_devices("nonsense"),
    "Unrecognized filter"
  )
  expect_identical(devs, c("012345", "099999"))
  valores <- vapply(router$calls(), function(x) x$body$valor, character(1))
  expect_true(all(c("active", "no_active") %in% valores))
})

test_that("fetch_ats_transmissions parses the XML into typed columns", {
  mock_ats()
  tr <- fetch_ats_transmissions(device_id = "012345")
  expect_named(tr, names(ats_empty_trans))
  expect_identical(tr$NumberFixes, 2L)
  expect_identical(tr$GmtOffset, -6)
  expect_false(tr$LowBatt)
  expect_identical(tr$Fawn0, "None")
  expect_identical(tr$CEPradius_km, 3L)
  expect_identical(attr(tr$DateUTC, "tzone"), "UTC")
})

test_that("fetch_ats_positions joins fixes to transmissions", {
  router <- mock_ats()
  pos <- fetch_ats_positions(chunk_size = 1)
  expect_named(pos, names(ats_empty_pos))
  expect_identical(nrow(pos), 2L)
  expect_identical(pos$NumSats, c(7L, 8L))
  expect_identical(pos$GmtOffset, c(-6, -6))
  expect_identical(
    format(pos$DateUTC, tz = "UTC"),
    c("2026-11-01 14:00:00", "2026-11-01 15:00:00")
  )
  # two collars at chunk_size = 1 means two position requests
  pos_calls <- Filter(
    function(x) identical(x$body$consulta, "download_txt_collars"),
    router$calls()
  )
  expect_length(pos_calls, 2)
  expect_identical(pos_calls[[1]]$body$type, "004")
})

test_that("fetch_ats_positions handles n, deprecated args and empty data", {
  router <- mock_ats(pos_rows = list())
  expect_identical(
    fetch_ats_positions(device_id = "012345", n = 5),
    ats_empty_pos
  )
  last <- router$calls()[[length(router$calls())]]
  expect_identical(last$body$type, "002")
  expect_warning(
    fetch_ats_positions(device_id = "012345", start = Sys.Date()),
    "Filtering by date is currently unavailable"
  )
  expect_warning(
    fetch_ats_positions(device_id = "012345", new = TRUE),
    "deprecated"
  )
})

test_that("fetch_ats_config and fetch_ats_events parse the CSV downloads", {
  mock_ats()
  cfg <- fetch_ats_config()
  expect_identical(cfg$CollarSerialNumber, "012345")
  expect_true(cfg$Active)
  expect_false(cfg$`SMS LowBatt`)

  ev <- fetch_ats_events()
  expect_s3_class(ev$DateCT, "POSIXct")
  expect_identical(
    format(ev$DateUTC, tz = "UTC"),
    "2026-11-01 16:00:00"
  )
})

test_that("ATS text-format parsers handle the GET download formats", {
  pos_csv <- fake_response(
    paste0(
      "CollarSerialNumber,Year,Julianday,Hour,Minute,Activity,Temperature,",
      "Latitude,Longitude,HDOP,NumSats,FixTime,2D/3D,Extra\n",
      "012345,26,305,8,0,1,12,44.5,-110.5,1.2,7,30,3,x\n",
      "012345,26,305,9,0,1,-3,44.6,-110.6,1.1,8,25,3,x\n"
    ),
    type = "text/csv"
  )
  trans_csv <- fake_response(
    paste0(
      "CollarSerialNumber,DateCT,NumberFixes,BattVoltage,Mortality,",
      "BreakOff,GpsOnTime,SatOnTime,SatErrors,GmtOffset,LowBatt,Event,",
      "Latitude,Longitude,CEPradius_km\n",
      "012345,11/01/2026 10:00:00 AM,2,3.6,No,No,10,5,0,6,No,None,",
      "44.5,-110.5,3\n"
    ),
    type = "text/csv"
  )
  tr <- ats_parse_trans(trans_csv)
  expect_identical(tr$GmtOffset, -6)
  expect_identical(tr$Birth, "None")

  pos <- ats_parse_pos(pos_csv, tr)
  expect_identical(pos$Temperature, c(12L, -3L))
  expect_identical(nrow(pos), 2L)

  html <- fake_response("<html><body>No data</body></html>")
  expect_identical(ats_parse_pos(html, tr), ats_empty_pos)
  expect_identical(ats_parse_trans(html), ats_empty_trans)
  expect_error(ats_parse_pos(list()), "Invalid response")
})

test_that("ATS requests refuse to run without a login or a valid path", {
  local_mocked_bindings(has_internet = function() TRUE, .package = "curl")
  local_mocked_bindings(check_cookie = function(...) FALSE)
  expect_error(ats_get(list("x")), "log in first")
  expect_error(ats_post(list("x"), list(a = 1)), "log in first")
})

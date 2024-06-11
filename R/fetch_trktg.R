################################################################################
#
# Track Tag Download Functions
#
################################################################################

# 1 - Global Objects ------------------------------------------------------

# * 1.1 - trktg_empty_pos -------------------------------------------------

# tibble with zero rows matching track tag position file format

trktg_empty_pos <- tibble::tibble(
  device_id = character(0),
  Date = as.POSIXct(NA),
  Received = as.POSIXct(NA),
  Address = character(0),
  `Lat/Lng` = character(0),
  Speed = character(0),
  Heading = character(0),
  Altitude = character(0),
  Via = character(0),
  `Near place(s)` = character(0),
  `Inside geofence(s)` = character(0),
  Extra = character(0),
  `I/O` = character(0)
)

# 2 - Download Functions --------------------------------------------------

# * 2.1 - fetch_trktg_positions -------------------------------------------

#' @title Download Position (Fix) Data from track tag website
#'
#' @description Retrieves GPS data optionally filtered by date or collar.
#'
#' @param device_id A single device id, or a list or vector of device ids,
#'   or NULL for all devices associated with current account. Device ids
#'   can be found on the track tag portal (Device ID/UID), or by examining the
#'   `user_id` column in the tibble returned by \code{fetch_trktg_devices}.
#' @param start_date Only fixes at or after start_date will be included
#'   in the output. Must be a POSIX date or date/time object. If NULL
#'   Jan 1 1970 is used.
#' @param end_date Only fixes at or before end_date are included,
#'   analagous to start_date above. If NULL current date/time is used.
#' @param time_zone Time zone used to convert date values to \code{POSIX}.
#' @param as_sf Boolean indicating if the result should be converted to an
#'   \code{sf} object.
#' @param sf_crs Coordinate referenece system for converting to \code{sf}. Only
#'   CRS 4326 (WGS84) has been tested, coordinates may not be parsed correctly
#'   for other systems.
#'
#' @return A tibble or sf object with 21 columns:
#' \describe{
#'   \item{device_id}{User label assigned through track tag portal (character)}
#'   \item{Date}{Fix date/time (POSIXct)}
#'   \item{Received}{Date/time received via satellite (character)}
#'   \item{Address}{Address nearest the GPS fix (character)}
#'   \item{Lat/Lng}{GPS coordinates (character)}
#'   \item{Speed}{Speed with units (character)}
#'   \item{Heading}{Unknown (character)}
#'   \item{Altitude}{Unknown (character)}
#'   \item{Via}{Transmission method (character)}
#'   \item{Near place(s)}{Unknown (character)}
#'   \item{Inside geofence(s)}{Unknown (character)}
#'   \item{Extra}{Unknown (character)}
#'   \item{I/O}{Unknown (character)}
#' }
#'
#' @seealso \code{\link{fetch_trktg_devices}} for downloading a list of
#'   transmitters associated with the current account.
#'
#' @export
#'
#' @examples
#'
#' trktg_login("some_user", "some_users_pw")
#'
#' # all fixes for all transmitters in 2024
#' fixes <- fetch_trktg_positions(
#'   start_date = "2024-01-01 00:00:00",
#'   end_date = "2025-01-01 00:00:00"
#' )
#'
#' # all fixes for a single transmitter
#' fixes_single_tr <- fetch_trktg_positions(device_id = "012345678")
#'
#' # all fixes for multiple transmitters
#' fixes_2_devices <- fetch_trktg_positions(
#'   device_id = c("012345678", "012345679")
#' )
#'
#' # fixes from the last week for a single transmitter
#' fixes <- fetch_trktg_positions(
#'   device_id = "012345678",
#'   start_date = Sys.Date() - 7
#' )
#'
#' # fixes from the last week as an sf object
#' fixes <- fetch_trktg_positions(
#'   start_date = Sys.Date() - 7,
#'   as_sf = TRUE
#' )
#'
#' trktg_logout()
#'
fetch_trktg_positions <- function(device_id = NULL,
                                  start_date = NULL,
                                  end_date = NULL,
                                  time_zone = Sys.timezone(),
                                  as_sf = FALSE,
                                  sf_crs = 4326) {

  # get login info
  # function will exit here if login info is invalid
  tkn <- trktg_token()

  body <- list(
    `__RequestVerificationToken` = tkn,
    IsForAllAssets = is.null(device_id),
    IsAppliedToGroups = FALSE,
    IsAppliedToAssets = !is.null(device_id)
  )

  if (!is.null(device_id)) {

    dev <- fetch_trktg_devices() %>%
      dplyr::filter(.data$user_id %in% device_id) %>%
      dplyr::pull(.data$api_id)

    dev <- dev %>%
      rep(2) %>%
      stats::setNames(c(
        rep("CheckAssetIds", length(dev)),
        rep("AssetIds", length(dev))
      ))

    body <- c(body, as.list(dev))

  }

  body$From <- dplyr::if_else(
    is.null(start_date),
    "1/1/1970 00:00",
    format(start_date, "%m/%d/%Y %R")
  ) %>%
    stringr::str_replace_all("0(\\d/)", "\\1")

  body$To <- dplyr::if_else(
    is.null(end_date),
    format(Sys.time(), "%m/%d/%Y %R"),
    format(end_date, "%m/%d/%Y %R")
  ) %>%
    stringr::str_replace_all("0(\\d/)", "\\1")

  # send request
  resp <- httr::RETRY(
    "POST",
    url = trktg_base_url,
    path = list("Reports", "Position"),
    body = body,
    encode = "form",
    quiet = TRUE
  )

  # verify successful status
  assertthat::assert_that(
    httr::status_code(resp) == 200,
    msg = paste(
      "API call failed - device list.",
      paste("Status:", httr::status_code(resp)),
      paste("Response:", httr::content(resp)),
      sep = "\n"
    )
  )

  tbls <- resp %>%
    httr::content() %>%
    rvest::html_table()

  if (length(tbls)) {

    ids <- resp %>%
      httr::content() %>%
      xml2::xml_find_all("//div[table]/preceding-sibling::h3") %>%
      xml2::xml_text()

    out <- purrr::map2(
      ids,
      tbls,
      function(id, pos) {
        tibble::tibble(device_id = id, pos)
      }
    ) %>%
      dplyr::bind_rows() %>%
      dplyr::select(dplyr::all_of(names(trktg_empty_pos))) %>%
      dplyr::mutate(
        dplyr::across(
          .data$Date:.data$Received,
          lubridate::mdy_hms,
          tz = time_zone
        )
      ) %>%
      purrr::map2(
        lapply(trktg_empty_pos, class),
        function(col, cls) {
          methods::as(col, cls[1])
        }
      ) %>%
      tibble::as_tibble()

  } else {

    out <- trktg_empty_pos

  }

  if (as_sf) {

    out <- out %>%
      dplyr::mutate(
        lng = stringr::str_extract(.data$`Lat/Lng`, "[0-9.-]+$"),
        lat = stringr::str_extract(.data$`Lat/Lng`, "^[0-9.-]+")
      ) %>%
      sf::st_as_sf(coords = c("lng", "lat"), crs = sf_crs)

  }

  out

}

# * 2.2 - fetch_trktg_devices ---------------------------------------------

#' @title Download Device List from track tag web portal
#'
#' @description Retrieves a list of devices (transmitters) associated
#'   with the account currently logged in via \code{trktg_login}.
#'
#' @return A tibble with 3 character columns:
#' \describe{
#'   \item{api_id}{Six digit internal id used in api calls},
#'   \item{trktg_id}{Nine digit transmitter id (serial number)},
#'   \item{user_id}{User label assigned through track tag portal}
#' }
#'
#' @seealso \code{\link{fetch_trktg_positions}} for downloading GPS data.
#'
#' @export
#'
#' @examples
#'
#' trktg_login("some_user", "some_users_pw")
#'
#' tt_devices <- fetch_trktg_devices()
#'
#' tt_fixes <- fetch_trktg_positions(device_id = tt_devices$user_id[1])
#'
#' trktg_logout()
#'
fetch_trktg_devices <- function() {

  # get login info
  # function will exit here if login info is invalid
  tkn <- trktg_token()

  # send request
  resp <- httr::RETRY(
    "GET",
    url = trktg_base_url,
    path = list("Reports", "Position"),
    body = list(
      `__RequestVerificationToken` = tkn
    ),
    encode = "form",
    quiet = TRUE
  )

  # verify successful status
  assertthat::assert_that(
    httr::status_code(resp) == 200,
    msg = paste(
      "API call failed - device list.",
      paste("Status:", httr::status_code(resp)),
      paste("Response:", httr::content(resp)),
      sep = "\n"
    )
  )

  # extract device ids from xml
  label_nodes <- resp %>%
    httr::content() %>%
    xml2::xml_find_all("//label[contains(@for, 'asset')]")

  tibble::tibble(
    api_id = label_nodes %>%
      xml2::xml_attr("for") %>%
      stringr::str_extract("\\d{6}"),
    trktg_id = label_nodes %>%
      xml2::xml_text() %>%
      stringr::str_extract("(?<=\\()\\d{9}(?=\\))"),
    user_id = label_nodes %>%
      xml2::xml_text() %>%
      stringr::str_extract("^.+(?= \\(\\d{9}\\))")
  )

}

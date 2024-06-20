################################################################################
#
# Lotek Download Functions
#
################################################################################

#' @title Download Alerts via Lotek API
#'
#' @description Retrieves alerts for all devices associated with the account
#'   currently logged in via lotek_login.
#'
#' @return A tibble with information about mortalities, births, etc.
#'
#' @seealso \code{\link{fetch_lotek_devices}} for downloading a list of
#'   available devices, and \code{\link{fetch_lotek_positions}} for downloading
#'   GPS data.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' lotek_login("demo", "PASSWORD09")
#'
#' # get all alerts for this account
#' alerts <- fetch_lotek_alerts()
#' }
fetch_lotek_alerts <- function() {

  # get login info
  # function will exit here if login info is invalid
  tkn <- lotek_token()

  # send request
  resp <- httr::RETRY(
    "GET",
    url = lotek_base_url,
    config = httr::add_headers(
      Authorization = tkn
    ),
    path = list("API", "alerts"),
    quiet = TRUE
  )

  # verify successful status
  assertthat::assert_that(
    httr::status_code(resp) == 200,
    msg = paste(
      "API call failed - alerts.",
      paste("Status:", httr::status_code(resp)),
      paste("Response:", httr::content(resp)),
      sep = "\n"
    )
  )

  # combine lists into tibble
  tbl_out <- purrr::map_dfr(
    httr::content(resp),
    ~ purrr::map(.x, function(y) ifelse(is.null(y), NA, y)) %>%
      tibble::as_tibble()
  )

  # clean up some column names and types
  if (nrow(tbl_out) > 0) {
    tbl_out <- tbl_out %>%
      dplyr::rename(
        DeviceID = .data$nDeviceID,
        AlertType = .data$strAlertType,
        AlertDate = .data$dtTimestamp,
        CancelDate = .data$dtTimestampCancel,
        VirtualFenceType = .data$strVirtualFenceType,
        ReleaseCodeOrTransmitterID = .data$strReleaseCodeOrTransmitterID
      ) %>%
      dplyr::mutate(
        AlertDate = lubridate::as_datetime(.data$AlertDate),
        CancelDate = lubridate::as_datetime(.data$CancelDate)
      )
  }

  tbl_out

}

#' @title Download Device List via Lotek API
#'
#' @description Retrieves a list of devices (collars) associated
#'   with the account currently logged in via lotek_login.
#'
#' @return A tibble with device information.
#'
#' @seealso \code{\link{fetch_lotek_alerts}} for downloading alerts such as
#'   mortality events, and \code{\link{fetch_lotek_positions}} for downloading
#'   GPS data.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' lotek_login("demo", "PASSWORD09")
#'
#' # get list of collars for this account
#' collars <- fetch_lotek_devices()
#'
#' # get fixes for the first collar
#' fixes <- fetch_lotek_positions(device_id = collars[[1, "DeviceID"]])
#' }
fetch_lotek_devices <- function() {

  # get login info
  # function will exit here if login info is invalid
  tkn <- lotek_token()

  # send request
  resp <- httr::RETRY(
    "GET",
    url = lotek_base_url,
    config = httr::add_headers(
      Authorization = tkn
    ),
    path = list("API", "devices"),
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

  # combine list into tibble and clean up names/types
  purrr::map_dfr(
    httr::content(resp),
    ~tibble::as_tibble(.x)
  ) %>%
    dplyr::rename(
      DeviceID = .data$nDeviceID,
      SpecialID = .data$strSpecialID,
      Created = .data$dtCreated,
      Satellite = .data$strSatellite
    ) %>%
    dplyr::mutate(Created = lubridate::as_datetime(.data$Created))

}

#' @title Download Position (Fix) Data via Lotek API
#'
#' @description Retrieves GPS data optionally filtered by date or collar.
#'
#' @section Notes:
#'
#'   Unlike other fetch_lotek... functions in this package, this will not
#'   return an error if the API call fails. This behavior is intentional and
#'   due to the fact that failures/errors and successful calls that did not
#'   return any data are indistinguishable on the client side. If the API
#'   response indicates an error (for either reason) an empty tibble is
#'   returned with the same columns as a successful download.
#'
#' @param device_id A single device id, or a list or vector of device ids,
#'   or NULL for all devices associated with current account.
#' @param start_date Only fixes at or after start_date will be included
#'   in the output. Must be in the format YYYY-MM-DD HH:MM:SS. If NULL
#'   Jan 1 1970 is used.
#' @param end_date Only fixes at or before end_date are included,
#'   analagous to start_date above. If NULL current date/time is used.
#'
#' @return A tibble containing position data, or an empty tibble in the
#'   same format is no rows are returned.
#'
#' @seealso \code{\link{fetch_lotek_alerts}} for downloading alerts such as
#'   mortality events, and \code{\link{fetch_lotek_devices}} for downloading
#'   a list of collars associated with the current account.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' lotek_login("demo", "PASSWORD09")
#'
#' # all fixes for all collars in 2020
#' fixes <- fetch_lotek_positions(start_date = "2020-01-01 00:00:00",
#'                                     end_date = "2021-01-01 00:00:00")
#'
#' # all fixes for collar 32763
#' fixes_32763 <- fetch_lotek_positions(device_id = 32763)
#'
#' # all fixes for collars 32763, 34023, and 42492
#' fixes_3_cols <- fetch_lotek_positions(device_id = c(32763, 34023, 42492))
#'
#' # fixes for collar 32763 in 2018
#' fixes <- fetch_lotek_positions(device_id = 32763,
#'                                start_date = "2018-01-01 00:00:00",
#'                                end_date = "2019-01-01 00:00:00")
#' }
fetch_lotek_positions <- function(device_id = NULL,
                                  start_date = NULL,
                                  end_date = NULL) {

  # get login info
  # function will exit here if login info is invalid
  tkn <- lotek_token()

  st <- dplyr::if_else(
    is.null(start_date),
    "1970-01-01T00:00:00Z",
    start_date)

  end <- dplyr::if_else(
    is.null(end_date),
    paste0(
      gsub(" ", "T", lubridate::now(tzone = "GMT")),
      "Z"),
    end_date)

  dev <- paste0(
    device_id,
    collapse = ",")

  # send request
  resp <- httr::RETRY(
    "GET",
    url = lotek_base_url,
    config = httr::add_headers(
      Authorization = tkn
    ),
    path = list("API", "positions", "findByDate"),
    query = as.list(c(from = st, to = end, deviceID = dev)),
    quiet = TRUE
  )

  # verify successful status
  if (httr::status_code(resp) == 200) {

    # combine lists into tibble
    purrr::map_dfr(
      httr::content(resp),
      ~tibble::as_tibble(.x)
    ) %>%
      dplyr::mutate(
        RecDateTime = lubridate::as_datetime(.data$RecDateTime),
        UploadTimeStamp = lubridate::as_datetime(.data$UploadTimeStamp)
      ) %>%
      dplyr::rename(
        HasTempVoltage = .data$bHasTempVoltage
      )

  } else {
    # API returns an error if filtered to 0 rows
    # unfortunately there is no way to tell from the response
    #   if an error occurred or no rows were returned
    # this ignores the error and returns an empty tibble with
    #   the same columns as a successful API call
    tibble::tibble(
      ChannelStatus = character(),
      UploadTimeStamp =  .POSIXct(double()),
      Latitude = double(),
      Longitude = double(),
      Altitude = double(),
      ECEFx = integer(),
      ECEFy = integer(),
      ECEFz = integer(),
      RxStatus = integer(),
      PDOP = double(),
      MainV = double(),
      BkUpV = double(),
      Temperature = double(),
      FixDuration = integer(),
      HasTempVoltage = logical(),
      DevName = character(),
      DeltaTime = integer(),
      FixType = integer(),
      CEPRadius = integer(),
      CRC = integer(),
      DeviceID = integer(),
      RecDateTime = .POSIXct(double())
    )

  }

}

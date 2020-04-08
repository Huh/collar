lotek_base_url <- "https://webservice.lotek.com"

#' Initial login to lotek web service
#'
#' @description Send username and password info to Lotek API
#'   to retrieve authentication token
#'
#' @param user username
#' @param pw password
#'
#' @return a list containing the json response,
#'   inluding access_token, refresh_token, and .expires
#'   This list is reused for authenticating subsequent calls
#'
#' @export
#'
#' @examples
#'
#' tkn <- lotek_get_token("demo", "PASSWORD09")
#'
lotek_get_token <- function(user, pw) {

  # call API
  resp <- httr::POST(
    url = lotek_base_url,
    path = list("API", "user", "login"),
    body = list(
      grant_type = "password",
      username = user,
      Password = pw
    ),
    encode = "form"
  )

  # check call was successful
  assertthat::assert_that(
    httr::status_code(resp) == 200,
    msg = "API call failed"
  )

  # convert json to list object
  httr::content(resp)

}

#' Check and when appropriate refresh lotek auth token
#'
#' @param lotek_auth a list matching the output of lotek_get_token
#'
#' @return the same list, or an updated list if tokens are refreshed
#'
#' @keywords internal
#'
lotek_check_token <- function(lotek_auth) {

  # get token expiration and current time
  exp_date <- lubridate::dmy_hms(lotek_auth[[".expires"]])
  now_date <- lubridate::now(tzone = "GMT")

  # if token is already expired a new login is required using lotek_get_token
  assertthat::assert_that(
    exp_date > now_date,
    msg = "Login timed out. Please log in to your Lotek account again.")

  # if 15 min or less are left until exp refresh token,
  #   otherwise return original token unchanged
  if (exp_date <= (now_date + lubridate::dminutes(15))) {
    refr_auth <- lotek_refresh_token(lotek_auth)
    return(lotek_check_token(refr_auth))
  } else {
    return(lotek_auth)
  }

}

#' Format current access token
#'
#' @description Checks token is valid and returns as character for API calls.
#'   Note that the current token in the environment is intentionally
#'   overwritten in this function, in case it was refreshed in the call to
#'   lotek_check_auth_token
#'
#' @param lotek_auth a list matching the output of lotek_get_token
#'
#' @return access token as string for authenticating API calls
#'
#' @keywords internal
#'
lotek_auth_token <- function(lotek_auth) {

  # check for valid input object
  assertthat::assert_that(
    assertthat::has_name(lotek_auth,
      c("token_type", "access_token", ".expires")),
    msg = "Invalid Lotek token. Are you logged in?")

  # replace input object with same or refreshed object
  eval.parent(substitute(lotek_auth <- lotek_check_token(lotek_auth)))

  # return as character for API header
  paste(lotek_auth["token_type"], lotek_auth["access_token"])

}

#' Refresh current access token
#'
#' @description Retrieves new authentication token from Lotek API
#'
#' @param lotek_auth a list matching the output of lotek_get_token
#'
#' @return a list matching the input with new values
#'
#' @keywords internal
#'
lotek_refresh_token <- function(lotek_auth) {

  # send request
  resp <- httr::POST(
    url = lotek_base_url,
    path = list("API", "user", "login"),
    body = list(
      grant_type = "refresh_token",
      username = lotek_auth[["userName"]],
      refresh_token = lotek_auth[["refresh_token"]]
    ),
    encode = "form"
  )

  # verify successful status
  assertthat::assert_that(
    httr::status_code(resp) == 200,
    msg = "API call failed"
  )

  # return response as list
  httr::content(resp)

}

#' Retrieve alerts from Lotek site via API
#'
#' @description Retrieves alert data from the lotek API.
#'   Retrieves alerts for all devices associated with the account
#'   currently logged in via lotek_auth.
#'
#' @param lotek_auth a list matching the output of lotek_get_token
#'
#' @return A tibble with alert data
#'
#' @export
#'
#' @examples
#'
#' tkn <- lotek_get_token("demo", "PASSWORD09")
#'
#' ltk_alerts <- lotek_get_alerts(tkn)
#' }
lotek_get_alerts <- function(lotek_auth) {

  # send request
  resp <- httr::GET(
    url = lotek_base_url,
    config = httr::add_headers(
      Authorization = lotek_auth_token(lotek_auth)
    ),
    path = list("API", "alerts")
  )

  # verify successful status
  assertthat::assert_that(
    httr::status_code(resp) == 200,
    msg = "API call failed"
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
        DeviceID = nDeviceID,
        AlertType = strAlertType,
        AlertDate = dtTimestamp,
        CancelDate = dtTimestampCancel,
        VirtualFenceType = strVirtualFenceType,
        ReleaseCodeOrTransmitterID = strReleaseCodeOrTransmitterID
      ) %>%
      dplyr::mutate(
        AlertDate = lubridate::as_datetime(AlertDate),
        CancelDate = lubridate::as_datetime(CancelDate)
      )
  }

  tbl_out

}

#' Retrieve device list from Lotek site via API
#'
#' @description Retrieves a list of devices (collars) associated
#'   with the account currently logged in via lotek_auth.
#'
#' @param lotek_auth a list matching the output of lotek_get_token
#'
#' @return A tibble with device information
#' @export
#'
#' @examples
#'
#' tkn <- lotek_get_token("demo", "PASSWORD09")
#'
#' ltk_collars <- lotek_get_device_list(tkn)
#' 
lotek_get_device_list <- function(lotek_auth) {

  # send request
  resp <- httr::GET(
    url = lotek_base_url,
    config = httr::add_headers(
      Authorization = lotek_auth_token(lotek_auth)
    ),
    path = list("API", "devices")
  )

  # verify successful status
  assertthat::assert_that(
    httr::status_code(resp) == 200,
    msg = "API call failed"
  )

  # combine list into tibble and clean up names/types
  purrr::map_dfr(
      httr::content(resp),
      ~tibble::as_tibble(.x)
    ) %>%
    dplyr::rename(
      DeviceID = nDeviceID,
      SpecialID = strSpecialID,
      Created = dtCreated,
      Satellite = strSatellite
    ) %>%
    dplyr::mutate(Created = lubridate::as_datetime(Created))

}

#' Retrieve position (fix) data from Lotek site via API
#'
#' @description Retrieves GPS data optionally filtered by date or collar.
#'
#' @param lotek_auth a list matching the output of lotek_get_token
#' @param device_id a single device id as character, or a
#'   list or vector of device ids, or NULL for all devices
#'   associated with current account
#' @param start_date only fixes at or after start_date will be inlcluded
#'   in the output. Must be in the format YYYY-MM-DDTHH:MM:SSZ. Don't
#'   forget the 'T' and 'Z'.  If NULL Jan 1 1970 is used.
#' @param end_date only fixes at or before end_date are included,
#'   analagous to start_date above.  If NULL current date/time is used.
#'
#' @return A tibble containing position data
#' @export
#'
#' @examples
#'
#' tkn <- lotek_get_token("demo", "PASSWORD09")
#'
#' ltk_fixes_2020 <- lotek_get_positions(tkn,
#'                                       start_date = "2020-01-01T00:00:00Z",
#'                                       end_date = "2021-01-01T00:00:00Z")
#'
#' ltk_fixes_collar32763 <- lotek_get_positions(tkn,
#'                                            device_id = "32763")
#'
lotek_get_positions <- function (lotek_auth,
                                        device_id = NULL,
                                        start_date = NULL,
                                        end_date = NULL) {

  # send request
  resp <- httr::GET(
    url = lotek_base_url,
    config = httr::add_headers(
      Authorization = lotek_auth_token(lotek_auth)
    ),
    path = list("API", "positions", "findByDate"),
    query = as.list(c(
      from = dplyr::if_else(is.null(start_date), "1970-01-01T00:00:00Z", start_date),
      to = dplyr::if_else(is.null(end_date),
                          paste0(
                            gsub(" ", "T", lubridate::now(tzone = "GMT")),
                            "Z"), end_date),
      deviceID = paste0(device_id, collapse = ",")
    ))
  )

  # verify successful status
  assertthat::assert_that(
    httr::status_code(resp) == 200,
    msg = "API call failed"
  )

  # combine lists into tibble
  tbl_out <- purrr::map_dfr(
    httr::content(resp),
    ~tibble::as_tibble(.x)
  )

  # set data type for date columns
  if (nrow(tbl_out) > 0) {
    dplyr::mutate(tbl_out,
                  RecDateTime = lubridate::as_datetime(RecDateTime),
                  UploadTimeStamp = lubridate::as_datetime(UploadTimeStamp))
  }

  tbl_out

}

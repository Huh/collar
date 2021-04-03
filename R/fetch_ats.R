# 1 - Internal Functions --------------------------------------------------

# * 1.1 - ats_get ---------------------------------------------------------

#' @title ATS GET
#'
#' @description Submit an http GET request to the ATS website
#'
#' @param path Character or list for the request path
#' @param task Character describing the purpose of the current request.
#'   If the request fails the message 'Failed to [task]' is displayed.
#'
#' @return Response object
#'
#' @seealso \code{\link{httr::GET}}
#'
#' @export
#'
#' @keywords internal
#'
#' @examples
#'
#' ats_get(
#'   path = list(
#'     "download_all_events",
#'     "download_all_events.aspx?dw=all"
#'   ),
#'   task = "download event data"
#' ) %>%
#'   ats_parse_txt()
#'
ats_get <- function(path, task = "download data") {

  # check internet
  assertthat::assert_that(
    curl::has_internet,
    msg = "No internet connection available."
  )

  # check login
  assertthat::assert_that(
    check_cookie(ats_base_url, "user"),
    msg = "You need to log in first."
  )

  # check path parameter
  assertthat::assert_that(
    !missing(path),
    assert_that::not_empty(path),
    inherits(path, "list") | inherits(path, "character"),
    msg = "Incorrect path parameter supplied to GET request."
  )

  httr::RETRY(
    "GET",
    url = ats_base_url,
    path = path,
    quiet = TRUE
  ) %>%
    httr::stop_for_status(task)

}

# * 1.2 - ats_parse_trans -------------------------------------------------

# convert text response to dataframe
# special handling for transmission data returned as text
ats_parse_trans <- function(resp) {

  assertthat::assert_that(
    inherits(resp, "response"),
    msg = "Invalid response passed to parsing function."
  )

  if (inherits(resp$content, "raw")) {

    # quote unquoted separators and return
    httr::content(resp, "text", encoding = "UTF-8") %>%
      textConnection() %>%
      readLines() %>%
      magrittr::extract(nchar(.) > 0) %>%
      repair_lines(15, 12) %>%
      readr::read_csv(col_types = "ccidcciiidccddi") %>%
      dplyr::mutate(
        DateCT = lubridate::as_datetime(
          DateCT,
          tz = "America/Menominee",
          format = "%m/%d/%Y %I:%M:%S %p"
        ),
        DateUTC = lubridate::with_tz(DateCT, tz = "UTC"),
        DateLocal = lubridate::with_tz(DateCT, tz = Sys.timezone())
      ) %>%
      dplyr::relocate(DateUTC, DateLocal, .after = DateCT)

  } else {

    # api returns html when no new data is available
    # return empty tibble
    tibble::tibble(
      CollarSerialNumber = character(0),
      DateCT = as.POSIXct(numeric(0)),
      DateUTC = as.POSIXct(numeric(0)),
      DateLocal = as.POSIXct(numeric(0)),
      NumberFixes = integer(0),
      BattVoltage = numeric(0),
      Mortality = character(0),
      BreakOff = character(0),
      GpsOnTime = integer(0),
      SatOnTime = integer(0),
      SatErrors = integer(0),
      GmtOffset = numeric(0),
      LowBatt = character(0),
      Event = character(0),
      Latitude = numeric(0),
      Longitude = numeric(0),
      CEPradius_km = integer(0)
    )

  }

}

# * 1.3 - ats_parse_txt ---------------------------------------------------

# convert text response to dataframe
ats_parse_txt <- function(resp) {

  assertthat::assert_that(
    inherits(resp, "response"),
    msg = "Invalid response passed to parsing function."
  )

  httr::content(resp, "text", encoding = "UTF-8") %>%
    readr::read_csv()

}

# * 1.4 - ats_parse_xml ---------------------------------------------------

# convert xml response to dataframe
ats_parse_xml <- function(resp) {

  assertthat::assert_that(
    inherits(resp, "response"),
    msg = "Invalid response passed to parsing function."
  )

  cols <- httr::content(resp) %>%
    xml2::xml_find_first("//Table") %>%
    xml2::xml_children() %>%
    xml2::xml_name()

  names(cols) <- cols

  purrr::map_dfc(
    cols,
    ~ httr::content(resp) %>%
      xml2::xml_find_all(paste0("//", .x)) %>%
      xml2::xml_text()
  )

}

# * 1.5 - ats_pos_all -----------------------------------------------------

# use the url to download position data for all collars
ats_pos_all <- function(new = FALSE) {

  type <- purrr::when(
    new,
    isTRUE(.) ~ "new",
    ~ "all"
  )

  resp <- ats_get(
    path = list(
      "download_all_data",
      paste0("Download_all_data.aspx?dw=", type)
    ),
    task = "download position data"
  )

  if (inherits(resp$content, "raw")) {
    ats_parse_txt(resp)
  } else {
    tibble::tibble(
      CollarSerialNumber = integer(0),
      Year = integer(0),
      Julianday = integer(0),
      Hour = integer(0),
      Minute = integer(0),
      Activity = integer(0),
      Temperature = integer(0),
      Latitude = numeric(0),
      Longitude = numeric(0),
      HDOP = numeric(0),
      NumSats = integer(0),
      FixTime = integer(0),
      X2D.3D = integer(0),
      Date = character(0)
    )
  }

}

# * 1.6 - ats_pos_filter --------------------------------------------------

# use a POST request to download position data for selected collars
ats_pos_filter <- function(collars = NULL,
                           start = NULL,
                           end = NULL,
                           n = NULL) {

  # check for valid n values
  assertthat::validate_that(
    any(is.null(n), n %in% c(5, 10)),
    msg = paste(
      "Only 5 and 10 are valid options for last n filters.",
      "Parameter n will be ignored."
    )
  )

  # check for conflicting filters
  assertthat::validate_that(
    all(is.null(start), is.null(end)) | is.null(n),
    msg = paste(
      "Last 5 and last 10 filters are incompatible with date filters.",
      "Only date filters will be used, parameter n will be ignored."
    )
  )

  # select collars
  purrr::when(
    collars,
    # default to all
    is.null(.) ~ fetch_ats_devices(),
    ~ collars
  ) %>%
    select_collars()

  # set defaults
  type <- "004"    # all data
  p1 <- ""         # no start date
  p2 <- ""         # no end date

  if (!any(is.null(start), is.null(end))) {
    type <- "001"    # filter by date range
    if (!is.null(start)) {
      p1 <- format(start, "%m/%d/%Y")
    }
    if (!is.null(start)) {
      p2 <- format(end, "%m/%d/%Y")
    }
  } else {
    if (!is.null(n)) {
      type <- purrr::when(
        n,
        . == 5 ~ "002",     # last 5
        . == 10 ~ "003",    # last 10
        ~ "004"
      )
    }
  }

  ats_post(
    path = "Servidor.ashx",
    body = list(
      consulta = "download_txt_collars",
      type = type,
      parameter1 = p1,
      parameter2 = p2
    ),
    task = "download position data"
  ) %>%
    ats_parse_xml()

}

# * 1.7 - ats_post --------------------------------------------------------

#' @title ATS POST
#'
#' @description Submit an http POST request to the ATS website
#'
#' @param path Character or list for the request path
#' @param body Named list of query parameters
#' @param task Character describing the purpose of the current request.
#'   If the request fails the message 'Failed to [task]' is displayed.
#'
#' @return Response object
#'
#' @seealso \code{\link{httr::POST}}
#'
#' @export
#'
#' @keywords internal
#'
#' @examples
#'
#' ats_post(
#'   path = "Servidor.ashx",
#'   body = list(
#'     consulta = "get_collars_user",
#'     valor = valor
#'   ),
#'   task = "download device list"
#' ) %>%
#'   ats_parse_xml()
#'
ats_post <- function(path, body = list(), task = "download data") {

  # check internet
  assertthat::assert_that(
    curl::has_internet,
    msg = "No internet connection available."
  )

  # check login
  assertthat::assert_that(
    check_cookie(ats_base_url, "user"),
    msg = "You need to log in first."
  )

  assertthat::assert_that(
    !missing(path),
    assert_that::not_empty(path),
    inherits(path, "list") | inherits(path, "character"),
    msg = "Incorrect path parameter supplied to POST request."
  )

  assertthat::assert_that(
    !missing(body),
    assert_that::not_empty(body),
    inherits(body, "list"),
    length(names(body)) == length(body),
    msg = "Incorrect body parameter supplied to POST request."
  )

  httr::RETRY(
    "POST",
    url = ats_base_url,
    path = path,
    body = body,
    encode = "form",
    quiet = TRUE
  ) %>%
    httr::stop_for_status(task)

}

# * 1.8 - count_sep -------------------------------------------------------

# function to count the number of commas in a string
count_sep <- function(x, sep = ",") {
  lengths(regmatches(x, gregexpr(sep, x)))
}

# * 1.9 - post_text -------------------------------------------------------

# function to return characters from nth to last separator to end of line
post_text <- function(x, n, sep = ",") {

  cs <- count_sep(x, sep)

  c_idx <- purrr::map2_int(
    gregexpr(sep, x),
    cs - n + 1,
    ~.x[.y]
  )

  ll <- nchar(x)

  c_idx[c_idx == ll] <- NA

  out <- x

  if (any(!is.na(c_idx))) {
    out[!is.na(c_idx)] <- substring(out, c_idx[!is.na(c_idx)] + 1, max(ll))
  }

  return(out)

}

# * 1.10 - pre_text -------------------------------------------------------

# function to return characters up to and including the nth separator
pre_text <- function(x, n, sep = ",") {

  c_idx <- sapply(
    gregexpr(sep, x),
    `[`,
    n
  )

  out <- x

  if (any(!is.na(c_idx))) {
    out[!is.na(c_idx)] <- substring(out, 1, c_idx[!is.na(c_idx)])
  }

  return(out)

}

# * 1.11 - repair_lines ---------------------------------------------------

# function to pull out lh columns and rh columns
#   and quote whatever is in between
repair_lines <- function(x, n_col, repair_col, sep = ",") {

  pre <- pre_text(x, repair_col - 1, sep)
  post <- post_text(x, n_col - repair_col, sep)
  repair <- substring(x, nchar(pre) + 1, nchar(x) - nchar(post) - 1)
  repair <- paste0("\"", repair, "\"", sep)

  return(paste0(pre, repair, post))

}

# * 1.12 - select_collars -------------------------------------------------

select_collars <- function(device_id) {

  # check collars parameter
  assertthat::assert_that(
    !missing(device_id),
    assertthat::not_empty(device_id),
    msg = "No collars selected."
  )

  cl <- paste0("000000", as.character(device_id), "_") %>%
    substr(nchar(.) - 6, nchar(.)) %>%
    paste0(collapse = "")

  ats_post(
    path = list("Servidor.ashx"),
    body = list(
      consulta = "cargarcokies",
      cadenacheckbox = cl
    ),
    task = "select collars"
  )

  return(TRUE)

}

# 2 - Download Functions --------------------------------------------------

# * 2.1 - fetch_ats_config ------------------------------------------------

#' @title Download Collar Configuration Data from ATS Website
#'
#' @description Retrieves configurtion information for all collars in the
#'   current account
#'
#' @return A tibble with configuration information
#'
#' @seealso \code{\link{ats_login}} for logging into an ATS account,
#'   \code{\link{fetch_ats_positions}} for downloading GPS data,
#'   \code{\link{fetch_ats_transmissions}} for downloading transmission
#'   data, \code{\link{fetch_ats_events}} for downloading alerts, and
#'   \code{link{fetch_ats_devices}} for downloading a list of available
#'   collars
#'
#' @export
#'
#' @examples
#'
#' ats_login("mary", ".")
#'
#' # get configuration details for all collars in this account
#' collar_details <- fetch_ats_config()
#'
#' ats_logout()
#'
fetch_ats_config <- function() {

  # download configurations
  ats_get(
    path = list(
      "download_collar_config",
      "Download_Collar_Config.aspx"
    ),
    task = "download device configurations"
  ) %>%
    ats_parse_txt()

}

# * 2.2 - fetch_ats_devices -----------------------------------------------

#' @title Download a List of Devices from ATS Website
#'
#' @description Retrieves a list of devices (collars), optionally
#'   filtered by status
#'
#' @param filter A single character value for filtering the results by
#'   status. If an invalid filter value is provided \code{fetch_ats_devices}
#'   returns a list of all devices with a warning. Valid filter values
#'   include:
#'     \itemize{
#'       \item{All}{Default - a list of all collars}
#'       \item{Active}{Only active collars}
#'       \item{Inactive}{Only inactive collars}
#'       \item{Low_batt}{Active collars with low battery alerts}
#'       \item{Mort}{Active collars with mortality alerts}
#'       \item{Birth}{Active collars with birth event alerts}
#'     }
#'
#' @return A character vector of device ids
#'
#' @seealso \code{\link{ats_login}} for logging into an ATS account,
#'   \code{\link{fetch_ats_config}} for downloading collar configurations,
#'   \code{\link{fetch_ats_positions}} for downloading GPS data,
#'   \code{\link{fetch_ats_transmissions}} for downloading transmission
#'   data, and \code{\link{fetch_ats_events}} for downloading alerts
#'
#' @export
#'
#' @examples
#'
#' ats_login("mary", ".")
#'
#' # get ids for all collars in this account
#' collar_list <- fetch_ats_devices()
#'
#' # get ids for collars active collars
#' collar_list <- fetch_ats_devices("Active")
#'
#' # get ids for collars inactive collars
#' collar_list <- fetch_ats_devices("Inactive")
#'
#' # get ids for collars with low battery
#' collar_list <- fetch_ats_devices("Low_batt")
#'
#' # get ids for collars in mortality
#' collar_list <- fetch_ats_devices("Mort")
#'
#' # get ids for collars with birth events triggered
#' collar_list <- fetch_ats_devices("Birth")
#'
#' ats_logout()
#'
fetch_ats_devices <- function(filter = "all") {

  # get filter parameter
  valor <- purrr::when(
    tolower(filter),
    . == "inactive" ~ "no_active",
    . == "low_batt" ~ "active_low_batt",
    . == "mort" ~ "active_mortality",
    . == "birth" ~ "active_birth_triggers",
    ~ "active"
  )

  resp <- ats_post(
    path = "Servidor.ashx",
    body = list(
      consulta = "get_collars_user",
      valor = valor
    ),
    task = "download device list"
  )

  devs <- httr::content(resp) %>%
    xml2::xml_find_all("//collar") %>%
    xml2::xml_text()

  if (all(valor == "active", valor != filter)) {
    if (tolower(filter) != "all") {
      warning(
        "Unrecognized filter provided, returning all collars."
      )
    }
    devs <- c(devs, fetch_ats_devices("inactive"))
  }

  devs

}

# * 2.3 - fetch_ats_events ------------------------------------------------

#' @title Download Event Data from ATS Website
#'
#' @description Retrieves all undownloaded events (a.k.a. alerts)
#'
#' @return A tibble with event information
#'
#' @seealso \code{\link{ats_login}} for logging into an ATS account,
#'   \code{\link{fetch_ats_config}} for downloading collar configurations,
#'   \code{\link{fetch_ats_positions}} for downloading GPS data,
#'   \code{\link{fetch_ats_transmissions}} for downloading transmission
#'   data, and \code{link{fetch_ats_devices}} for downloading a list of
#'   available collars
#'
#' @export
#'
#' @examples
#'
#' ats_login("mary", ".")
#'
#' # get undownloaded events for all collars in this account
#' alerts <- fetch_ats_events()
#'
#' ats_logout()
#'
fetch_ats_events <- function() {

  # download
  ats_get(
    path = list(
      "download_all_events",
      "download_all_events.aspx?dw=all"
    ),
    task = "download event data"
  ) %>%
    ats_parse_txt()

}

# * 2.4 - fetch_ats_positions ---------------------------------------------

#' @title Download GPS Fixes from ATS Website
#'
#' @description Retrieves GPS data optionally filtered by date, last n
#'   positions, or collar
#'
#' @section Notes:
#'
#'   Currently filtering by date is broken on the ATS site - using the
#'   date range filters when downloading data returns an internal server
#'   error. The functionality is included here in the hopes that it will
#'   be fixed in the near future, but currently date filters are ignored.
#'
#' @param device_id A single device id, or a list or vector of device ids,
#'   or NULL for all devices associated with current account. Overrides
#'   the new parameter when specified.
#' @param start_date,end_date Currently ignored (see \link{Notes}).
#' @param n A single integer specifying how many fixes to return per
#'   collar (sorted by recency). Valid values are 5 and 10.
#' @param new A logical flag. When new = true only data that hasn't been
#'   previously downloaded is returned. If device_id is specified the flag
#'   is ignored.
#'
#' @return A tibble with information about mortalities, births, etc.
#'
#' @seealso \code{\link{ats_login}} for logging into an ATS account,
#'   \code{\link{fetch_ats_config}} for downloading collar configurations,
#'   \code{\link{fetch_ats_transmissions}} for downloading transmission
#'   data, \code{\link{fetch_ats_events}} for downloading alerts, and
#'   \code{link{fetch_ats_devices}} for downloading a list of available
#'   collars
#'
#' @export
#'
#' @examples
#'
#' ats_login("mary", ".")
#'
#' # get undownloaded fixes for all collars in this account
#' fixes <- fetch_ats_positions(new = TRUE)
#'
#' # get all fixes for all collars in this account
#' fixes <- fetch_ats_positions()
#'
#' # get all fixes for specific collars
#' collar_list <- sample(fetch_ats_devices(), 10)
#' fixes <- fetch_ats_positions(collar_list)
#'
#' # get all fixes for collars in mortality
#' collar_list <- fetch_ats_devices("mortality")
#' fixes <- fetch_ats_positions(device_id = collar_list)
#'
#' # get last 10 fixes for certain collars
#' fixes <- fetch_ats_positions(device_id = collar_list, n = 10)
#'
#' # get fixes in 2019 for all collars
#' fixes <- fetch_ats_positions(
#'   start = as.POSIXct("2019-01-01"),
#'   end = as.POSIXct("2020-01-01")
#' )
#'
#' # get fixes in 2019 for certain collars
#' fixes <- fetch_ats_positions(
#'   device_id =  = collar_list,
#'   start = as.POSIXct("2019-01-01"),
#'   end = as.POSIXct("2020-01-01")
#' )
#'
#' ats_logout()
#'
fetch_ats_positions <- function(device_id = NULL,
                                start = NULL,
                                end = NULL,
                                n = NULL,
                                new = FALSE) {

  # check for dates - error on ATS website
  # TODO remove when ATS site is fixed
  if (!(is.null(start) & is.null(end))) {
    warning(paste(
      "Filtering by date is currently unavailable due to an error",
      "on the ATS website. Start date and end date parameters",
      "will be ignored."
    ))
    start <- NULL
    end <- NULL
  }

  # get filters
  args <- as.list(environment())
  args <- args[names(args) != "new"]

  if (all(sapply(args, is.null))) {
    ats_pos_all(new)
  } else {
    do.call(ats_pos_filter, args)
  }

}

# * 2.5 - fetch_ats_transmissions -----------------------------------------

#' @title Download Transmission Data from ATS Website
#'
#' @description Retrieves all transmissions or undownloaded transmissions,
#'   optionally filtered by collar
#'
#' @param device_id A single device id, or a list or vector of device ids,
#'   or NULL for all devices associated with current account. Overrides
#'   the new parameter when specified.
#' @param new A logical flag. When new = true only data that hasn't been
#'   previously downloaded is returned. If device_id is specified the flag
#'   is ignored.
#'
#' @return A tibble with transmission information
#'
#' @seealso \code{\link{ats_login}} for logging into an ATS account,
#'   \code{\link{fetch_ats_config}} for downloading collar configurations,
#'   \code{\link{fetch_ats_positions}} for downloading GPS data,
#'   \code{\link{fetch_ats_events}} for downloading alerts, and
#'   \code{link{fetch_ats_devices}} for downloading a list of available
#'   collars
#'
#' @export
#'
#' @examples
#'
#' ats_login("mary", ".")
#'
#' # get undownloaded transmissions for all collars in this account
#' trans <- fetch_ats_transmissions(new = TRUE)
#'
#' # get all transmissions for all collars in this account
#' trans <- fetch_ats_transmissions()
#'
#' # get all transmissions for specific collars
#' collar_list <- sample(fetch_ats_devices(), 10)
#' trans <- fetch_ats_transmissions(device_id = collar_list)
#'
#' ats_logout()
#'
fetch_ats_transmissions <- function(device_id = NULL, new = FALSE) {

  if (any(is.null(device_id), length(device_id) == 0)) {

    type <- purrr::when(
      new,
      isTRUE(.) ~ "new",
      ~ "all"
    )

    ats_get(
      path = list(
        "download_all_transmission",
        paste0("download_all_transmission.aspx?dw=", type)
      ),
      task = "download transmission data"
    ) %>%
      ats_parse_trans()

  } else {

    select_collars(device_id)

    ats_post(
      path = "Servidor.ashx",
      body = list(
        consulta = "download_trans_collars"
      ),
      task = "download transmission data"
    ) %>%
      ats_parse_xml()
  }

}

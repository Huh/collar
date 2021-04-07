# 1 - Global Objects ------------------------------------------------------

# * 1.1 - ats_empty_pos ---------------------------------------------------

# tibble with zero rows matching ATS position file format
# used to enforce consistency between 4 different possible response formats
#   (get with data, get without data, post with data, post without data)

ats_empty_pos <- tibble::tibble(
  CollarSerialNumber = character(0),
  Year = integer(0),
  JulianDay = integer(0),
  Hour = integer(0),
  Minute = integer(0),
  Activity = integer(0),
  Temperature = integer(0),
  Latitude = numeric(0),
  Longitude = numeric(0),
  HDOP = numeric(0),
  NumSats = integer(0),
  FixTime = integer(0),
  `2D/3D` = integer(0),
  DateOffset = as.POSIXct(NA),
  GmtOffset = numeric(0),
  DateUTC = as.POSIXct(NA),
  DateLocal = as.POSIXct(NA),
  VITTemp = integer(0),
  VITLight = integer(0),
  VITComm = integer(0),
  Fawn0Comm = integer(0),
  Fawn1Comm = integer(0),
  Fawn2Comm = numeric(0),
  TransDateUTC = as.POSIXct(NA),
  TransDateLocal = as.POSIXct(NA)
)

# * 1.2 - ats_empty_trans -------------------------------------------------

# tibble with zero rows matching ATS transmission file format
# used to enforce consistency between 4 different possible response formats
#   (get with data, get without data, post with data, post without data)

ats_empty_trans <- tibble::tibble(
  CollarSerialNumber = character(0),
  DateCT = as.POSIXct(NA),
  DateUTC = as.POSIXct(NA),
  DateLocal = as.POSIXct(NA),
  NumberFixes = integer(0),
  BattVoltage = numeric(0),
  Mortality = character(0),
  BreakOff = character(0),
  GpsOnTime = integer(0),
  SatOnTime = integer(0),
  SatErrors = integer(0),
  GmtOffset = numeric(0),
  LowBatt = logical(0),
  Birth = character(0),
  Fawn0 = character(0),
  Fawn1 = character(0),
  Fawn2 = character(0),
  Latitude = numeric(0),
  Longitude = numeric(0),
  CEPradius_km = integer(0)
)

# 2 - Internal Functions --------------------------------------------------

# * 2.1 - ats_get ---------------------------------------------------------

#' @title GET
#'
#' @description Submit an http GET request to the ATS website
#'
#' @param path Character or list for the request path.
#' @param task Character describing the purpose of the current request.
#'   If the request fails the message 'Failed to [task]' is displayed.
#'
#' @return HTTP response object
#'
#' @seealso \code{\link{httr::GET}}
#'
#' @export
#'
#' @keywords internal
#'
#' @examples
#'
#' ats_login("mary", ".")
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
    curl::has_internet(),
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
    assertthat::not_empty(path),
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

# * 2.2 - ats_join_trans --------------------------------------------------

#' @title Join Fixes and Transmissions
#'
#' @description Add transmission info to fixes to apply offset and infer
#'   data substitutions
#'
#' @param pos Tibble of position data.
#' @param trans Tibble of corresponding transmission data.
#'
#' @return  A tibble with 25 columns (see \code{\link{ats_fetch_positions}})
#'
#' @seealso \code{\link{ats_fetch_positions}}
#'
#' @export
#'
#' @keywords internal
#'
#' @examples
#'
#' ats_login("mary", ".")
#'
#' trans <- fetch_ats_transmissions()
#'
#' ats_get(
#'   path = list(
#'     "download_all_data",
#'     paste0("Download_all_data.aspx?dw=all")
#'   ),
#'   task = "download position data"
#' ) %>%
#'   httr::content("text", encoding = "UTF-8") %>%
#'   readr::read_csv(col_types = "ciiiiiidddiic_") %>%
#'   dplyr::rename(JulianDay = Julianday) %>%
#'   ats_join_trans(trans)
#'
ats_join_trans <- function(pos, trans) {

  tr_w_fixnumber <- trans %>%
    dplyr::group_by(CollarSerialNumber) %>%
    dplyr::mutate(FixNumber = cumsum(NumberFixes)) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      CollarSerialNumber, FixNumber, GmtOffset, TransDateUTC = DateUTC,
      TransDateLocal = DateLocal, Birth, Fawn0:Fawn2
    )

  pos %>%
    dplyr::group_by(CollarSerialNumber) %>%
    dplyr::mutate(FixNumber = dplyr::row_number()) %>%
    dplyr::left_join(
      tr_w_fixnumber,
      by = c("CollarSerialNumber", "FixNumber")
    ) %>%
    tidyr::fill(
      GmtOffset, TransDateUTC, TransDateLocal, Birth, Fawn0:Fawn2,
      .direction = "up"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      DateOffset = lubridate::as_datetime(
        paste(Year, JulianDay, Hour, Minute),
        format = "%y %j %H %M"
      ),
      DateUTC = DateOffset + lubridate::hours(GmtOffset),
      DateLocal = lubridate::with_tz(DateUTC, tz = Sys.timezone()),
      DataMode = dplyr::case_when(
        nchar(`2D/3D`) == 1 ~ "Normal",
        Birth != "None" ~ "VIT",
        !all(Fawn0 == "None", Fawn1 == "None", Fawn2 == "None") ~ "Fawn",
        TRUE ~ "Unknown"
      ),
      VITTemp = dplyr::case_when(
        DataMode != "VIT" ~ NA_integer_,
        TRUE ~ Temperature
      ),
      Temperature = dplyr::case_when(
        DataMode != "Normal" ~ NA_integer_,
        TRUE ~ Temperature
      ),
      VITLight = dplyr::case_when(
        DataMode != "VIT" ~ NA_integer_,
        TRUE ~ FixTime
      ),
      FixTime = dplyr::case_when(
        DataMode %in% c("Fawn", "Normal") ~ FixTime,
        TRUE ~ NA_integer_
      ),
      Fawn2Comm = dplyr::case_when(
        DataMode != "Fawn" ~ NA_real_,
        TRUE ~ HDOP
      ),
      HDOP = dplyr::case_when(
        DataMode != "Normal" ~ NA_real_,
        TRUE ~ HDOP
      ),
      Fawn1Comm = dplyr::case_when(
        DataMode != "Fawn" ~ NA_integer_,
        TRUE ~ NumSats
      ),
      NumSats = dplyr::case_when(
        DataMode != "Normal" ~ NA_integer_,
        TRUE ~ NumSats
      ),
      Fawn0Comm = dplyr::case_when(
        DataMode != "Fawn" ~ NA_integer_,
        TRUE ~ as.integer(`2D/3D`)
      ),
      VITComm = dplyr::case_when(
        DataMode != "VIT" ~ NA_integer_,
        TRUE ~ as.integer(`2D/3D`)
      ),
      `2D/3D` = dplyr::case_when(
        DataMode != "Normal" ~ NA_integer_,
        TRUE ~ as.integer(`2D/3D`)
      )
    ) %>%
      dplyr::relocate(DateOffset, .before = GmtOffset) %>%
      dplyr::relocate(VITTemp, VITLight, VITComm, .after = DateLocal) %>%
      dplyr::relocate(Fawn0Comm, Fawn1Comm, Fawn2Comm, .after = VITComm)  %>%
      dplyr::relocate(TransDateUTC, TransDateLocal, .after = Fawn2Comm)  %>%
      dplyr::select(-c(FixNumber, Birth, Fawn0:Fawn2, DataMode))

}

# * 2.3 - ats_parse_pos ---------------------------------------------------

#' @title Parse Fixes
#'
#' @description Convert http response to tibble
#'
#' @section Notes:
#'
#'   Data can be retrieved either via \code{httr::GET} (for all
#'   transmissions) or \code{httr::POST} (by collar), and each request
#'   type can return two different formats depending on whether there is
#'   data available. This function handles the four different possibilities
#'   and returns a tibble with the same column names and types no matter
#'   what. GmtOffset is multiplied by -1 to correct for ATS using
#'   non-standard syntax. Dates in UTC and current system time zone are
#'   added. Column substitutions in neolink modes are reversed.
#'
#' @param resp HTTP response object
#' @param trans Tibble of corresponding transmission data
#'
#' @return A tibble with 25 columns (see \code{\link{ats_fetch_positions}})
#'
#' @seealso \code{\link{ats_fetch_positions}}
#'
#' @export
#'
#' @keywords internal
#'
#' @examples
#'
#' ats_login("mary", ".")
#'
#' trans <- fetch_ats_transmissions()
#'
#' ats_get(
#'   path = list(
#'     "download_all_data",
#'     paste0("Download_all_data.aspx?dw=all")
#'   ),
#'   task = "download position data"
#' ) %>%
#'   ats_parse_pos(trans)
#'
ats_parse_pos <- function(resp, trans) {

  assertthat::assert_that(
    inherits(resp, "response"),
    msg = "Invalid response passed to parsing function."
  )

  if (resp$request$method == "GET") {

    if (inherits(httr::content(resp), "xml_document")) {

      # api returns html when no new data is available
      ats_empty_pos

    } else {

      # parse txt file
      httr::content(resp, "text", encoding = "UTF-8") %>%
        readr::read_csv(col_types = "ciiiiiidddiic_") %>%
        dplyr::rename(JulianDay = Julianday) %>%
        ats_join_trans(trans)

    }

  } else {

    pos_data <- resp %>%
      ats_parse_xml()

    if (ncol(pos_data) == 0) {

      # no data returned (tibble 0x0)
      ats_empty_pos

    } else {

      # parse xml file
      pos_data %>%
        dplyr::select(
          CollarSerialNumber = AtsSerialNum, Year, JulianDay, Hour, Minute,
          Activity, Temperature, Latitude, Longitude, HDOP = Hdop, NumSats,
          FixTime, `2D/3D` = Dimension
        ) %>%
        dplyr::mutate(
          dplyr::across(c(2:7, 11:12), as.integer),
          dplyr::across(c(8:9, 10), as.numeric)
        ) %>%
        ats_join_trans(trans)

    }

  }

}

# * 2.4 - ats_parse_trans -------------------------------------------------

#' @title Parse Transmissions
#'
#' @description Convert http response to tibble
#'
#' @section Notes:
#'
#'   Data can be retrieved either via \code{httr::GET} (for all
#'   transmissions) or \code{httr::POST} (by collar), and each request
#'   type can return two different formats depending on whether there is
#'   data available. This function handles the four different possibilities
#'   and returns a tibble with the same column names and types no matter
#'   what. GmtOffset is multiplied by -1 to correct for ATS using
#'   non-standard syntax. Dates in UTC and current system time zone are
#'   added. Neolink events are split into separate columns.
#'
#' @param resp HTTP response object
#'
#' @return A tibble with 19 columns (see \code{\link{ats_fetch_positions}})
#'
#' @seealso \code{\link{ats_fetch_transmissions}}
#'
#' @export
#'
#' @keywords internal
#'
#' @examples
#'
#' ats_login("mary", ".")
#'
#' trans <- fetch_ats_transmissions()
#'
#' ats_get(
#'   path = list(
#'     "download_all_transmission",
#'     "download_all_transmission.aspx?dw=all"
#'   ),
#'   task = "download transmission data"
#' ) %>%
#'   ats_parse_trans()
#'
ats_parse_trans <- function(resp) {

  assertthat::assert_that(
    inherits(resp, "response"),
    msg = "Invalid response passed to parsing function."
  )

  if (resp$request$method == "GET") {

    if (inherits(httr::content(resp), "xml_document")) {

      # api returns html when no new data is available
      ats_empty_trans

    } else {

      # txt file - quote unquoted separators, reformat events, add dates
      httr::content(resp, "text", encoding = "UTF-8") %>%
        textConnection() %>%
        readLines() %>%
        {sub("^(([^,]*,){11})(.+)((,[^,]*){3})$", "\\1\"\\3\"\\4", .)} %>%
        readr::read_csv(col_types = "ccidcciiidccddi") %>%
        tidyr::separate(
          Event,
          into = c("Fawn0", "Fawn1", "Fawn2"),
          sep = ",",
          fill = "right"
        ) %>%
        dplyr::mutate(
          DateCT = as.POSIXct(
            DateCT,
            tz = "America/Menominee",
            format = "%m/%d/%Y %I:%M:%S %p"
          ),
          GmtOffset = GmtOffset * -1,
          LowBatt = dplyr::if_else(LowBatt == "Yes", TRUE, FALSE),
          Birth = dplyr::if_else(grepl("^Birth", Fawn0), Fawn0, "None"),
          Fawn0 = dplyr::if_else(grepl("^Birth", Fawn0), "None", Fawn0),
          Fawn1 = tidyr::replace_na(Fawn1, "None"),
          Fawn2 = tidyr::replace_na(Fawn2, "None")
        ) %>%
        dplyr::relocate(Birth, .before = Fawn0) %>%
        ats_trans_dates()

    }

  } else {

    tr_data <- resp %>%
      ats_parse_xml()

    if (ncol(tr_data) == 0) {

      # no data returned (tibble 0x0)
      ats_empty_trans

    } else {

      # reformat events, rename columns, add dates
      tr_data %>%
        tidyr::separate(
          ev,
          into = c("ev0", "ev1", "ev2"),
          sep = ",",
          fill = "right"
        ) %>%
        tidyr::separate(
          evc,
          into = c("evc0", "evc1", "evc2"),
          sep = ",",
          fill = "right"
        ) %>%
        dplyr::transmute(
          CollarSerialNumber = collar,
          DateCT = as.POSIXct(
            fecha,
            tz = "America/Menominee"
          ),
          NumberFixes = as.integer(number),
          BattVoltage = as.numeric(battvol),
          Mortality = morty,
          BreakOff = breakoff,
          GpsOnTime = as.integer(gpson),
          SatOnTime = as.integer(saton),
          SatErrors = as.integer(saterror),
          GmtOffset = as.numeric(gmt) * -1,
          LowBatt = as.logical(lowbatt),
          Birth = dplyr::if_else(
            grepl("^Birth", ev0),
            paste(ev0, evc0, sep = "-"),
            "None"
          ),
          Fawn0 = dplyr::if_else(
            grepl("^Birth", ev0) | ev0 == "None",
            "None",
            paste(ev0, evc0, sep = "-")
          ),
          Fawn1 = dplyr::if_else(
            is.na(ev1) | ev1 == "None",
            "None",
            paste(ev1, evc1, sep = "-")
          ),
          Fawn2 = dplyr::if_else(
            is.na(ev2) | ev2 == "None",
            "None",
            paste(ev2, evc2, sep = "-")
          ),
          Latitude = as.numeric(lat),
          Longitude = as.numeric(long),
          CEPradius_km = as.integer(cpe)
        ) %>%
        ats_trans_dates()

    }

  }

}

# * 2.5 - ats_parse_txt ---------------------------------------------------

#' @title Parse Text
#'
#' @description Convert raw text file in HTTP response to tibble
#'
#' @param resp HTTP response object
#' @param ... Additional arguments passed to \code{readr::read_csv}
#'
#' @return A tibble
#'
#' @export
#'
#' @keywords internal
#'
#' @examples
#'
#' ats_login("mary", ".")
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
ats_parse_txt <- function(resp, ...) {

  assertthat::assert_that(
    inherits(resp, "response"),
    msg = "Invalid response passed to parsing function."
  )

  httr::content(resp, "text", encoding = "UTF-8") %>%
    readr::read_csv(...)

}

# * 2.6 - ats_parse_xml ---------------------------------------------------

#' @title Parse XML
#'
#' @description Convert raw xml in HTTP response to tibble
#'
#' @param resp HTTP response object
#'
#' @return A tibble
#'
#' @export
#'
#' @keywords internal
#'
#' @examples
#'
#' ats_login("mary", ".")
#'
#' ats_post(
#'   path = "Servidor.ashx",
#'   body = list(
#'     consulta = "download_txt_collars",
#'     type = "004"",
#'     parameter1 = "",
#'     parameter2 = ""
#'   ),
#'   task = "download position data"
#' ) %>%
#'   ats_parse_xml()
#'
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

# * 2.7 - ats_post --------------------------------------------------------

#' @title POST
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
#' ats_login("mary", ".")
#'
#' ats_select_collars("044286")
#'
#' ats_post(
#'   path = "Servidor.ashx",
#'   body = list(
#'     consulta = "download_trans_collars"
#'   ),
#'   task = "download transmission data"
#' ) %>%
#'   ats_parse_xml()
#'
ats_post <- function(path, body = list(), task = "download data") {

  # check internet
  assertthat::assert_that(
    curl::has_internet(),
    msg = "No internet connection available."
  )

  # check login
  assertthat::assert_that(
    check_cookie(ats_base_url, "user"),
    msg = "You need to log in first."
  )

  assertthat::assert_that(
    !missing(path),
    assertthat::not_empty(path),
    inherits(path, "list") | inherits(path, "character"),
    msg = "Incorrect path parameter supplied to POST request."
  )

  assertthat::assert_that(
    !missing(body),
    assertthat::not_empty(body),
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

# * 2.8 - ats_select_collars ----------------------------------------------

#' @title Select Collars
#'
#' @description Submit an http POST request to create a cookie of selected
#'   collars for subsequent requests
#'
#' @param device_id A single device id, or a list or vector of device ids.
#'
#' @return Logical, TRUE if successful
#'
#' @export
#'
#' @keywords internal
#'
#' @examples
#'
#' ats_login("mary", ".")
#'
#' all_collars <- fetch_ats_devices()
#' some_collars <- sample(all_collars, 5)
#' ats_select_collars(some_collars)
#'
ats_select_collars <- function(device_id) {

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

# * 2.9 - ats_trans_dates -------------------------------------------------

#' @title Transmission Dates
#'
#' @description Add date columns to transmission data
#'
#' @param trans A tibble of transmission data
#'
#' @return A tibble with dates in UTC and local (system) time added
#'
#' @export
#'
#' @keywords internal
#'
#' @examples
#'
#' ats_login("mary", ".")
#'
#' ats_select_collars("044286")
#'
#' ats_post(
#'   path = "Servidor.ashx",
#'   body = list(
#'     consulta = "download_trans_collars"
#'   ),
#'   task = "download transmission data"
#' ) %>%
#'   ats_parse_xml() %>%
#'   dplyr::mutate(
#'     DateCT = as.POSIXct(
#'       fecha,
#'       tz = "America/Menominee"
#'     ),
#'     GmtOffset = as.numeric(gmt) * -1
#'   ) %>%
#'   ats_trans_dates()
#'
ats_trans_dates <- function(trans) {

  trans %>%
    dplyr::mutate(
      DateUTC = lubridate::with_tz(DateCT, tz = "UTC"),
      DateLocal = lubridate::with_tz(DateCT, tz = Sys.timezone())
    ) %>%
    dplyr::relocate(DateUTC, DateLocal, .after = DateCT)

}

# 3 - Download Functions --------------------------------------------------

# * 3.1 - fetch_ats_config ------------------------------------------------

#' @title Download Collar Configuration Data from ATS Website
#'
#' @description Retrieves configuration information for all collars in the
#'   current account
#'
#' @return A tibble with 6 columns:
#' \describe{
#'   \item{CollarSerialNumber}{ATS Collar ID (character)}
#'   \item{Email}{Email recipient for alerts (character)}
#'   \item{Active}{Is collar currently active (logical)}
#'   \item{Phone Num SMS}{SMS recipient for alerts (character)}
#'   \item{FTP Url}{URL for retrieving data via ftp (character)}
#'   \item{RestEndPoint}{Is data available via REST API (logical)
#' }
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
    ats_parse_txt(col_types = "cccccc") %>%
    dplyr::mutate(
      CollarSerialNumber = dplyr::if_else(
        grepl("^00", CollarSerialNumber) & nchar(CollarSerialNumber) == 7,
        substr(CollarSerialNumber, 2, 7),
        CollarSerialNumber
      ),
      Active = dplyr::if_else(Active == "yes", TRUE, FALSE),
      RestEndPoint = dplyr::if_else(RestEndPoint == "yes", TRUE, FALSE)
    )

}

# * 3.2 - fetch_ats_devices -----------------------------------------------

#' @title Download a List of Devices from ATS Website
#'
#' @description Retrieves a list of devices (collars), optionally
#'   filtered by status
#'
#' @param filter A single character value for filtering the results by
#'   status. If an invalid filter value is provided
#'   \code{fetch_ats_devices} returns a list of all devices with a warning.
#'   Valid filter values include:
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

  if (all(valor == "active", valor != tolower(filter))) {
    if (tolower(filter) != "all") {
      warning(
        "Unrecognized filter provided, returning all collars."
      )
    }
    devs <- c(devs, fetch_ats_devices("inactive"))
  }

  devs

}

# * 3.3 - fetch_ats_events ------------------------------------------------

#' @title Download Event Data from ATS Website
#'
#' @description Retrieves all undownloaded events (a.k.a. alerts)
#'
#' @Return A tibble with 8 columns:
#' \describe{
#'   \item{CollarSerialNumber}{ATS Collar ID (character)}
#'   \item{DateCT}{Timestamp from server (US Cental time, POSIXct))}
#'   \item{DateUTC}{Timestamp in UTC/GMT (POSIXct)}
#'   \item{DateLocal}{Timestamp in current system time zone (POSIXct)}
#'   \item{Birth}{VIT birth event (character)}
#'   \item{Fawn0}{Main collar event or neolink slot 0 event (character)}
#'   \item{Fawn1}{Neolink slot 1 event (character)}
#'   \item{Fawn2}{Neolink slot 2 event (character)}
#' }
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
    ats_parse_txt() %>%
    dplyr::rename(DateCT = DateSent) %>%
    dplyr::mutate(
      DateCT = as.POSIXct(
        DateCT,
        tz = "America/Menominee",
        format = "%m/%d/%Y %I:%M:%S %p"
      )
    ) %>%
    ats_trans_dates()

}

# * 3.4 - fetch_ats_positions ---------------------------------------------

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
#' @return A tibble with 25 columns:
#' \describe{
#'   \item{CollarSerialNumber}{ATS Collar ID (character)},
#'   \item{Year}{Two digit year (integer)},
#'   \item{JulianDay}{Julian day (1:366, integer)},
#'   \item{Hour}{Hour (integer)},
#'   \item{Minute}{Minute (integer)},
#'   \item{Activity}{Activity sensor reading (integer)},
#'   \item{Temperature}{Ambient temperature (Celsius, integer)},
#'   \item{Latitude}{Latitude (decimal degrees, numeric)},
#'   \item{Longitude}{Longitude (decimal degrees, numeric)},
#'   \item{HDOP}{Horizontal Dilution of Precision (numeric)},
#'   \item{NumSats}{Number of GPS satellites (integer)},
#'   \item{FixTime}{GPS Fix Time (integer)},
#'   \item{`2D/3D`}{GPS fix dimension (integer)},
#'   \item{DateOffset}{Timestamp from raw data with fixed offset applied
#'     (POSIXct)},
#'   \item{GmtOffset}{Offset from GMT in hours, mutliplied by negative one
#'     to correct for non-standard ATS usage (numeric)},
#'   \item{DateUTC}{Timestamp in UTC/GMT (POSIXct)},
#'   \item{DateLocal}{Timestamp in current system time zone (POSIXct)},
#'   \item{VITTemp}{VIT temperature (Celsius, integer)},
#'   \item{VITLight}{VIT light sensor reading (integer) (integer)},
#'   \item{VITComm}{VIT communication count (integer)},
#'   \item{Fawn0Comm}{Fawn slot 0 communication count (integer)},
#'   \item{Fawn1Comm}{Fawn slot 1 communication count (integer)},
#'   \item{Fawn2Comm}{Fawn slot 2 communication count (numeric)},
#'   \item{TransDateUTC}{Transmission timestamp in UTC/GMT (POSIXct)},
#'   \item{TransDateLocal}{Transmission timestamp in current system time
#'     zone (POSIXct)}
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
#'   device_id = collar_list,
#'   start = as.POSIXct("2019-01-01"),
#'   end = as.POSIXct("2020-01-01")
#' )
#'
#' # get undownloaded fixes for a single collar
#' fixes <- fetch_ats_positions("044286", new = TRUE)
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

    # get transmission data for determining gmt offset
    trans <- fetch_ats_transmissions(new = new)

    # get dw parameter for GET request
    type <- purrr::when(
      new,
      isTRUE(.) ~ "new",
      ~ "all"
    )

    # send request and parse
    ats_get(
      path = list(
        "download_all_data",
        paste0("Download_all_data.aspx?dw=", type)
      ),
      task = "download position data"
    ) %>%
      ats_parse_pos(trans)

  } else {

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

    # get transmission data for determining gmt offset
    trans <- fetch_ats_transmissions(device_id)

    # set defaults
    type <- "004"    # all data
    p1 <- ""         # no start date
    p2 <- ""         # no end date

    # adjust request parameters
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

    # send request and parse
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
      ats_parse_pos(trans)

  }

}

# * 3.5 - fetch_ats_transmissions -----------------------------------------

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
#' @return A tibble with 20 columns:
#' \describe{
#'   \item{CollarSerialNumber}{ATS Collar ID (character)}
#'   \item{DateCT}{Timestamp from server (US Cental time, POSIXct))}
#'   \item{DateUTC}{Timestamp in UTC/GMT (POSIXct)}
#'   \item{DateLocal}{Timestamp in current system time zone (POSIXct)}
#'   \item{NumberFixes}{Number of fixes transmitted (integer)}
#'   \item{BattVoltage}{Battery voltage (numeric)}
#'   \item{Mortality}{Mortality message (character)}
#'   \item{BreakOff}{Breakoff message (character)}
#'   \item{GpsOnTime}{GPS module on time (integer)}
#'   \item{SatOnTime}{Satellite module on time (integer)}
#'   \item{SatErrors}{Number of satellite errors (integer)}
#'   \item{GmtOffset}{Offset (in hours) from UTC/GMT (numeric)}
#'   \item{LowBatt}{Is collar in low battery mode (logical)}
#'   \item{Birth}{VIT birth event (character)}
#'   \item{Fawn0}{Neolink slot 0 event (character)}
#'   \item{Fawn1}{Neolink slot 1 event (character)}
#'   \item{Fawn2}{Neolink slot 2 event (character)}
#'   \item{Latitude}{Latitude in decimal degrees (numeric)}
#'   \item{Longitude}{Latitude in decimal degrees (numeric)}
#'   \item{CEPradius_km}{Circular Error Probability in km (integer)}
#' }
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

    ats_select_collars(device_id)

    ats_post(
      path = "Servidor.ashx",
      body = list(
        consulta = "download_trans_collars"
      ),
      task = "download transmission data"
    ) %>%
      ats_parse_trans()

  }

}

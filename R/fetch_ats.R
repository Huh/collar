# 1 - Download Functions --------------------------------------------------

# * 1.x - fetch_ats_positions ---------------------------------------------

#' @title Download GPS Fixes from ATS Website
#'
#' @description Retrieves GPS data optionally filtered by date, last n,
#'   or collar.
#'
#' @section Notes:
#'
#'   Currently filtering by date is broken on the ATS side - using the
#'   date range filters when downloading data returns an internal server
#'   error. The functionality is included here in the hopes that it will
#'   be fixed in the near future, but currently date filters are ignored.
#'
#' @return A tibble with information about mortalities, births, etc.
#'
#' @seealso \code{\link{ats_login}} for logging into an ATS account,
#'   \code{\link{fetch_ats_transmissions}} for downloading transmission
#'   data, and \code{\link{fetch_ats_events}} for downloading alerts.
#'
#' @export
#'
#' @examples
#'
#' ats_login("demo", "PASSWORD09")
#'
#' # get all fixes for all collars in this account
#' fixes <- fetch_ats_positions()
#' 
#' # get all fixes for certain collars
#' 
#' # get all fixes for all collars in this account
#' 
#' # get all fixes for all collars in this account
#' ats_logout()
#'
fetch_ats_positions <- function(collars = NULL, start = NULL, end = NULL, n = NULL) {

  # get filters
  args <- as.list(environment())

  if (all(unlist(lapply(args, is.null)))) {
    ats_all_pos()
  } else {
    do.call(ats_pos, args)
  }

}

# TODO rename this
ats_pos <- function(collars = NULL, start = NULL, end = NULL, n = NULL) {

  # get login info
  # function will exit here if login info is invalid
  if (is.null(collars)) {
    cookies <- ats_auth_cookies()
  } else {
    cookies <- ats_collar_cookies(collars)
  }

  type <- "004"
  p1 <- ""
  p2 <- ""

  # TODO warn for argument collisions (s/e >> n), require collars arg
  if (!is.null(start) | !is.null(end)) {
    type <- "001"
    if (!is.null(start)) {
      p1 <- format(start, "%m/%d/%Y")
    }
    if (!is.null(start)) {
      p2 <- format(end, "%m/%d/%Y")
    }
  } else {
    if (!is.null(n)) {
      if (n == 5) {
        type <- "002"
      }
      if (n == 10) {
        type <- "003"
      }
    }
  }

  resp <- httr::POST(
    url = ats_base_url,
    path = "Servidor.ashx",
    body = list(
      consulta = "download_txt_collars",
      type = type,
      parameter1 = p1,
      parameter2 = p2
    ),
    encode = "form",
    httr::set_cookies(cookies)
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

ats_all_pos <- function() {

  resp <- httr::GET(
    url = ats_base_url,
    path = list("download_all_data", "Download_all_data.aspx?dw=all"),
    httr::set_cookies(ats_auth_cookies())
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

  resp$content %>% 
    readBin(what = "character", n = length(loc$content) / 4) %>% 
    textConnection() %>% 
    read.csv()

}

fetch_ats_devices <- function(filter = NULL) {

  valor <- purrr::when(
    tolower(filter),
    . == "inactive" ~ "no_active",
    grepl("low", .) ~ "active_low_batt",
    grepl("mort", .) ~ "active_mortality",
    grepl("birth", .) ~ "active_birth_triggers",
    ~ "active"
  )

  resp <- httr::POST(
    url = ats_base_url,
    path = "Servidor.ashx",
    body = list(
      consulta = "get_collars_user",
      valor = valor
    ),
    encode = "form",
    httr::set_cookies(ats_collar_cookies(collar_list))
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
  
  devs <- httr::content(resp) %>%
    xml2::xml_find_all("//collar") %>%
    xml2::xml_text()

  if (any(is.null(filter), tolower(filter) == "all")) {
    devs <- c(devs, fetch_ats_devices("inactive"))
  }

  devs

}

# TODO use retry in all the new stuff
# TODO write tests
# TODO try with different credentials
# TODO disable date filtering
# TOD add functions for events and transmissions
# TODO add assert status function or switch to httr::stop_for_status

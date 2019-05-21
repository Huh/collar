#' Scrape data from ATS website
#'
#' @param bttn_nm The name of the button to download data from
#' @param usr The username associated with the account
#' @param pwd The password used with the supplied username
#' @param base_url The URL where the function attempts to gain access. By default this is set to "atsidaq.net" which is the domain used for recent GPS collars. Use of other domains (e.g. "atsdat.net") is supported by specifying a URL value in order to support older Iridium based collars and for future compatibility.
#'
#' @details The url where the function attempts to gain access is atsidaq.net.
#'
#' To download from a new button name use the developer tools in your browser and record the name of the button as described there.  For example, the download all button has CSS name 'ctl00$ContentPlaceHolder1$DownloadAll3'.
#' @return A tibble with automatic parsing of column types
#' @export
#'
#' @examples
#' \dontrun{
#'   fetch_ats(usr = "my_username", pwd = "secret_code")
#' }
fetch_ats <- function(bttn_nm = NULL,
                      usr = NULL,
                      pwd = NULL,
                      base_url = "atsidaq.net") {
  if (is.null(bttn_nm)) {
    warning("Button name NULL, downloading all data")
  }
  stopifnot(!is.null(usr))
  stopifnot(!is.null(pwd))

  #  Connect to webpage
  pgsession <- rvest::html_session(base_url)
  httr::stop_for_status(pgsession)

  #  Extract html form
  pgform <- rvest::html_form(pgsession)[[1]]

  #  Fill in username and password
  filled_form <- rvest::set_values(
    pgform,
    "txt_username" = usr,
    "txt_password" = pwd
  )

  # "click" login button
  dwnld_form <- rvest::submit_form(pgsession, filled_form) %>%
    xml2::read_html(.) %>%
    rvest::html_form(.)

  # Which button to download data from?
  if (is.null(bttn_nm)) {
    bttn_nm <- "ctl00$ContentPlaceHolder1$DownloadAll3"
  }

  #  "Click" button to download data and then parse the response
  dat_dwnld <- rvest::submit_form(
    pgsession,
    dwnld_form[[1]],
    submit = bttn_nm
  )

  httr::stop_for_status(dat_dwnld)

  out <- httr::content(dat_dwnld$response, type = "text/csv")

  return(out)
}

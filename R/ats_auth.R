`%>%` <- magrittr::`%>%`
utils::globalVariables(".")

# 1 - Global Objects ------------------------------------------------------

# * 1.1 - ats_base_url (base url for ATS website) -------------------------

ats_base_url <- "https://atsidaq.net"

# 2 - Internal Functions --------------------------------------------------

# * 2.1 - check_cookie ----------------------------------------------------

#' @title Check Cookie
#'
#' @description Check that a particular cookie exists for a certain site
#'
#' @param url http hostname (base url)
#' @param cookie name of the cookie to check
#'
#' @return True if cookie exists, false if not
#'
#' @export
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#'
#' check_cookie(ats_base_url, "ARRAffinity")
#'
#'}
#'
check_cookie <- function(url, cookie) {

  cookie %in% httr::cookies(httr::handle_find(url))$name

}

# 3 - Visible Functions ---------------------------------------------------

# * 3.1 - ats_login -------------------------------------------------------

#' @title Authenticate to ATS Website
#'
#' @description Pass credentials to ATS website and save login information
#'
#' @param usr username
#' @param pwd password
#'
#' @return True if login succeeds
#'
#' @seealso \code{\link{ats_logout}} for closing the session
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' ats_login("mary", ".")
#'
#' events <- fetch_ats_events()
#'
#' ats_logout()
#'
#' }
#'
ats_login <- function(usr, pwd) {

  # clear existing login if present
  if (check_cookie(ats_base_url, "user")) {
    ats_logout()
  }

  # log in to ATS website
  httr::RETRY(
    "POST",
    url = ats_base_url,
    path = list("Servidor.ashx"),
    body = list(
      consulta = "login",
      user = usr,
      pass = pwd
    ),
    encode = "form",
    quiet = TRUE
  ) %>%
    httr::stop_for_status("log in")

  # check that user cookie exists
  assertthat::assert_that(
    check_cookie(ats_base_url, "user"),
    msg = "Login failed."
  )

  # return true if login succeeded
  return(TRUE)

}

# * 3.2 - ats_logout ------------------------------------------------------

#' @title Close ATS Session
#'
#' @description Logs out of website
#'
#' @return True if log out request is successful, false if log out fails
#'
#' @seealso \code{\link{ats_login}} for starting the session
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' ats_login("mary", ".")
#'
#' fixes <- fetch_ats_positions()
#'
#' ats_logout()
#'
#'}
#'
ats_logout <- function() {

  # log out of ATS website
  httr::RETRY(
    "POST",
    url = ats_base_url,
    path = list("Servidor.ashx"),
    body = list(
      consulta = "logout"
    ),
    encode = "form",
    quiet = TRUE
  ) %>%
    httr::stop_for_status("log out")

  # return true if user cookie is gone
  (!check_cookie(ats_base_url, "user"))

}

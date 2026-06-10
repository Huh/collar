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
#' }
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
  ats_post(
    path = list("Servidor.ashx"),
    body = list(
      consulta = "login",
      user = usr,
      pass = pwd
    ),
    task = "log in"
  )

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
#' @description Logs out of the ATS website and clears the local session.
#'
#' @section Notes:
#'
#'   The server-side logout request is best-effort. On Windows, newer versions
#'   of libcurl (bundled with R >= 4.5) can fail the request with a schannel
#'   \code{SEC_E_CONTEXT_EXPIRED} error on a reused TLS connection (\code{curl}
#'   issue 18029) even though the session is closed on the server. That
#'   transport error is caught and ignored; the local session is always cleared
#'   by resetting the connection handle.
#'
#' @return \code{TRUE} once the local session has been cleared.
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

  # Best-effort server-side logout. On Windows, newer libcurl (>= 8.x, bundled
  # with R >= 4.5) can fail this request with schannel SEC_E_CONTEXT_EXPIRED on
  # a reused TLS connection (curl#18029) even though the server still ends the
  # session. A transport-layer failure here must not stop us from clearing the
  # local session below.
  try(
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
      httr::stop_for_status("log out"),
    silent = TRUE
  )

  # Reset the handle to drop cookies and the stale TLS context, so the local
  # session is cleared and the next login starts from a fresh connection.
  httr::handle_reset(ats_base_url)

  # return true if user cookie is gone
  (!check_cookie(ats_base_url, "user"))

}

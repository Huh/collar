# 1 - Global Objects ------------------------------------------------------

# * 1.1 - ats_base_url (base url for ATS website) -------------------------

ats_base_url <- "https://atsidaq.net"

# * 1.2 - ats.env (environment for managing ATS cookies) ------------------
ats.env <- new.env()

# 2 - Internal Functions --------------------------------------------------

# * 2.1 - ats_auth_cookies ------------------------------------------------
#   returns authentication cookies from login request

#' @title Format ATS Cookies for \code{httr}
#'
#' @return Named vector of cookie values
#'
#' @export
#'
#' @keywords internal
#'
ats_auth_cookies <- function() {

  # exit if user is not logged in
  assertthat::assert_that(
    inherits(ats.env$cookies, "data.frame"),
    msg = paste(
      "No login information available.",
      "Use the ats_login function to log in to your ATS account",
      sep = "\n"
    )
  )

  setNames(
    ats.env$cookies$value,
    ats.env$cookies$name
  )

}

# * 2.2 - ats_collar_cookies ----------------------------------------------
#   returns collar selection cookie in addition to
#   cookies from ats_auth_cookies

#' @title Format ATS Cookies for \code{httr}
#'
#' @return Named vector of cookie values,
#'   including one for selected collars
#'
#' @export
#'
#' @keywords internal
#'
ats_collar_cookies <- function(collars) {

  # add leading zeros if needed
  cc <- paste0("000000", as.character(collars)) %>%
    substr(nchar(.) - 5, nchar(.))

  # inner xml nodes (collar tags)
  cc <- sprintf("<collar>%s</collar>", cc)

  # mid-level xml nodes (inner dscgca tags)
  cc <- sprintf("  <dscgca>\n    %s\n  </dscgca>", cc)

  # outer node
  cc <- sprintf("<dscgca>\n%s\n</dscgca>", paste0(cc, collapse = "\n"))

  # return standard cookies with collar list appended
  c(ats_auth_cookies(), cgca = cc)

}

# 3 - Visible Functions ---------------------------------------------------

# * 3.1 - ats_login -------------------------------------------------------

#' @title Authenticate to ATS Website
#'
#' @description Pass credentials to ATS website and save login information.
#'
#' @param usr username
#' @param pwd password
#'
#' @return True if login succeeds, false if not.
#'
#' @seealso \code{\link{ats_logout}} for closing the session.
#'
#' @export
#'
#' @examples
#'
#' ats_login(usr = "my_username", pwd = "secret_code")
#'
#' events <- fetch_ats_events()
#'
#' ats_logout()
#'
ats_login <- function(usr, pwd) {

  # clear existing login if present
  if (!is.null(ats.env$cookies)) {
    ats_logout()
  }

  # log in to ATS website
  resp <- httr::RETRY(
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
  )

  # check that login was successful
  assertthat::assert_that(
    httr::status_code(resp) == 200,
    msg = paste(
      "Login failed.",
      paste("Status:", httr::status_code(resp)),
      paste("Response:", httr::content(resp)),
      sep = "\n"
    )
  )

  # save cookies from response
  ats.env$cookies <- httr::cookies(resp)

  # save username for reference
  ats.env$user <- usr

  # return true if successful
  inherits(ats.env$cookies, "data.frame") &&
    "user" %in% ats.env$cookies$name

}

# * 3.2 - ats_logout ------------------------------------------------------

#' @title Close ATS Session
#'
#' @description Logs out of website and removes
#'   authentication info from memory.
#'
#' @return True if log out request is successful, false if log out fails.
#'
#' @seealso \code{\link{ats_login}} for starting the session.
#'
#' @export
#'
#' @examples
#'
#' ats_login("demo", "PASSWORD09")
#'
#' fixes <- fetch_ats_positions()
#'
#' ats_logout()
#'
ats_logout <- function() {

  # log out of ATS website
  resp <- httr::RETRY(
    "POST",
    url = ats_base_url,
    path = list("Servidor.ashx"),
    body = list(
      consulta = "logout"
    ),
    encode = "form",
    httr::set_cookies(ats_auth_cookies()),
    quiet = TRUE
  )

  # clear login info from memory
  ats.env$cookies <- NULL
  ats.env$user <- NULL

  # check that log out was successful
  assertthat::assert_that(
    httr::status_code(resp) == 200,
    msg = paste(
      "The request to log out failed.",
      paste("Status:", httr::status_code(resp)),
      paste("Response:", httr::content(resp)),
      sep = "\n"
    )
  )

  (!inherits(ats.env$cookies, "data.frame"))

}

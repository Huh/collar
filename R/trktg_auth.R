################################################################################
#
# Track Tag Authentication Functions
#
################################################################################

# 1 - Global Objects ------------------------------------------------------

# * 1.1 - trktg_base_url (base url for track tag website) -----------------

trktg_base_url <- "https://www.tracktagllc.com"

# * 1.2 - trktg.env (environment for managing authentication info) --------

trktg.env <- new.env()

# 2 - Internal Functions --------------------------------------------------

# * 2.1 - trktg_token -----------------------------------------------------

#' @title Track Tag Request Verification Token
#'
#' @return Character request verification token for http requests.
#'
#' @section Notes:
#'
#'   There is also a cookie called \code{__RequestVerificationToken} set when
#'   logging in, but it doesn't appear to be used in subsequent requests. The
#'   token returned here is generated in a hidden input in the login page, and
#'   must be passed as form data in subsequent requests.
#'
#' @export
#'
#' @keywords internal
#'
trktg_token <- function() {

  assertthat::assert_that(
    "rvt" %in% ls(envir = trktg.env),
    msg = paste(
      "No login information available.",
      "Use the trktg_login function to log in to your Track Tag account",
      sep = "\n"
    )
  )

  trktg.env$rvt

}

# 3 - Visible Functions ---------------------------------------------------

# * 3.1 - trktg_login -----------------------------------------------------

#' @title Authenticate to Track Tag website
#'
#' @description Send username and password info to Track Tag website
#'   to log in to a user account.
#'
#' @param usr username The username associated with the account
#' @param pwd password The password used with the supplied username
#'
#' @return True if login succeeds, false if not.
#'
#' @seealso \code{\link{trktg_logout}} for closing the session.
#'
#' @export
#'
#' @examples
#'
#' trktg_login("some_user", "some_users_pw")
#'
#' fixes <- fetch_trktg_positions()
#'
#' trktg_logout()
#'
trktg_login <- function(usr, pwd) {

  # clear existing login info
  suppressWarnings(rm(rvt, envir = trktg.env))

  # use get request to obtain request verification token
  resp <- httr::RETRY(
    "GET",
    url = trktg_base_url,
    path = list("Login"),
    query = list(local = 1)
  )

  assertthat::assert_that(
    httr::status_code(resp) == 200,
    msg = paste(
      "Unable to access track tag website.",
      paste("Status:", httr::status_code(resp)),
      paste("Response:", httr::content(resp)),
      sep = "\n"
    )
  )

  rvt <- resp %>%
    httr::content() %>%
    xml2::xml_find_first("//input[@name='__RequestVerificationToken']") %>%
    xml2::xml_attr("value")

  assertthat::assert_that(
    !is.na(rvt),
    msg = "Unable to parse track tag token."
  )

  # submit form to authenticate
  resp <- httr::RETRY(
    "POST",
    url = trktg_base_url,
    path = list("Login"),
    query = list(local = 1),
    body = list(
      `__RequestVerificationToken` = rvt,
      UserName = usr,
      Password = pwd,
      PrivacyAgree = TRUE,
      RememberMe = FALSE
    ),
    encode = "form",
    quiet = TRUE
  )

  # check call was successful
  resp_title <- resp %>%
    httr::content() %>%
    xml2::xml_find_first("//title") %>%
    xml2::xml_text()

  assertthat::assert_that(
    httr::status_code(resp) == 200 && !grepl("Please Login", resp_title),
    msg = paste(
      "Login failed.",
      paste("Status:", httr::status_code(resp)),
      paste("Response:", httr::content(resp)),
      sep = "\n"
    )
  )

  trktg.env$rvt <- rvt

  # return true if successful
  "rvt" %in% ls(envir = trktg.env)

}

# * 3.2 - trktg_logout ----------------------------------------------------

#' @title Close Track Tag Session
#'
#' @description Removes authentication info from memory and logs out of
#'   Track Tag website.
#'
#' @return True if login info is erased, false if not.
#'
#' @seealso \code{\link{trktg_login}} for starting the session.
#'
#' @export
#'
#' @examples
#'
#' trktg_login("some_user", "some_users_pw")
#'
#' fixes <- fetch_trktg_positions()
#'
#' trktg_logout()
#'
trktg_logout <- function() {

  resp <- httr::RETRY(
    "GET",
    url = trktg_base_url,
    path = list("account", "logoff")
  )

  suppressWarnings(rm("rvt", envir = trktg.env))

  length(ls(envir = trktg.env)) == 0 && httr::status_code(resp) == 200

}

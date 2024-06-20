################################################################################
#
# Lotek Authentication Functions
#
################################################################################

# base url for API calls
lotek_base_url <- "https://webservice.lotek.com"

# environment for managing authentication info
ltk.env <- new.env()

# list where authentication info is stored
ltk.env$ltk <- list()

#' @title Authenticate to Lotek API
#'
#' @description Send username and password info to Lotek API
#'   to log in to a user account.
#'
#' @param usr username The username associated with the account
#' @param pwd password The password used with the supplied username
#' @param user deprecated to align with other functions in collar
#' @param pw deprecated to align with other functions in collar
#'
#' @return True if login succeeds, false if not.
#'
#' @seealso \code{\link{lotek_logout}} for closing the session.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' lotek_login("demo", "PASSWORD09")
#'
#' alerts <- fetch_lotek_alerts()
#'
#' lotek_logout()
#' }
lotek_login <- function(usr, pwd, user, pw) {

  if (!missing(user)) {
    warning(paste(
      "Parameter 'user' is deprecated and will be replaced with",
      "'usr' in a future version. Please update your code."
    ))
    if (missing(usr)) {
      usr <- user
    }
  }

  if (!missing(pw)) {
    warning(paste(
      "Parameter 'pw' is deprecated and will be replaced with",
      "'pwd' in a future version. Please update your code."
    ))
    if (missing(pwd)) {
      pwd <- pw
    }
  }

  # clear existing login info
  ltk.env$ltk <- list()

  # call API
  resp <- httr::RETRY(
    "POST",
    url = lotek_base_url,
    path = list("API", "user", "login"),
    body = list(
      grant_type = "password",
      username = usr,
      Password = pwd
    ),
    encode = "form",
    quiet = TRUE
  )

  # check call was successful
  assertthat::assert_that(
    httr::status_code(resp) == 200,
    msg = paste(
      "Login failed.",
      paste("Status:", httr::status_code(resp)),
      paste("Response:", httr::content(resp)),
      sep = "\n"
    )
  )

  # convert json to list object
  ltk.env$ltk <- httr::content(resp)

  # return true if successful
  as.logical(length(ltk.env$ltk))

}

#' @title Close Lotek Session
#'
#' @description Removes authentication info from memory.
#'
#' @section Notes:
#'
#'   The name is somehwat misleading. As of writing the Lotek API does not
#'   include functionality to terminate the session on the server, so this
#'   function removes locally stored authentication information but does
#'   not notify the server.
#'
#' @return True if login info is erased, false if not.
#'
#' @seealso \code{\link{lotek_login}} for starting the session.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' lotek_login("demo", "PASSWORD09")
#'
#' alerts <- fetch_lotek_devices()
#'
#' lotek_logout()
#' }
lotek_logout <- function() {

  # at the moment this just removes login info from memory
  ltk.env$ltk <- list()

  (length(ltk.env$ltk) == 0)

}

#' @title Refresh Lotek Access Token
#'
#' @description Attempts to acquire a new access token when current one expires.
#'
#' @section Notes:
#'
#'   This function only works sporadically and the reasons are unclear.
#'   If this function fails a new access token can be created by resending
#'   the user name and password with lotek_login.
#'
#' @param force_refresh Logical - only used for testing.
#'
#' @return True if current token is still valid or token is refreshed,
#'   false if token is expired and attempt to refresh failed.
#'
#' @seealso \code{\link{lotek_login}} for starting a new session.
#'
#' @export
#'
#' @keywords internal
#'
lotek_refresh_token <- function(force_refresh = FALSE) {

  # get token expiration and current time
  exp_date <- lubridate::dmy_hms(ltk.env$ltk$.expires)
  now_date <- lubridate::now(tzone = "GMT")

  # check expiration and force paramter
  if (any((now_date >= exp_date), force_refresh)) {

    # send request
    resp <- httr::RETRY(
      "POST",
      url = lotek_base_url,
      path = list("API", "user", "login"),
      body = list(
        grant_type = "refresh_token",
        username = ltk.env$ltk$userName,
        refresh_token = ltk.env$ltk$refresh_token
      ),
      encode = "form",
      quiet = TRUE
    )

    # clear previous tokens
    ltk.env$ltk <- list()

    # verify successful status
    assertthat::assert_that(
      httr::status_code(resp) == 200,
      msg = paste(
        "Login info expired and attempt to refresh failed.",
        "Try using the lotek_login function to log back in.",
        paste("Status:", httr::status_code(resp)),
        paste("Response:", httr::content(resp)),
        sep = "\n"
      )
    )

    # convert json to list object
    ltk.env$ltk <- httr::content(resp)

  }

  # return true if valid tokens exist
  as.logical(length(ltk.env$ltk))

}

#' @title Format Access Token as String
#'
#' @return Character access token for API calls.
#'
#' @export
#'
#' @keywords internal
#'
lotek_token <- function() {

  # exit if user is not logged in
  assertthat::assert_that(
    length(ltk.env$ltk) > 0,
    msg = paste(
      "No login information available.",
      "Use the lotek_login function to log in to your Lotek account",
      sep = "\n"
    )
  )

  # refresh auth token if needed
  lotek_refresh_token()

  paste(
    ltk.env$ltk$token_type,
    ltk.env$ltk$access_token
  )

}

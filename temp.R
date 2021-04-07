# base url for ATS website
ats_base_url <- "https://atsidaq.net"

# environment for managing selenium session
ats.env <- new.env()

# list where authentication info is stored
ats.env$ats <- list()

# create folder for downloaded files
ats.env$td <- tempdir()
ats.env$td <- suppressWarnings(
  normalizePath(file.path(ats.env$td, "ats_tmp"))
)
suppressWarnings(unlink(ats.env$td, recursive = TRUE))
try(dir.create(ats.env$td))

# send request
resp <- httr::RETRY(
  "POST",
  url = ats_base_url,
  path = list("../../Servidor.ashx"),
  body = list(
    consulta = "login",
    email = "Aval_Elk20",
    pass = "Adobe20$"
  ),
  encode = "form",
  quiet = TRUE
)
curl <- RCurl::getCurlHandle()
curl <- RCurl::curlSetOpt(cookiejar = 'cookies.txt', followlocation = TRUE, autoreferer = TRUE, curl = curl)
html <- RCurl::getURL('https://atsidaq.net/login.aspx', curl = curl)
vs <- as.character(sub('.*id="__VIEWSTATE" value="([0-9a-zA-Z+/=]*).*', '\\1', html))
vs_gen <- as.character(sub('.*id="__VIEWSTATEGENERATOR" value="([0-9a-zA-Z+/=]*).*', '\\1', html))
params <- list(
    'ctl01$__VIEWSTATE'                                  = vs,
    'ctl01$__VIEWSTATEGENERATOR'                                  = vs_gen,
    'ctl01$username'    = 'Aval_Elk20',
    'ctl01$password'    = 'Adobe20$',
    'ctl01$log' = 'Sign In'
)
html = RCurl::postForm('https://atsidaq.net/login.aspx', .params = params, curl = curl)

str(html)
cat(html)

resp <- httr::POST(
  url = ats_base_url,
  path = list("../../Servidor.ashx"),
  body = list(
    consulta = "login",
    user = "Aval_Elk20",
    pass = "Adobe20$"
  ),
  encode = "form"
)

httr::content(resp)

str(resp)

loc <- httr::GET(
  url = ats_base_url,
  path = "../download_all_data/Download_all_data.aspx?dw=all",
  httr::set_cookies(ats_auth_cookies())
)

loc_data <- loc$content %>%
  readBin(what = "character", n = length(loc$content) / 4) %>%
  textConnection() %>%
  read.csv()

head(loc_data)

collar_list <- c(41516, 43655, 43660)

ats_collar_cookie <- function(collars) {

  # add leading zeros if needed
  x <- paste0("000000", as.character(collars)) %>%
    substr(nchar(.) - 5, nchar(.))

  # inner xml nodes (collar tags)
  x <- sprintf("<cookie>%s</cookie>", x)

  # mid-level xml nodes (inner dscgca tags)
  x <- sprintf("  <dscgca>\n    %s\n  </dscgca>", x)

  # outer node
  x <- sprintf("<dscgca>\n%s\n</dscgca>", x)

}

outer_xml <- c("<dscgca>", "</dscgca>")
inner_xml <- paste(" ", outer_xml)
collar_cookie <- paste0(
  outer_xml,
  inner_xml,
  paste0(
    "    <collar>",
    collar_list,
    "</collar>"
  ),
  collapse = "\n"
)
cat(sprintf(
"  <dscgca>
    <cookie>%s</cookie>
  </dscgca>
",
  collar_list
))
cat(collar_cookie)

# API Calls

tibble::tribble(
  ~desc, ~path, ~method, ~consulta, ~params, ~incl_cgca,
  "Download All Data", "../download_all_data/Download_all_data.aspx?dw=all",
    "GET", NA_character_, list(), FALSE,
  "Download Last 5 By Collar", "../../Servidor.ashx",
    "POST", "download_txt_collars",
    list(type = "002", parameter1 = "", parameter2 = ""), TRUE,
)

resp <- httr::POST(
  url = ats_base_url,
  path = "Servidor.ashx",
  body = list(
    consulta = "download_txt_collars",
    type = "002",
    parameter1 = "",
    parameter2 = ""
  ),
  encode = "form",
  httr::set_cookies(ats_collar_cookies(collar_list))
)

cols <- httr::content(resp) %>%
  xml2::xml_find_first("//Table") %>%
  xml2::xml_children() %>%
  xml2::xml_name()

names(cols) <- cols

ats_dat <- purrr::map_dfc(
  cols,
  ~ httr::content(resp) %>%
    xml2::xml_find_all(paste0("//", .x)) %>%
    xml2::xml_text()
)

tf <- tempfile(fileext = ".xml")
w_tf <- file(tf, "wb")
writeBin(resp$content, w_tf)
close(w_tf)

shell(paste("notepad.exe", tf))

# types for download_txt_collars
# 001 - date range, uses parameter1 & parameter2 (BROKEN)
# 002 - last 5
# 003 - last 10
# 004 - all data points

# experimenting with date range

# this is how the site sends it

resp <- httr::POST(
  url = ats_base_url,
  path = "Servidor.ashx",
  body = list(
    consulta = "download_txt_collars",
    type = "001",
    parameter1 = "01/01/2020",
    parameter2 = "03/31/2021"
  ),
  encode = "form",
  httr::set_cookies(ats_collar_cookies(collar_list))
)

httr::http_status(resp)
# $category
# [1] "Server error"

# $reason
# [1] "Internal Server Error"

# $message
# [1] "Server error: (500) Internal Server Error"

# try with more universal date format

resp <- httr::POST(
  url = ats_base_url,
  path = "Servidor.ashx",
  body = list(
    consulta = "download_txt_collars",
    type = "001",
    parameter1 = "2020-01-01",
    parameter2 = "2021-03-31"
  ),
  encode = "form",
  httr::set_cookies(ats_collar_cookies(collar_list))
)

httr::http_status(resp)
# $category
# [1] "Server error"

# $reason
# [1] "Internal Server Error"

# $message
# [1] "Server error: (500) Internal Server Error"

# try with start only

resp <- httr::POST(
  url = ats_base_url,
  path = "Servidor.ashx",
  body = list(
    consulta = "download_txt_collars",
    type = "001",
    parameter1 = "2020-01-01",
    parameter2 = ""
  ),
  encode = "form",
  httr::set_cookies(ats_collar_cookies(collar_list))
)

httr::http_status(resp)

# back to short date format
resp <- httr::POST(
  url = ats_base_url,
  path = "Servidor.ashx",
  body = list(
    consulta = "download_txt_collars",
    type = "001",
    parameter1 = "01/01/2020",
    parameter2 = ""
  ),
  encode = "form",
  httr::set_cookies(ats_collar_cookies(collar_list))
)

httr::http_status(resp)

# try with ambiguous date in case short format is d/m/y
resp <- httr::POST(
  url = ats_base_url,
  path = "Servidor.ashx",
  body = list(
    consulta = "download_txt_collars",
    type = "001",
    parameter1 = "01/01/2020",
    parameter2 = "03/01/2021"
  ),
  encode = "form",
  httr::set_cookies(ats_collar_cookies(collar_list))
)

httr::http_status(resp)

# try escaping slashes
resp <- httr::POST(
  url = ats_base_url,
  path = "Servidor.ashx",
  body = list(
    consulta = "download_txt_collars",
    type = "001",
    parameter1 = "01%2F01%2F2020",
    parameter2 = "03%2F01%2F2021"
  ),
  encode = "form",
  httr::set_cookies(ats_collar_cookies(collar_list))
)

httr::http_status(resp)

# try homemade escaping
resp <- httr::POST(
  url = ats_base_url,
  path = "Servidor.ashx",
  body = list(
    consulta = "download_txt_collars",
    type = "001",
    parameter1 = "01-01-2020",
    parameter2 = "03-01-2021"
  ),
  encode = "form",
  httr::set_cookies(ats_collar_cookies(collar_list))
)

httr::http_status(resp)

# nothing

# ok, let's try getting a list of devices
# for nathaniel's account there are 31 active collars
#   and 6 inactive collars
resp <- httr::POST(
  url = ats_base_url,
  path = "Servidor.ashx",
  body = list(
    consulta = "get_collars_user",
    valor = ""
  ),
  encode = "form",
  httr::set_cookies(ats_collar_cookies(collar_list))
)

httr::http_status(resp)
# $category
# [1] "Success"

# $reason
# [1] "OK"

# $message
# [1] "Success: (200) OK"

httr::content(resp)
# {xml_document}
# <NewDataSet>

# so it works but it doesn't work

resp <- httr::POST(
  url = ats_base_url,
  path = "Servidor.ashx",
  body = list(
    consulta = "get_collars_user",
    valor = "active"
  ),
  encode = "form",
  httr::set_cookies(ats_collar_cookies(collar_list))
)

httr::http_status(resp)
# $category
# [1] "Success"

# $reason
# [1] "OK"

# $message
# [1] "Success: (200) OK"

httr::content(resp)
# {xml_document}
# <NewDataSet>
#  [1] <Table1>\n  <collar>041516</collar>\n  <existe>true</existe>\n</Table1>
#  [2] <Table1>\n  <collar>043655</collar>\n  <existe>true</existe>\n</Table1>
#  [3] <Table1>\n  <collar>043660</collar>\n  <existe>true</existe>\n</Table1>
#  ...

# yay - so valor (filter) is required
# valor options:
#   active
#   no_active
#   active_low_batt
#   active_mortality
#   active_birth_triggers

ats_dev <- httr::content(resp) %>%
  xml2::xml_find_all("//collar") %>%
  xml2::xml_text()

length(ats_dev)
# [1] 31

resp <- httr::POST(
  url = ats_base_url,
  path = "Servidor.ashx",
  body = list(
    consulta = "get_collars_user",
    valor = "no_active"
  ),
  encode = "form",
  httr::set_cookies(ats_collar_cookies(collar_list))
)

ats_dev <- c(
  ats_dev,
  httr::content(resp) %>%
    xml2::xml_find_all("//collar") %>%
    xml2::xml_text()
)

length(ats_dev)
# [1] 37

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

resp <- httr::RETRY(
  "POST",
  url = ats_base_url,
  path = list("Servidor.ashx"),
  body = list(
    consulta = "cargarcokies",
    cadenacheckbox = "41516_43655_43660_"
  ),
  encode = "form",
  quiet = TRUE
)

tf <- tempfile()
w_tf <- file(tf, "wb")
resp$content %>%
    readBin(what = "character", n = length(resp$content) / 4) %>%
    writeBin(w_tf)

close(w_tf)

shell(paste("notepad.exe", tf))

foo <- function(x = NULL) {
  missing(x)
}

bar <- function(y = NULL) {
  is.null(y)
}

blah <- function(z) {
  c(missing(z), is.null(z))
}

bleh <- function(xyz) {
  inherits(xyz, "numeric")
}
bar(3)

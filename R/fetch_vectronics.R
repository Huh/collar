#' Download data from Vectronics' web service
#'
#' A function to download data from the Vectronics' web service. The function relies on a directory containing collar keys \code{key_dir}. The function will then attempt to download the relevant data using the keys in the \code{key_dir} directory. The user may alter the data downloaded by, for example, downloading data after some arbitrary date, downloading different types of data or after a data id.
#'
#' @inheritParams get_keys
#' @inheritParams build_vec_url
#'
#' @details
#' This function assumes that the user stores key files for each collar deployed in a single directory and that directory only contains key files. The function will extract all of the keys contained in this directory and attempt to download data for each collar.
#'
#'  If desired, the user may limit the amount of data downloaded by supplying a data id or start date. In both cases only data after the data id or date will be downloaded. One value of data_id is required for each collar. For start_date, the user has the option of supplying a single value or one for each collar.
#'
#'  The type argument allows the user to download different types of information collected by Vectronics collars. The possible options are
#'
#' \itemize{
#'   \item "gps" returns location data
#'   \item "act" returns activity data
#'   \item "mit" returns implant mortality data
#'   \item "mor" returns mortality data
#'   \item "prx" returns proximity data
#'   \item "SEP" returns separation data
#'   \item "TRAP" returns trap event data
#' }
#'
#'
#' @return An object of class collar, which inherits from data.frame, tbl_df
#' @export
#'
#' @examples
#' \dontrun{
#'   fetch_vectronics(key_dir = "./keys")
#' }

fetch_vectronics <- function(key_dir = key_dir,
                             base_url = NULL,
                             after_data_id = NULL,
                             type = "gps",
                             start_date = NULL,
                             which_date = NULL) {
  ids <- get_id_from_key(key_dir)

  keys <- get_keys(key_dir)

  urls <- build_vec_urls(
    base_url = base_url,
    collar_id = ids,
    collar_key = keys,
    type = type,
    after_data_id = after_data_id,
    start_date = start_date,
    which_date = which_date
  )

  call_vec_api(urls)
}

#' Create paths to key files
#'
#' @inheritParams get_keys
#' @param ext Defaults to keyx, which should be correct for key files, but could also be a regular expression used to match file extension.
#'
#' @return Character vector defining path to each file matching \code{ext}
#' @keywords internal
#'
#' @examples
#' collar:::key_paths(
#'   system.file("extdata", package = "collar")
#' )
key_paths <- function(key_dir, ext = "keyx") {
  assertthat::assert_that(assertthat::is.string(key_dir))
  assertthat::assert_that(assertthat::is.dir(key_dir))
  assertthat::assert_that(assertthat::is.readable(key_dir))

  list.files(key_dir, pattern = ext, full.names = TRUE)
}

#' Extract collar ID from key
#'
#' @inheritParams get_keys
#' @param ... Other arguments to pass to key_paths
#'
#' @details Vectronics includes the collar ID in the key file name as well as within the XML of the .keyx file.  These functions give the user the option of either extracting the information from the name of the file(s) get_id_from_fnames or reading the ID encoded in the key file get_id__from_key. Storing information in file names or directory names is not a good practice and we would rather have all information stored in files and so prefer and suggest reading the XML within the files.
#'
#' @keywords internal
#' @return character vector of collar IDs
#'
#' @examples
#' collar:::get_id_from_fnames(
#'   system.file("extdata", package = "collar")
#' )
#' collar:::get_id_from_key(
#'   system.file("extdata", package = "collar")
#' )
get_id_from_fnames <- function(key_dir, ...) {
  assertthat::assert_that(assertthat::is.dir(key_dir))
  assertthat::assert_that(assertthat::is.readable(key_dir))

  paths <- key_paths(key_dir, ...)

  assertthat::assert_that(
    all(
      file.exists(key_paths(key_dir))
    )
  )

  gsub("(.*Collar)([0-9]+)(_.*)", "\\2", paths)
}

#' @rdname get_id_from_fnames

get_id_from_key <- function(key_dir) {
  assertthat::assert_that(assertthat::is.dir(key_dir))
  assertthat::assert_that(assertthat::is.readable(key_dir))

  paths <- key_paths(key_dir)

  purrr::map_chr(
    paths,
    ~xml2::read_xml(.x) %>%
      xml2::xml_find_all("//collar") %>%
      xml2::xml_attr("ID")
  )
}

#' Retrieve keys from a path to a key file or read all keys contained within a directory
#'
#' The key is need to call the Vectronics API and given the length of the key copy and paste errors seems likely.  The functions here help users access the keys in a programmatic fashion.
#'
#' @param key_path A length 1 character string describing the full path to a single key file.
#' @param key_dir path to a single directory containing, potentially many, key files
#'
#' @details get_keys is a wrapper for get_key where the former is retained to allow users to access a single file if desired.
#'
#' @keywords internal
#' @return A character vector of keys
#'
#' @examples
#' collar:::get_key(
#'   system.file(
#'     "extdata",
#'     "Collar123456_Registration.keyx",
#'     package = "collar"
#'   )
#' )
#'
#' collar:::get_keys(
#'   system.file(
#'     "extdata",
#'     package = "collar"
#'   )
#' )

get_key <- function(key_path) {
  is_file <- function(x) {
    !assertthat::is.dir(x)
  }

  assertthat::on_failure(is_file) <- function(call, env) {
    "key_path should be the path to a single file, not a directory.  Did you mean to call get_keys?"
  }

  assertthat::assert_that(is_file(key_path))
  assertthat::assert_that(is.character(key_path))
  assertthat::assert_that(file.exists(key_path))
  assertthat::assert_that(assertthat::has_extension(key_path, ext = "keyx"))
  assertthat::assert_that(assertthat::is.readable(key_path))

  xml2::read_xml(key_path) %>%
    xml2::xml_find_all("//key") %>%
    xml2::xml_text()
}

#' @rdname get_key

get_keys <- function(key_dir) {
  assertthat::assert_that(assertthat::is.dir(key_dir))
  assertthat::assert_that(assertthat::is.readable(key_dir))

  paths <- key_paths(key_dir)

  purrr::map_chr(paths, get_key)
}

#' Functions to build the url required to call the API
#'
#' The url is composed of a base that never changes, a collar id, a collar key
#' data type and either a start id or date at a minimum.  These pieces must be
#' put together in a rather particular order with appropriate punctuation.  This
#' is the role of these functions.
#'
#' @param base_url NULL or the url to build from, the default NULL should
#' suffice in almost all cases
#' @param collar_id The ID(s) of the collars to query for data
#' @param collar_key The key(s) of the collars to query for data
#' @param type The data type, options include "gps", "act", "mit", "mor", "prx",
#' "SEP" and "TRAP".  See details.
#' @param after_data_id All data types have a unique ID maintained by the
#' manufacturer.  Use this parameter to download data after the supplied data
#' ID.  Must be equal in length to collar_id and collar_key.  Only one of
#' start_date and after_data_id may be supplied.
#' @param start_date A character vector specifying date and time as
#' DD-MM-YYYYTHH:MM:SS (the T is required, for example 01-01-2018T00:00:00).
#' Supplying this parameter will cause the API to only return data collected
#' after the supplied date.  Must be equal in length to collar_id and
#' collar_key.  Only one of start_date and after_data_id may be supplied.
#' @param which_date either scts or acquisition, indicates date column to use
#' for subsetting
#'
#' @details The functions build_vec_url and build_vec_urls differ only
#' in that build_vec_url builds a single url from a collection of length 1
#' arguments while build_vec_urls will function on vectors of inputs to
#' create numerous urls.  Functionally the user is encouraged to only use the
#' plural version so that code will function regardles of the length of the
#' input.
#'
#' Data types are defined by the user and only a single data type is returned by
#' the API for any given call.  The url tells the API which data type to return,
#' such that passing
#' \itemize{
#'   \item "gps" returns location data
#'   \item "act" returns activity data
#'   \item "mit" returns implant mortality data
#'   \item "mor" returns mortality data
#'   \item "prx" returns proximity data
#'   \item "SEP" returns separation data
#'   \item "TRAP" returns trap event data
#' }
#'
#' @keywords internal
#' @return Formatted url(s) as a character string
#'
#' @examples
#' collar:::build_vec_urls(
#'   base_url = NULL,
#'   collar_id = collar:::get_id_from_key(
#'     system.file("extdata", package = "collar")
#'   ),
#'   collar_key = collar:::get_keys(
#'     system.file(
#'       "extdata",
#'       package = "collar"
#'     )
#'   ),
#'   type = "act"
#' )
build_vec_url <- function(base_url = NULL,
                          collar_id = NULL,
                          collar_key = NULL,
                          type = c(
                            "gps", "act", "mit", "mor", "prx", "SEP", "TRAP"
                          ),
                          after_data_id = NULL,
                          start_date = NULL,
                          which_date = NULL) {
  one_null <- function(x, y) {
    is.null(x) | is.null(y)
  }

  assertthat::on_failure(one_null) <- function(call, env) {
    "The Vectronics API cannot accept both the after_data_id and start_date arguments at the same time.  Please change one or both to NULL."
  }

  assertthat::assert_that(one_null(after_data_id, start_date))

  assertthat::assert_that(
    type %in% c("gps", "act", "mit", "mor", "prx", "SEP", "TRAP")
  )

  if(!is.null(start_date)){
    assertthat::assert_that(
      which_date %in% c("scts", "acquisition")
    )
  }

  if (is.null(base_url)) {
    base_url <- "https://wombat.vectronic-wildlife.com:9443"
  }

  url <- httr::modify_url(
    base_url,
    path = list(
      "collar",
      collar_id,
      type
    ),
    query = list(
      "collarkey" = collar_key
    )
  )

  if (!is.null(after_data_id)) {
    url <- paste0(url, "&gt-id=", after_data_id)
  }

  if (!is.null(start_date)) {
    url <- switch(
      which_date,
      "scts" = paste0(url, "&afterScts=", start_date),
      "acquisition" = paste0(url, "&afterAcquisition=", start_date)
    )
  }

  url
}

#' @rdname build_vec_url

build_vec_urls <- function(base_url = NULL,
                           collar_id = NULL,
                           collar_key = NULL,
                           type = c(
                             "gps", "act", "mit", "mor", "prx",
                             "SEP", "TRAP"
                           ),
                           after_data_id = NULL,
                           start_date = NULL,
                           which_date = c("scts", "acquisition")) {
  purrr::map2_chr(
    collar_id, collar_key,
    ~build_vec_url(
      base_url,
      .x,
      .y,
      type,
      after_data_id,
      start_date,
      which_date
    )
  )
}

#' A basic call to the Vectronics API
#'
#' @param url a character string representing the url to query
#'
#' @keywords internal
#' @return tibble
#'
#' @examples
#' #  Build url from base url, collar id, collar key and data type
#' url <- collar:::build_vec_urls(
#'   base_url = NULL,
#'   collar_id = collar:::get_id_from_key(
#'     system.file("extdata", package = "collar")
#'   ),
#'   collar_key = collar:::get_keys(
#'     system.file(
#'       "extdata",
#'       package = "collar"
#'     )
#'   ),
#'   type = "act"
#' )
#' # Call API - This will not work without a valid key and collar id
#' # collar:::call_vec_api(url)
call_vec_api <- function(url) {
  assertthat::assert_that(
    RCurl::url.exists("www.google.com"),
    msg = "Are you connected to the internet? You must have an internet connection to call the API"
  )

  st_time <- Sys.time()

  message("Downloading raw collar data...")

  pb <- dplyr::progress_estimated(length(url))

  resp <- purrr::map(
    url,
    ~{
      pb$tick()$print()
      httr::GET(.x)
    }
  )

  message("Parsing data...")

  pb <- dplyr::progress_estimated(length(resp))

  parse <- purrr::map_df(
    resp,
    ~{
      pb$tick()$print()
      jsonlite::fromJSON(
        httr::content(.x, "text"),
        simplifyVector = TRUE
      )
    }
  ) %>%
    tibble::as.tibble()

  difftime(Sys.time(), st_time, units = "mins") %>%
    as.numeric() %>%
    round(2) %>%
    message("Done. Elapsed time: ", ., " minutes")
  return(parse)
}

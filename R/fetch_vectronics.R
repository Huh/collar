#' Download data from Vectronics' web service
#'
#' A function to download data from the Vectronics' web service. The function relies on a directory containing collar keys \code{key_dir}. The function will then attempt to download the relevant data using the keys in the \code{key_dir} directory. The user may alter the data downloaded by, for example, downloading data after some arbitrary date, downloading different types of data or after a data id.
#'
#' @inheritParams get_keys
#' @inheritParams build_vec_url
#' @inheritParams fetch_ats
#'
#' @details
#' This function assumes that the user stores key files for each collar deployed in a single directory and that directory only contains key files. The function will extract all of the keys contained in this directory and attempt to download data for each collar.
#'
#'  If desired, the user may limit the amount of data downloaded by supplying a data id or start date. In both cases only data after the data id or date will be downloaded. One value of data_id is required for each collar. For start_date, the user has the option of supplying a single value or one for each collar.
#'
#'  The type argument allows the user to download different types of information collected by Vectronics collars. The possible options are
#'
#' \itemize{
#'   \item "count" returns count of data records, used with values below
#'   \item "gps" returns location data
#'   \item "act" returns activity data
#'   \item "mit" returns implant mortality data
#'   \item "mor" returns mortality data
#'   \item "prx" returns proximity data
#'   \item "sep" returns separation data
#'   \item "trap" returns trap event data
#'   \item "vit" returns vaginal implant data
#' }
#'
#' Column names are adjusted using a custom function, but the user can pass any function they want to manipulate column names (e.g. make.names). The default removes non-ASCII characters, coerces all characters to lower case and replaces "." with "_".
#'
#' @return Tibble
#' @export
#'
#' @examples
#' \dontrun{
#' # Use get_paths function to extract full path to each key file in directory
#' path <- get_paths(
#'   system.file(
#'     "extdata",
#'     package = "collar"
#'   )
#' )
#'
#' # Download count of GPS data
#' fetch_vectronics(path, type = "gps", count = TRUE)
#'
#' # Download count of activity data
#' fetch_vectronics(path, type = "act", count = TRUE)
#'
#' # Download all data
#' all_dat <- fetch_vectronics(path, type = "gps")
#'
#' # Fake multiple key_paths
#' too_much <- fetch_vectronics(c(path, path), type = "gps")
#'
#' # Download data after some data id
#' id_pos <- all_dat$idposition
#' data_id <- id_pos[which(id_pos == (max(id_pos) - 10))]
#' new_dat <- fetch_vectronics(path, type = "gps", after_data_id = data_id)
#'
#' (nrow(new_dat) == 10)
#'
#' # Download all data after some acquisition date
#' after <- "2018-06-30T00:00:00"
#' after_dat <- fetch_vectronics(
#'   path,
#'   type = "gps",
#'   start_date = after,
#'   which_date = "acquisition"
#' )
#' }
fetch_vectronics <- function(key_paths,
                             base_url = NULL,
                             after_data_id = NULL,
                             type = "gps",
                             count = FALSE,
                             start_date = NULL,
                             which_date = NULL,
                             rename_fun = adj_col_nms) {

  ids <- get_id_from_key(key_paths)

  keys <- get_keys(key_paths)

  urls <- build_vec_urls(
    base_url = base_url,
    collar_id = ids,
    collar_key = keys,
    type = type,
    count = count,
    after_data_id = after_data_id,
    start_date = start_date,
    which_date = which_date
  )

  call_vec_api(urls)
}

#' Extract path(s) to files in a directory
#'
#' @param key_dir Path to a directory
#' @param ext Defaults to keyx, which should be correct for Vectronics key files, but could also be a regular expression used to match any file extension.
#' @param ... other arguments to pass to list.files, see ?list.files
#'
#' @return full path to each file matching \code{ext}
#' @family get
#' @export
#' @examples
#' # Example collar key
#' get_paths(
#'   system.file("extdata", package = "collar"),
#'   ext = "keyx$"
#' )
#'
#' # Example trap key
#' get_paths(
#'   system.file("extdata", "TrapKeys", package = "collar")
#' )
#'
#' # All csv files included in this package
#' get_paths(
#'   system.file("extdata", package = "collar"),
#'   ext = "csv$"
#' )
#'
#' # All keyx files included with this package
#' get_paths(system.file("extdata", package = "collar"), recursive = TRUE)
get_paths <- function(key_dir, ext = "keyx$", ...) {
  assertthat::assert_that(assertthat::is.string(key_dir))
  assertthat::assert_that(assertthat::is.dir(key_dir))
  assertthat::assert_that(assertthat::is.readable(key_dir))

  list.files(
    normalizePath(key_dir, winslash = "/", mustWork = TRUE),
    full.names = TRUE,
    pattern = ext,
    ...
  )
}

#' Extract collar ID from key files
#'
#' @inheritParams get_keys
#'
#' @details
#' Vectronics includes the collar ID in the XML of the .keyx file.  The get_id_from_key function gives the user the ability to read the ID encoded in the key file. This function was originally intended for internal package use, but is exported to package users in the hope it is helpful for reporting or other purposes. There is no reason to call it within the established package workflow.
#'
#' @return character vector of collar IDs
#' @export
#' @family get
#'
#' @examples
#' get_id_from_key(
#'   get_paths(system.file("extdata", package = "collar"))
#' )
get_id_from_key <- function(key_paths) {
  assertthat::assert_that(
    all(file.exists(key_paths)),
    msg = "In get_id_from_key, one or more key_paths point to files that do not exist"
  )
  assertthat::assert_that(
    all(purrr::map_lgl(key_paths, assertthat::is.readable)),
    msg = "In get_id_from_key, one or more key_paths point to files that cannot be read, is this a permissions issue?"
  )
  assertthat::assert_that(
    all(purrr::map_lgl(key_paths, assertthat::has_extension, ext = "keyx")),
    msg = "In get_id_from_key, one of more key_paths point to files that are not .keyx files"
  )

  purrr::map_chr(
    key_paths,
    ~xml2::read_xml(.x) %>%
      xml2::xml_find_all("//collar") %>%
      xml2::xml_attr("ID")
  )
}

#' Extract alphanumeric key from key files
#'
#' The key is needed to call the Vectronics API and given the length of the key copy and paste errors seems likely.  The functions here help users access the keys in a programmatic fashion. We don't anticipate much need for users to use this function as it was originally intended to be internal to the package, but if listing keys is needed for reporting purposes or otherwise the function is made available.
#'
#' @param key_paths The full path to one or more key files
#' @family get
#' @export
#' @return A character vector of keys
#'
#' @examples
#'
#' my_key <- system.file(
#'   "extdata",
#'   "Collar1000001_Registration.keyx",
#'   package = "collar"
#' )
#'
#' # Extract one key
#' get_keys(my_key)
#'
#' # Extract keys from "several" files
#' get_keys(c(my_key, my_key, my_key))
get_keys <- function(key_paths) {
  assertthat::assert_that(
    all(purrr::map_lgl(key_paths, file.exists)),
    msg = "In get_keys, one or more files in key_paths do not exist"
  )
  is_file <- function(x) {
    !assertthat::is.dir(x)
  }
  assertthat::assert_that(
    all(purrr::map_lgl(key_paths, is_file)),
    msg = "In get_keys, key_paths must be files not directories"
  )
  assertthat::assert_that(
    all(purrr::map_lgl(key_paths, assertthat::has_extension, ext = "keyx")),
    msg = "In get_keys, one or more files in key_paths do not have extension .keyx"
  )
  assertthat::assert_that(
    all(purrr::map_lgl(key_paths, assertthat::is.readable)),
    msg = "In get_keys, one or more files in key_paths are not readable, is this a permissions issue?"
  )

  purrr::map_chr(key_paths,
    ~ xml2::read_xml(.x) %>%
      xml2::xml_find_all("//key") %>%
      xml2::xml_text()
  )
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
#' "sep", "trap" and "vit".  See details.
#' @param count logical indicating whether you want a count of the data type
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
#'   \item "count" modifies requests below to return count of data type
#'   \item "gps" returns location data
#'   \item "act" returns activity data
#'   \item "mit" returns implant mortality data
#'   \item "mor" returns mortality data
#'   \item "prx" returns proximity data
#'   \item "sep" returns separation data
#'   \item "trap" returns trap event data
#'   \item "vit" returns vaginal implant data
#' }
#'
#' @keywords internal
#' @return Formatted url(s) as a character string
build_vec_url <- function(base_url = NULL,
                          collar_id = NULL,
                          collar_key = NULL,
                          type = c(
                            "gps", "act", "mit", "mor", "prx", "sep", "trap",
                            "vit"
                          ),
                          count = FALSE,
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
    assertthat::is.string(collar_id),
    msg = "In build_vec_url, collar_id must be a length one character vector, did you pass more than one collar_id?"
  )
  assertthat::assert_that(
    assertthat::is.string(collar_key),
    msg = "In build_vec_url, collar_key must be a length one character vector, did you pass more than one collar_key?"
  )
  assertthat::assert_that(
    type %in% c("gps", "act", "mit", "mor", "prx", "sep", "trap", "vit"),
    msg = paste(
      "Function build_vec_url called with type",
      paste0(type, ","),
      "but type must be one of gps, act, mit, mor, prx, sep, trap, vit"
    )
  )
  assertthat::assert_that(
    assertthat::is.flag(count),
    msg = paste(
      "Function build_vec_url called with count set to",
      paste0(count, ","),
      "but count must be TRUE or FALSE"
    )
  )
  if(!is.null(start_date)){
    assertthat::assert_that(
      assertthat::is.string(start_date),
      msg = "In build_vec_url, start_date must be a length one character vector, did you pass more than one start_date?"
    )
    assertthat::assert_that(
      grepl(
        "^(19[78][0-9]|199[0-9]|20[0-9]{2}|2100)-0*([1-9]|1[0-2])-0*([1-9]|[12][0-9]|3[01])(T)0*([0-9]|1[0-9]|2[0-4]):0*([0-9]|[1-5][0-9]|60):0*([0-9]|[1-5][0-9]|60)$",
        start_date
      ),
      msg = "In build_vec_url, the format of start_date must follow YYYY-MM-DDTHH:MM:SS, don't forget the T in the middle"
    )
    assertthat::assert_that(
      which_date %in% c("scts", "acquisition"),
      msg = paste(
        "Function build_vec_url called with which_date set to",
        paste0(which_date, ","),
        "but which_date must be one of scts or acquisition."
      )
    )
  }

  # Insert base url if NULL to save typing
  if (is.null(base_url)) {
    base_url <- "https://wombat.vectronic-wildlife.com:9443/"
  }

  # Modify url conditional on count variable to simplify user interaction
  if(count){
    url <- httr::modify_url(
      base_url,
      path = list(
        "v2",
        "collar",
        collar_id,
        type,
        "count"
      ),
      query = list(
        "collarkey" = collar_key
      )
    )
  }else{
    url <- httr::modify_url(
      base_url,
      path = list(
        "v2",
        "collar",
        collar_id,
        type
      ),
      query = list(
        "collarkey" = collar_key
      )
    )
  }

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
                             "sep", "trap", "vit"
                           ),
                           count = FALSE,
                           after_data_id = NULL,
                           start_date = NULL,
                           which_date = c("scts", "acquisition")) {
  assertthat::assert_that(
    length(collar_id) == length(collar_key),
    msg = "In build_vec_urls, collar_id and collar_key must be the same length"
  )

  purrr::map2_chr(
    collar_id, collar_key,
    ~build_vec_url(
      base_url,
      collar_id = .x,
      collar_key = .y,
      type,
      count,
      after_data_id,
      start_date,
      which_date
    )
  )
}

#' A basic call to the Vectronics API
#'
#' @param url a character string representing the url to query
#' @inheritParams fetch_ats
#'
#' @keywords internal
#' @return tibble
call_vec_api <- function(url, rename_fun = adj_col_nms) {
  assertthat::assert_that(
    RCurl::url.exists("www.google.com"),
    msg = "Are you connected to the internet? You must have an internet connection to call the API"
  )

  st_time <- Sys.time()

  message("Downloading Vectronics collar data...")

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

  if(grepl("count?collarkey=", url[1], fixed = TRUE)){
    parse <- purrr::map2_dfr(
      resp, url,
      ~{
        pb$tick()$print()
        tibble::tibble(
          id = gsub("^.*(v2/collar/)([0-9]*)(/).*$", "\\2", .y),
          type = gsub("^.*(v2/collar/.*/)([A-z]*)(/).*$", "\\2", .y),
          count = httr::content(.x)
        )
      }
    )
  }else{
    parse <- purrr::map_dfr(
        resp,
        ~{
          pb$tick()$print()
          jsonlite::fromJSON(
            httr::content(.x, "text"),
            simplifyVector = TRUE
          )
        }
      ) %>%
      tibble::as.tibble() %>%
      dplyr::rename_all(rename_fun)
  }

  difftime(Sys.time(), st_time, units = "mins") %>%
    as.numeric() %>%
    round(2) %>%
    message("Done. Elapsed time: ", ., " minutes")
  return(parse)
}

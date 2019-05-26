#' Read collar data stored in CSV file(s)
#'
#' @param file_path The full path to one or more csv(s) to read
#' @param skip integer, the number of rows to skip at the top of the
#' file, see details
#' @param ... Other parameters to pass to readr::read_csv
#'
#' @seealso cllr_remove_header
#'
#' @return a tibble
#'
#' @export
#'
#' @examples
#' #  Define file path
#' fpath <- system.file(
#'   "extdata",
#'   package = "collar",
#'   mustWork = TRUE
#' )
#'
#' # List files
#' fls <- list.files(fpath, full.names = T, pattern = "csv$")
#'
#' #  Read file
#' ltk <- fetch_csv(fpath[1])
#'
#' # Read first two files
#' all_ltk <- fetch_csv(fpath[1:2])
#'
#' # You might get into trouble if too many formats are contained in the same directory
#' all_fls <- fetch_csv(fpath[1:5])
#'
#' #  Example with large header
#' fpath <- system.file(
#' "extdata",
#' "telonics.csv",
#' package = "collar",
#' mustWork = TRUE
#' )
#'
#' #  Read in whole data, use it to find first row in next step
#' r1 <- fetch_csv(fpath[4])
#'
#' #  Read in Telonics data skipping header
#' tlncs <- fetch_csv(fpath[4], skip = which(r1[,1] == "Acquisition Time"))
#'
#' # However, a function is included in this package to help with that issue
#' tlncs2 <- fetch_csv(fpath[4])
#' cllr_remove_header(tlncs2, lat_col = "Acquisition Time")
#'
fetch_csv <- function(file_path, skip = 0, ...) {
  assertthat::assert_that(length(file_path) >= 1)
  assertthat::assert_that(all(file.exists(file_path)))
  assertthat::assert_that(all(purrr::map_lgl(fls, assertthat::is.readable)))

  suppressMessages(
    purrr::map_dfr(x, readr::read_csv, skip = skip, ...) %>%
      dplyr::rename_all(tolower) %>%
      dplyr::rename_all(list(~ gsub("\\s", "", .)))
  )

}

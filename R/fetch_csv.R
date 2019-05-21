#' Read collar data stored in CSV file(s)
#'
#' @param file_path The full path to one csv file to read
#' @param skip Scalar integer, the number of rows to skip at the top of the
#' file, see details
#' @param ... Other parameters to pass to readr::read_csv
#'
#' @return a tibble with columns collar_id, date_time, x, y plus any additional
#' columns requested via aux_cols
#' @export
#'
#' @examples
#' #  Define file path
#' fpath <- system.file(
#'   "extdata",
#'   "lotek_2.csv",
#'   package = "collar",
#'   mustWork = TRUE
#' )
#'
#' #  Read file
#' ltk <- readr::read_csv(fpath)
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
#' r1 <- readr::read_csv(fpath)
#'
#' #  Read in Telonics data skipping header
#' tlncs <- readr::read_csv(fpath, skip = which(r1[,1] == "Acquisition Time"))
#'
fetch_csv <- function(file_path, skip = 0, ...) {
  assertthat::assert_that(length(file_path) == 1)
  assertthat::assert_that(all(file.exists(file_path)))
  assertthat::assert_that(assertthat::is.readable(file_path))

  readr::read_csv(file_path, skip = skip, ...) %>%
    dplyr::rename_all(tolower)
}

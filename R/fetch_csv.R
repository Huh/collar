#' Read collar data stored in CSV file(s)
#'
#' @param file_path The full path to one or more csv(s) to read
#' @param skip integer, the number of rows to skip at the top of the
#' file, see details
#' @param delim the delimiter to apply to the file, see ?readr::read_delim
#' @param rename_fun a function used to rename columns, see example
#' @param ... Other parameters to pass to readr::read_csv
#'
#' @details
#' Column names are adjusted using a custom function, but the user can pass any function they want to manipulate column names (e.g. make.names). The default removes non-ASCII characters, coerces all characters to lower case and replaces "." with "_".
#'
#' @seealso cllr_remove_header
#'
#' @return a tibble
#'
#' @export
#'
#' @examples
#' #  Define file path
#' dir_path <- system.file(
#'   "extdata",
#'   package = "collar",
#'   mustWork = TRUE
#' )
#'
#' # List files
#' fls <- list.files(dir_path, full.names = TRUE, pattern = "csv$")
#'
#' #  Read file
#' ltk <- fetch_csv(fls[1])
#'
#' # Read first two files
#' all_ltk <- fetch_csv(fls[1:2])
#'
#' # You might get into trouble if too many formats are contained in the same directory,
#' #  this will fail because column types are different. You could specify col_types
#' #  argument and pass it to readr::read_csv, but because different manufacturers use
#' #  different column names this won't work (col_types is not vectorized).
#' \dontrun{
#' all_fls <- fetch_csv(fls)
#' }
#'
#' #  Example with large header, common with companies like Telonics
#' #  Read in data with header and use it to find first row in next step
#' r1 <- fetch_csv(fls[4])
#'
#' # At this point you might print the first n rows of r1
#' # Then find where the data start or do the following to skip lines
#' tlncs <- fetch_csv(fls[4], skip = which(r1[,1] == "Acquisition Time"))
#'
#' # However, a function is included in this package to help with this issue
#' # Beware that in some cases the algorithm that finds the first row of data
#' #  may be overly simplistic
#' cllr_remove_header(r1, col_nm = `Acquisition Time`)
#'
#' # Note that you can pass arguments to readr::read_*
#' # If reading a tab delimited file something like this might work
#' \dontrun{
#' fetch_delim(some_file_name, delim = ",\t", skip = 0, na = c("N/A"))
#' }
#'
#' # The argument rename_fun can take custom functions for renaming
#' # Default custom function attempts to remove non-ASCII characters
#' colnames(fetch_csv(fls[1]))
#'
#' # Simply make every name lowercase
#' colnames(fetch_csv(fls[1], rename_fun = tolower))
#'
#' # Use the make.names function
#' colnames(fetch_csv(fls[1], rename_fun = make.names))
#'
#'
#'
fetch_csv <- function(file_path, skip = 0, rename_fun = collar:::adj_col_nms, ...) {
  assertthat::assert_that(
    length(file_path) >= 1,
    msg = "file_path argument is empty, please provide a valid path to a file"
  )
  assertthat::assert_that(
    all(file.exists(file_path)),
    msg = "One or more files described in file_path do not exist"
  )
  assertthat::assert_that(
    all(purrr::map_lgl(file_path, assertthat::is.readable)),
    msg = "One or more files described in file_path are not readable, could this be a permissions issue?"
  )

  suppressMessages(
    purrr::map_dfr(file_path, readr::read_csv, skip = skip, ...) %>%
      dplyr::rename_all(rename_fun)
  )

}

#' @export
#' @rdname fetch_csv
fetch_delim <- function(file_path,
                        delim = ",",
                        skip = 0,
                        rename_fun = collar:::adj_col_nms,
                        ...) {
  assertthat::assert_that(
    length(file_path) >= 1,
    msg = "file_path argument is empty, please provide a valid path to a file"
  )
  assertthat::assert_that(
    all(file.exists(file_path)),
    msg = "One or more files described in file_path do not exist"
  )
  assertthat::assert_that(
    all(purrr::map_lgl(file_path, assertthat::is.readable)),
    msg = "One or more files described in file_path are not readable, could this be a permissions issue?"
  )

  suppressMessages(
    purrr::map_dfr(
      file_path,
      readr::read_delim,
      delim = delim,
      skip = skip,
      ...
    ) %>%
    dplyr::rename_all(rename_fun)
  )
}

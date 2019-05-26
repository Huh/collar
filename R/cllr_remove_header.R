#' A function to find and remove data headers form collar data based on the latitude column
#'
#' @param x A dataframe containing collar data
#' @param col_nm The unqouted name of any one column in the data frame
#' @param rm_header Logical indicating whether to remove header completely, defaults to TRUE
#'
#' @details
#' The argument rm_header is meant to aid in programming scenarios where some files should be left alone and others need the header removed. We are not sure this is actually useful, but thought of the scenario and accommodated it just the same.
#'
#' @export
#' @return tibble
#'
#' @examples
#' ugly_file <- read.csv(
#'   system.file("extdata", "telonics.csv", package = "collar")
#' )
#'
#' # The column name needs to be unquoted (Column, not "Column")
#' # If spaces exist in the column name you may need to use backticks
#' collar::cllr_remove_header(ugly_file, `GPS Latitude`, rm_header = TRUE)
cllr_remove_header <- function(x, col_nm, rm_header = TRUE) {
  col_name <- rlang::as_name(rlang::enquo(col_nm))

  if (!rm_header) {
    return(x)
  } else {
    # Find column containing 'lat_col'
    col_loc <- x %>%
      dplyr::summarize_all(list(~any(. == col_name))) %>%
      dplyr::select(which(as.logical(.))) %>%
      colnames(.)

    # Find row with column names
    row_loc <- x %>%
      dplyr::slice(1:100) %>%
      dplyr::select(col_loc) %>%
      dplyr::summarise(data_start = which(.[, 1] == col_name)) %>%
      dplyr::pull(.)

    # Remove header and rename columns
    x %>%
      magrittr::set_colnames(
        .,
        as.character(unlist(dplyr::slice(., row_loc)))
      ) %>%
      dplyr::slice((row_loc + 1):nrow(x))
  }
}

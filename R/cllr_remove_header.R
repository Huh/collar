#' A function to find and remove data headers form collar data based on the latitude column
#'
#' @param x A dataframe containing collar data
#' @param lat_col The unqouted name of the latitude or any other column in the data frame
#' @param rm_header Logical indicating whether to remove header completely
#'
#' @export
#' @return tibble
#'
#' @examples
#' ugly_file <- read.csv(system.file("extdata", "telonics.csv", package = "collar"))
#' # Use backticks for column names with spaces
#' collar::cllr_remove_header(ugly_file, `GPS Latitude`, rm_header = TRUE)
cllr_remove_header <- function(x, lat_col, rm_header) {
  latc <- rlang::enquo(lat_col)
  lat_col_name <- rlang::as_name(latc)

  if (!rm_header) {
    return(x)
  } else {
    # Find column containing 'lat_col'
    col_loc <- x %>%
      dplyr::summarize_all(list(~any(. == lat_col_name))) %>%
      dplyr::select(which(as.logical(.))) %>%
      colnames(.)

    # Find row with column names
    row_loc <- x %>%
      dplyr::slice(1:100) %>%
      dplyr::select(col_loc) %>%
      dplyr::summarise(data_start = which(.[, 1] == lat_col_name)) %>%
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

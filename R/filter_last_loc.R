#' Find the last location for each individual
#'
#' @param x A dataframe containing collar data.
#' @param id A character vector corresponding the the column to be used as a grouping variable.
#'
#' @return A dataframe with the last location for each individual.
#' @export
#'
#' @examples
#' \dontrun{
#'   filter_last_loc(collar_data)
#' }
filter_last_loc <- function(x,
                            id = "id"
) {

  assertthat::assert_that(id %in%  names(x), msg = paste(id, "is not a column name in the data frame supplied"))

  x %>%
  dplyr::group_by(id) %>%
  dplyr::slice(dplyr::n()) %>%
  dplyr::ungroup()
}

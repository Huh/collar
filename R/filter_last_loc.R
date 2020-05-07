#' Select the last location for each individual
#'
#' @param x A data frame containing collar data.
#' @param ... unquoted column name(s) to be used as a grouping variable e.g. if "id" is used, a single row will be returned for each id.
#' @param order_by A character vector of the column(s) to be used to arrange the data e.g. if the date/time column is used rows will be ordered chronologically.
#'
#' @return A data frame with the last location for each individual.
#' @export
#'
#' @examples
#' \dontrun{
#'   filter_last_loc(collar_data, ... = id, order_by = dt)
#' }
filter_last_loc <- function(x,
                            ...,
                            order_by

) {
  if(!missing(order_by)) x <- dplyr::arrange(x, {{ order_by }})

  x %>%
    dplyr::group_by( ... ) %>%
    dplyr::slice(dplyr::n()) %>%
    dplyr::ungroup()
}

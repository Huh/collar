#' Find the last location for each individual
#'
#' @param x A dataframe containing collar data.
#' @param grps A character vector of the column(s) to be used as a grouping variable e.g. if "id" is used, a single row will be returned for each id.
#' @param order_by A character vector of the column(s) to be used to arrange the data e.g. if the date/time column is used rows will be ordered chronologically.
#'
#' @return A dataframe with the last location for each individual.
#' @export
#'
#' @examples
#' \dontrun{
#'   collar_data %>%
#'   filter_last_loc()
#' }
filter_last_loc <- function(x,
                            grps = "id",
                            order_by = NULL
) {

  assertthat::assert_that(!is.null(grps), msg = "grps can not be NULL")
  assertthat::assert_that(assertthat::noNA(grps))
  purrr::map(grps, ~assertthat::assert_that(assertthat::has_name(x, .)))
  if(!is.null(order_by)) {purrr::map(order_by, ~assertthat::assert_that(assertthat::has_name(x, .)))}

  if(!is.null(order_by)) {
    order_by <- purrr::map(grps, ~parse_quo(., env = parent.frame()))
    x <- dplyr::arrange(x, !!!order_by)
  }

  grps <- purrr::map(grps, ~parse_quo(., env = parent.frame()))

  x %>%
  dplyr::group_by(!!!grps) %>%
  dplyr::slice(dplyr::n()) %>%
  dplyr::ungroup()
}

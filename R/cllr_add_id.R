#' @title Add id column
#'
#' @description If your data does not have unique identifiers for each animal cllr_add_id will create that column and insert values you provide.
#'
#' @inheritParams cllr_rename_id
#' @param id_vals values (character) to include in a newly created id column, these are the unique identifier for each individual
#'
#' @details
#' When adding a new column to data using cllr_add_id the user should supply either a length 1 character string or a character vector equal in length to the number of observations in the data. The latter is not recommended.
#'
#' @return data.frame
#' @seealso cllr_rename_id
#' @export
#'
#' @examples
#' # Add unique identifiers
#' df <- data.frame(val = 1:3)
#' cllr_add_id(df, "1A")
cllr_add_id <- function(x, id_vals) {
  assertthat::assert_that(inherits(x, "data.frame"))
  assertthat::assert_that(inherits(id_vals, "character"))

  out <- x %>%
    dplyr::mutate(
      id = as.character(id_vals)
    )
}

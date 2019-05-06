#' @title Rename id column
#'
#' @description The purpose of cllr_rename_id is to provide a simple utility to change a column name to conform to this package's expectation that individual identifiers are stored in a column called id. If your data does not have unique identifiers for each animal cllr_add_id will create that column and insert provided values.
#'
#' @param x object inherting from data.frame
#' @param id_col unquoted name of column containing unique identifier for each individual
#'
#' @return data.frame
#' @seealso cllr_add_id
#' @export
#'
#' @examples
#' # Unquoted input when renaming id column
#' df <- data.frame(val = 1:3, animal_name = 1)
#' cllr_rename_id(df, animal_name)
cllr_rename_id <- function(x, id_col){
  assertthat::assert_that(inherits(x, "data.frame"))
  is_unquo <- function(x) {
    class(try(class(x), silent = T)) == "try-error"
  }

  assertthat::on_failure(is_unquo) <- function(call, env) {
    paste0("Column ", deparse(call$x), " must be unquoted and match a column name. If special characters or spaces exist use back ticks (`A B`).")
  }

  assertthat::assert_that(is_unquo(id_col))

  idc <- rlang::enquo(id_col)

  out <- x %>%
    dplyr::rename(
      id = !!idc
    )
}

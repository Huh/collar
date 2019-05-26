#' A function to find and remove data headers form collar data based on the latitude column
#'
#' @param x A dataframe containing collar data
#' @param col_nm The unqouted name of any one column in the data frame
#' @param rm_header Logical indicating whether to remove header completely, defaults to TRUE
#' @inheritParams fetch_ats
#'
#' @details
#' The argument rm_header is meant to aid in programming scenarios where some files should be left alone and others need the header removed. We are not sure this is actually useful, but thought of the scenario and accommodated it just the same.
#'
#' Column names are adjusted using a custom function, but the user can pass any function they want to manipulate column names (e.g. make.names). The default removes non-ASCII characters, coerces all characters to lower case and replaces "." with "_".
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
#' # If spaces exist in the column name you may need to use backticks, top left
#' #  on most(?) qwerty keyboards
#' collar::cllr_remove_header(ugly_file, `GPS Latitude`, rm_header = TRUE)
cllr_remove_header <- function(x,
                               col_nm,
                               rm_header = TRUE,
                               rename_fun = adj_col_nms) {
  is_unquo <- function(x) {
    class(try(class(x), silent = T)) == "try-error"
  }
  assertthat::on_failure(is_unquo) <- function(call, env) {
    paste0("Column ", deparse(call$x), " must be unquoted. If special characters or spaces exist use back ticks (`A B`).")
  }
  assertthat::assert_that(is_unquo(col_nm))
  assertthat::assert_that(
    assertthat::is.flag(rm_header),
    msg = paste(
      "Function cllr_remove_header called with rm_header set to",
      paste0(rm_header, ","),
      "but rm_header must be TRUE or FALSE"
    )
  )
  assertthat::assert_that(
    class(rename_fun) == "function",
    msg = paste(
      "In cllr_remove_header rename_fun must be a function, received",
      class(rename_fun)
    )
  )

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
      dplyr::slice((row_loc + 1):nrow(x)) %>%
      dplyr::rename_all(list(~ rename_fun))
  }
}

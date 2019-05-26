#' Return locations within the specified date range
#'
#' @param x A dataframe containing collar data with column "dt_col" that can be coerced into class "Date"
#' @param date_col A character vector that specifies the name of date column in the supplied dataframe.
#' @param start A character, date, or POSIXct string that can be coerced into a date.  Used to return locations after the specified date.
#' @param end A character, date, or POSIXct string that can be coerced into a date.  Used to return locations before the specified date.
#'
#' @return A dataframe with locations recorded between the start date and end date provided.
#' @export
#'
#' @examples
#' \dontrun{
#'   filter_date_range(collar_data, start = "2000-12-30", end = "2019-01-30")
#' }

filter_date_range <- function(x,
                              dt_col = "dt",
                              start = "1900-01-01",
                              end = (Sys.Date() + 1)) {

  assertthat::assert_that(dt_col %in% names(x), msg = paste(dt_col, "is not a column name in the data frame supplied"))

  dplyr::filter(
    x,
    as.Date(!!as.name(dt_col)) >= as.Date(start),
    as.Date(!!as.name(dt_col)) <= as.Date(end)
  )
}

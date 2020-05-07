#' Return locations within the specified date range
#'
#' @param x A data frame containing collar data with column "dt_col" that can be coerced into class "Date"
#' @param dt_col A character vector that specifies the name of date column in the supplied data frame.
#' @param start A character, date, or POSIXct string that can be coerced into a date.  Used to return locations on or after the specified date.
#' @param end A character, date, or POSIXct string that can be coerced into a date.  Used to return locations on or before the specified date.
#'
#' @return A data frame with locations recorded between the start date and end date provided.
#' @export
#'
#' @examples
#' \dontrun{
#'   filter_date_range(collar_data, start = "2000-12-30", end = "2019-01-30")
#' }

filter_date_range <- function(x,
                              dt_col = dt,
                              start,
                              end){

  assertthat::assert_that(inherits(try(as.Date(dplyr::pull(x, {{ dt_col }})), silent = T), "Date"), msg = paste(dt_col, "column does not have a class of date and can not be coerced using as.Date()"))
  if(!missing(start)) assertthat::assert_that(inherits(try(as.Date(start), silent = T), "Date"), msg = "'start' does not have a class of date and can not be coerced using as.Date()")
  if(!missing(end)) assertthat::assert_that(inherits(try(as.Date(end), silent = T), "Date"), msg = "'end' does not have a class of date and can not be coerced using as.Date()")

  if(missing(start)) {start = "1900-01-01"}
  if(missing(end))   {end = Sys.Date()}

  dplyr::filter(
    x,
    as.Date({{ dt_col }}) >= as.Date(start),
    as.Date({{ dt_col }}) <= as.Date(end)
  )
}

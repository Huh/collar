#' Save collar locations to disk as a GPX file
#'
#' @param x a data frame containing collar data with latitude and longitude location data
#' @param file the file path where the output GPX file should be saved
#' @param lon unquoted name of column containing longitude coordinates
#' @param lat unquoted name of column containing longitude coordinates
#' @param date_time unquoted name of column containing date and time of location. The format of date_time must follow YYYY-MM-DD HH:MM:SS or YYYY-MM-DDTHH:MM:SS.
#' @param name unquoted name of column containing the name to be assigned to the GPS location
#' @param desc unquoted name of column containing a a description to be assigned to the GPS location
#'
#' @return The original data frame passed to the function.
#' @export
#'
#' @examples
#' \dontrun{
#'   make_gpx(collar_data)
#' }
make_gpx <- function(x,
                       file,
                       lon = lon,
                       lat = lat,
                       date_time,
                       name,
                       desc
){

  assertthat::assert_that(assertthat::is.dir(dirname(file)))

  lat <- dplyr::pull(x, {{ lat }})
  lon <- dplyr::pull(x, {{ lon }})

  assertthat::assert_that(assertthat::noNA(lon))
  assertthat::assert_that(assertthat::noNA(lat))

  if(missing(date_time)) {dt = ""} else {dt = dplyr::pull(x, {{ date_time }})}
  if(missing(name)) {nm = ""} else {nm = dplyr::pull(x, {{ name }})}
  if(missing(desc)) {dsc = ""} else {dsc = dplyr::pull(x, {{ desc }})}


  if(!identical(dt, "")){
    assertthat::assert_that(
      sum(grepl(
        "^(19[78][0-9]|199[0-9]|20[0-9]{2}|2100)-0*([1-9]|1[0-2])-0*([1-9]|[12][0-9]|3[01])(T| )0*([0-9]|1[0-9]|2[0-4]):0*([0-9]|[1-5][0-9]|60):0*([0-9]|[1-5][0-9]|60)$",
        dt
      )) == length(dt),
      msg = "In create_gpx, the format of date_time must follow YYYY-MM-DD HH:MM:SS or YYYY-MM-DDTHH:MM:SS."
    )
  }

  pre <- '<?xml version="1.0" encoding="utf-8" standalone="yes"?> <gpx version="1.1" creator="Collar R pacakge https://github.com/Huh/collar" xmlns="http://www.topografix.com/GPX/1/1" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.topografix.com/GPX/1/1 http://www.topografix.com/GPX/1/1/gpx.xsd">'
  post <- '</gpx>'

  glue::glue(
    '<wpt lat="{lat}" lon="{lon}">
      <time>{dt}</time>
      <name>{nm}</name>
      <desc>{dsc}</desc>
    </wpt>'
  ) %>%
    c(pre, ., post) %>%
    paste(collapse = "\n") %>%
    writeLines(file)

  return(invisible(x))
}

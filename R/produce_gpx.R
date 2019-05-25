#' Save collar locations to disk as a GPX file
#'
#' @param id_col Quouted name of the ID column that signals the individual identifier.
#' @param lat_col Quouted name of the latitude column.
#' @param lon_colQuouted name of the longitude column.
#' @param dt_col Quouted name of the date/time column with class POSIXct.
#' @param x A dataframe containing collar data with column "dt" that can be coerced into class "Date"
#' @param dir The directory to save the GPX file output. Default is here::here().
#' @param file_name The name of the GPX file created with this function.
#' @param crs The coordiante refernce system associated with the collar data in the supplied data frame.
#' @param ... Additional arguments to pass to sf::st_write()
#'
#' @return The original data frame passed to the function.
#' @export
#'
#' @examples
#' \dontrun{
#'   produce_gpx(collar_data, start = Sys.date() - 30)
#' }
produce_gpx <- function(x,
                        id_col = "id",
                        lat_col = "lat",
                        lon_col = "lon",
                        dt_col = "dt",
                        dir = here::here(),
                        file_name = paste0("GPS Locations ", format(Sys.time(), "%Y-%m-%d %H%M%S"), ".gpx"),
                        crs = 4326,
                        ...
){
  assertthat::assert_that(assertthat::noNA(x$lon))
  assertthat::assert_that(assertthat::noNA(x$lat))
  assertthat::assert_that(assertthat::is.dir(dir))
  assertthat::assert_that(lat_col %in%  names(x), msg = paste(lat, "is not a column name in the data frame supplied"))
  assertthat::assert_that(lon_col %in%  names(x), msg = paste(lon, "is not a column name in the data frame supplied"))
  assertthat::assert_that(dt_col %in%  names(x), msg = paste(lon, "is not a column name in the data frame supplied"))

  x %>%
  dplyr::mutate(name = paste(x[, id_col], format(as.data.frame(x[, dt_col]), "%Y-%m-%d h%H"))) %>%
  sf::st_as_sf(coords = c(lon_col, lat_col), crs = crs) %>%
  dplyr::select(name) %>%
  sf::st_write(dsn = paste(dir, file_name, sep = "/"), layer = "waypoints", driver = "GPX", ...)

  return(invisible(x))
}

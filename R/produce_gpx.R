#' Save collar locations to disk as a GPX file
#'
#' @param x A dataframe containing collar data with column "dt" that can be coerced into class "Date"
#' @param lat A character string that specifies the name of latitude column in the supplied dataframe.
#' @param lon A character string that specifies the name of longitude column in the supplied dataframe.
#' @param dir The directory to save the GPX file output. Default is the current working directory.
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
                        lat = "lat",
                        lon = "lon",
                        dir = here::here(),
                        file_name = paste0("GPS Locations ", format(Sys.time(), "%Y-%m-%d %H%M%S"), ".gpx"),
                        crs = 4326,
                        ...
){
  assertthat::assert_that(assertthat::noNA(x$lon))
  assertthat::assert_that(assertthat::noNA(x$lat))
  assertthat::assert_that(assertthat::is.dir(dir))
  assertthat::assert_that(lat %in%  names(x), msg = paste(lat, "is not a column name in the data frame supplied"))
  assertthat::assert_that(lon %in%  names(x), msg = paste(lon, "is not a column name in the data frame supplied"))
  assertthat::assert_that(dt %in%  names(x), msg = paste(lon, "is not a column name in the data frame supplied"))

  x %>%
  dplyr::mutate(name = paste(.$id, format(.$dt, "%Y-%m-%d h%H"))) %>%
  sf::st_as_sf(coords = c(lon, lat), crs = crs) %>%
  dplyr::select(name) %>%
  sf::st_write(dsn = paste(dir, file_name, sep = "/"), layer = "waypoints", driver = "GPX", ...)

  return(invisible(x))
}

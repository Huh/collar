#' Save collar locations to disk as a GPX file
#'
#' @param id_col Quouted name of the ID column that signals the individual identifier.
#' @param lat_col Quouted name of the latitude column.
#' @param lon_col Quouted name of the longitude column.
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
#'   make_gpx(collar_data, start = Sys.date() - 30)
#' }
make_gpx <- function(x,
                     id_col = "id",
                     lat_col = "lat",
                     lon_col = "lon",
                     dt_col = "dt",
                     file = paste0("./GPS Locations ", format(Sys.time(), "%Y-%m-%d %H%M%S"), ".gpx"),
                     crs = 4326,
                     ...
){
  assertthat::assert_that(assertthat::noNA(dplyr::pull(x, lon_col)))
  assertthat::assert_that(assertthat::noNA(dplyr::pull(x, lat_col)))
  purrr::map(
    c(id_col, lat_col, lon_col, dt_col),
    ~assertthat::assert_that(assertthat::has_name(x, .))
  )


  x %>%
  dplyr::mutate(name = paste(x[, id_col], format(as.data.frame(x[, dt_col]), "%Y-%m-%d h%H"))) %>%
  sf::st_as_sf(coords = c(lon_col, lat_col), crs = crs) %>%
  dplyr::select(name) %>%
  sf::st_write(dsn = file, layer = "waypoints", driver = "GPX", ...)

  return(invisible(x))
}

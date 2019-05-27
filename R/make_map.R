#' Create an interactive html map of animal locations.
#'
#' @param x A data frame containing collar data with the columns "id", "lat", "lon" and  "date_time".
#' @param id_col Quoted name of the ID column that signals the individual identifier.
#' @param lat_col Quoted name of the latitude column.
#' @param lon_col Quoted name of the longitude column.
#' @param dt_col Quoted name of the date/time column with class POSIXct.
#'
#' @return A leaflet map.
#' @export
#'
#' @examples
#' \dontrun{
#'   collardata %>%
#'   filter_last_loc() %>%
#'   make_map()
#' }
make_map <- function(x,
                     id_col = "id",
                     lon_col = "lon",
                     lat_col = "lat",
                     dt_col = "dt"
                     ) {

  assertthat::assert_that(inherits(x, "data.frame"))
  purrr::map(
    c(lat_col, lon_col, id_col, dt_col),
    ~assertthat::assert_that(assertthat::has_name(x, .))
  )

  assertthat::assert_that(assertthat::noNA(x[, lon_col]))
  assertthat::assert_that(assertthat::noNA(x[, lat_col]))

  if(interactive()){
    if (nrow(x) > 1000)
      if (!isTRUE(
        askYesNo(
          paste("Plotting", nrow(x), "locations can take a long time.  Do you wish to continue?"),
          default = F,
          prompts = getOption("askYesNo", gettext(c("Yes", "No", "Cancel")))
        )))
        stop("Canceled by user.")
  }

  content <- paste(
    sep = "",
    "<b>ID:</b> ", dplyr::pull(x, id_col), "<br/>",
    "<b>Date/time:</b> ", dplyr::pull(x, dt_col), "<br/>",
    "<b>GPS:</b> ", paste(round(dplyr::pull(x, lat_col), 6), round(dplyr::pull(x, lon_col), 6))
  )

  x_sf <- sf::st_as_sf(x, coords = c(lon_col, lat_col), crs = 4326)

  out <-
    leaflet::leaflet("map") %>%
    leaflet::addProviderTiles(leaflet::providers$Esri.WorldTopoMap, group = "Topo") %>%
    leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "World Imagery") %>%
    leaflet::addLayersControl(
      baseGroups = c("Topo", "World Imagery"),
      options = leaflet::layersControlOptions(collapsed = FALSE)
    ) %>%
    leaflet::addScaleBar(position = "bottomright") %>%
    leaflet::addMarkers(
      data = x_sf,
      popup = content
    )

  return(out)
}

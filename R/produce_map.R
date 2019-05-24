#' Create an interactive html based map of animal locations.
#'
#' @param x A dataframe containing collar data with the columns "id", "lat", "lon" and  "date_time".
#' @param save_html Logical, whether or not the map should be saved to disk.
#' @param file Used only if save_html = T. The direcotry and file name of the output html map file.
#'
#' @return A leaflet map.
#' @export
#'
#' @examples
#' \dontrun{
#'   collardata %>%
#'   filter_last_loc() %>%
#'   produce_map()
#' }
produce_map <- function(x,
                        save_html = F,
                        file = "map.html"
) {
  assertthat::assert_that("lat" %in%  names(x), msg = "A latitude column named 'lat' is required")
  assertthat::assert_that("lon" %in%  names(x), msg = "A longitude column named 'lon' is required")
  assertthat::assert_that("id" %in%  names(x), msg = "An id column named 'id' is required")
  assertthat::assert_that("dt" %in%  names(x), msg = "A date time column named 'dt' is required")
  assertthat::assert_that(assertthat::noNA(x$lon))
  assertthat::assert_that(assertthat::noNA(x$lat))

  if(interactive())
    if (n > 1000)
      if (!isTRUE(
        askYesNo(
          paste("Plotting", n, "locations can take a long time.  Do you wish to continue?"),
          default = F,
          prompts = getOption("askYesNo", gettext(c("Yes", "No", "Cancel")))
        )))
        stop("Canceled by user.")

  content <- paste(
    sep = "",
    "<b>ID:</b> ", x$id, "<br/>",
    "<b>Date/time:</b> ", x$dt, "<br/>",
    "<b>GPS:</b> ", paste(round(x$lat, 6), round(x$lon, 6))
  )

  x_sf <-
    sf::st_as_sf(x, coords = c("lon", "lat"), crs = 4326)

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

  if(save_html == T){htmlwidgets::saveWidget(out, file=file)}

  return(out)
}

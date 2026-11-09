#' Create an interactive html map of animal locations.
#'
#' @param x A data frame containing collar data with latitude and longitude
#'   columns
#' @param lat_col name of the latitude column
#' @param lon_col name of the longitude column
#' @param id_col name of the ID column that signals the individual identifier
#' @param dt_col name of the date/time column
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
                     lon_col = lon,
                     lat_col = lat,
                     id_col,
                     dt_col) {

  assertthat::assert_that(inherits(x, "data.frame"))

  lon <- dplyr::pull(x, {{ lon_col }})
  lat <- dplyr::pull(x, {{ lat_col }})

  assertthat::assert_that(assertthat::noNA(lon))
  assertthat::assert_that(assertthat::noNA(lat))

  id <- if (missing(id_col)) "" else dplyr::pull(x, {{ id_col }})
  d_t <- if (missing(dt_col)) "" else dplyr::pull(x, {{ dt_col }})

  if (interactive() && nrow(x) > 1000) {
    proceed <- askYesNo(
      paste(
        "Plotting", nrow(x), "locations can take a long time.",
        " Do you wish to continue?"
      ),
      default = FALSE,
      prompts = getOption("askYesNo", gettext(c("Yes", "No", "Cancel")))
    )
    if (!isTRUE(proceed)) {
      stop("Canceled by user.")
    }
  }

  content <-
    glue::glue(
      "<b>ID:</b> {id}<br/>
      <b>Date/time:</b> {d_t} <br/>
      <b>GPS:</b> {paste(round(lat, 6), round(lon, 6))}"
    )

  x_sf <-
    tibble::tibble(
      lon = lon,
      lat = lat,
      id = id,
      dt = d_t
    ) %>%
    sf::st_as_sf(coords = 1:2, crs = 4326)

  out <-
    leaflet::leaflet("map") %>%
    leaflet::addProviderTiles(
      leaflet::providers$Esri.WorldTopoMap,
      group = "Topo"
    ) %>%
    leaflet::addProviderTiles(
      leaflet::providers$Esri.WorldImagery,
      group = "World Imagery"
    ) %>%
    leaflet::addLayersControl(
      baseGroups = c("Topo", "World Imagery"),
      options = leaflet::layersControlOptions(collapsed = FALSE)
    ) %>%
    leaflet::addScaleBar(position = "bottomright") %>%
    leaflet::addMarkers(
      data = x_sf,
      popup = content
    )

  out
}

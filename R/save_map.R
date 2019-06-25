#' Save interactive maps to html file
#'
#' @param x A leaflet class object e.g. as created with make_map.
#' @param file The file path where the output html file should be saved.
#'
#' @return The original object passed to the function unmodified.
#' @export
#'
#' @examples
#' \dontrun{
#'   collar_data %>%
#'   collar::make_map() %>%
#'   collar::save_map()
#' }

save_map <- function(
                     x,
                     file = "./map.html"
                     ) {

  assertthat::assert_that(inherits(x, "leaflet"))

  htmlwidgets::saveWidget(x, file = file)

  return(invisible(x))
}

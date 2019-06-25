#' @title Fill Scale Constructor for ggplot
#'
#' @description The purpose of style_scale_fill is to make our own colors!
#'              This includes the "duck hunt" color palette!
#'
#' @param palette Character name of palette in cllr_palettes
#' @param discrete Boolean; Is the color aesthetic discrete or not?
#' @param reverse Boolean; Should the colors be reversed?
#' @param ... Additional arguments which can be passed to ggplot2::disrete_scale()
#'            or ggplot2::scale_color_gradientn(), used when discrete is
#'            true or false, respectively
#'
#' @details There are four color schemes to choose from:
#'          1) "main" = simple colors, duckhunt; "NULL" value
#'          2) "duckhunt" = palette from duckhunt
#'          3) "cblind" = color blind palette
#'          4) "gray" = gray scale if you're boring
#'          If discrete = F, will pick gradient between discrete colors
#'
#' @return An object of class "ggproto", for use in ggplot2::ggplot
#' @export
#'
#' @examples
#' # Create a bar graph with continuous color palette, duckhunt
#' ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(manufacturer, fill = manufacturer)) +
#'   ggplot2::geom_bar() +
#'   ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
#'   style_scale_fill(palette = "duckhunt", guide = "none")
style_scale_fill <- function(palette = "main", discrete = T,
                             reverse = F, ...) {
  pal <- cllr_pal(palette = palette, reverse = reverse)

  if (discrete) {
    ggplot2::discrete_scale("fill", paste0("cllr_", palette),
      palette = pal, ...
    )
  } else {
    ggplot2::scale_fill_gradientn(colors = pal(256), ...)
  }
}

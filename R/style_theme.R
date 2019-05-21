#' @title Theme Layout for ggplot
#'
#' @description Create a simple ggplot theme for graphs
#'
#' @export
#' @importFrom ggplot2 %+replace%
#'
#' @examples
#' # Create a scatterplot with style!
#' ggplot2::ggplot(iris,
#'   ggplot2::aes(Sepal.Width, Sepal.Length, color = Species)) +
#'   ggplot2::geom_point(size = 5) +
#'   style_theme()

style_theme <- function() {
  ggplot2::theme_bw() %+replace%
    ggplot2::theme(
      panel.border = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      axis.title = ggplot2::element_text(size = 20),
      axis.text = ggplot2::element_text(size = 16, color = "black"),
      legend.text = ggplot2::element_text(size = 16, color = "black"),
      legend.title = ggplot2::element_text(size = 20, color = "black")
    )
}

########################################################################

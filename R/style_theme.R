#' @title Theme Layout for ggplot
#'
#' @description Create a simple ggplot theme for graphs
#'
#' @details We can change this later to accomodate opinions...

style_theme <- function(){
  theme_bw() %+replace%
    theme(
      panel.border = element_blank(), panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black"),
      axis.title = element_text(size=20),
      axis.text = element_text(size=16, color="black"),
      legend.text = element_text(size=16,color="black"),
      legend.title = element_text(size=20,color="black")
    )
}

########################################################################







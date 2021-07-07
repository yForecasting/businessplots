#' gauge chart function
#'
#' Creates a gauge chart of an integer or a numeric value.
#'
#' This function creates a gauge chart based on an integer or a numeric value.
#'
#' @param gauge_value An integer or a numeric value
#'
#' @author Ruben Vanhecke
#'
#' at import plotly
#'
#' @return A gauge chart
#' @export
#'
#' @examples
#'   \dontrun{
#'      gaugechart(25)
#'   }
#'
#'
#'

# plot gauge chart
gaugechart <- function(gauge_value){
  # gauge_value is an integer or a numeric value

  # library
  library(plotly)

  # plot gauge chart
  fig <- plot_ly(
    domain = list(x = c(0, 1), y = c(0, 1)),
    value = as.numeric(gsub(",", ".", gauge_value)),
    title = list(text = "Title"), # TODO: load from formatobject?
    type = "indicator", # TODO: load from formatobject
    mode = "gauge+number") # TODO: load from formatobject
  fig <- fig %>%
    layout(margin = list(l=20,r=30))

  fig
}





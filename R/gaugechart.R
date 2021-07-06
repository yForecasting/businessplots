#' generic businessplot function
#'
#' Creates a general plot of the data.
#'
#' This function creates a plot based on the format of the data
#' and this can be: scatterplot, lineplot or bar plot.
#'
#'
#' @param file A csv file with the source data
#'
#' @author Yves R. Sagaert
#'
#' at import forecast
#' at import Mcomp
#'
#' @return A graphical plot
#' @export
#'
#' @examples
#'   \dontrun{
#'      bp("testdata.csv")
#'   }
#'
#'
#'

# plot gauge chart (speedometer)
gaugechart <- function(gauge_value){
  # file is the file with all original data to read
  # main_column is the name of the column with the column names of the pie chart

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





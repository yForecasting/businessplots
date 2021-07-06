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

# plot pie chart
piechart <- function(file, main_column){
  # file is the file with all original data to read
  # main_column is the name of the column with the column names of the pie chart

  # read data
  data <- read.csv(file,header=TRUE,sep=';')
  if (substr(colnames(data)[1],2,3)== ".."){
    data <- read.csv(file,fileEncoding="UTF-8-BOM",header=TRUE,sep=';')
  }

  # set labels
  labels <- names(data)[names(data) != main_column]

  # set colors
  colors <- c("red","orange","yellow","blue","green") # TODO: read from formatobject

  # set main values
  main_values <- data[, main_column]

  # plot pie charts
  for (main_value in main_values){

    # set values for pie chart
    values <- data[data[main_column] == main_value][names(data) != main_column]

    # set x for pie chart
    x <- as.numeric(gsub(",", ".", values))

    # plot pie chart
    pie(x=x, labels=labels, col=colors)
  }
}





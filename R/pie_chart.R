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

pieChart <- function(file, main_column){
  df <- read.csv(file,header=TRUE,sep=';')
  if (substr(colnames(df)[1],2,3)== ".."){
    df <- read.csv(file,fileEncoding="UTF-8-BOM",header=TRUE,sep=';')
  }

  labels <- names(df)[names(df) != main_column]
  colors <- c("red","orange","yellow","blue","green")
  main_values <- df[, main_column]

  for (main_value in main_values){
    values <- df[df[main_column] == main_value][names(df) != main_column]
    x <- c(strtoi(values[c(1, 2, 3)]), 125)
    pie(x, labels = labels, col=colors)
  }
}





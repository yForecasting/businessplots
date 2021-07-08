#'  Scatter plot function
#'
#' Creates a scatter plot of the data.
#'
#' This function creates a plot based on the format of the data
#'
#'
#'
#' @param file A csv file with the source data
#' @param x_column Name of the column from the file you want to plot on the x-axis
#' @param y_column Name of the column from the file you want to plot on the y-axis
#' @param symbol number representing the symbol used for the scatter plot
#' @param auto_fit Put false to start from 0, True shows the best fit.
#' @param primary_color First color for bar plot
#' @param secondary_color Second color for bar plot
#'
#' @author Emiel Creus
#'
#
#'
#' @return A scatter plot
#' @export
#'
#' @examples
#'   \dontrun{
#'      scatter("testdata.csv", "column_1", "column2", 2, FALSE, "#004D9A", "#002142")
#'   }
#'
#'

scatter <- function(file, x_column, y_column, symbol, auto_fit = TRUE, primary_color, secondary_color){
  df <- read.csv(file,header=TRUE,sep=';')
  if (substr(colnames(df)[1],2,3)== ".."){
    df <- read.csv(file,fileEncoding="UTF-8-BOM",header=TRUE,sep=';')
  }
  if(auto_fit){
    ylim <- c(min(df[,y_column], na.rm=TRUE),max(df[,y_column], na.rm=TRUE))
  }
  else{
    ylim <- c(0,max(df[,y_column], na.rm=TRUE))
  }
  names <- names(df)
  basic_palette <- "Paired"

  # todo detect sep automatically later ;/,/tab
  plot(x=df[,x_column], y=df[,y_column], pch = symbol, col = primary_color,
       col.axis = secondary_color, col.lab = secondary_color,
       ylim = ylim,
    xlab=x_column , ylab = y_column
    )

}

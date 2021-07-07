#'  scatter plot function
#'
#' Creates a scatter plot of the data.
#'
#' This function creates a plot based on the format of the data
#'
#'
#'
#' @param file A csv file with the source data, x_colom, a x as, Y_colum, a y as, primaryColor, a primary color for the plot
#'
#' @author Yves R. Sagaert
#'
#

#'
#' @return A graphical plot
#' @export
#'
#' @examples
#'   \dontrun{
#'      scatter("testdata.csv")
#'   }
#'
#'

scatter <- function(file, x_column, y_column, symbol, primary_color, secondary_color){
  df <- read.csv(file,header=TRUE,sep=';')
  if (substr(colnames(df)[1],2,3)== ".."){
    df <- read.csv(file,fileEncoding="UTF-8-BOM",header=TRUE,sep=';')
  }
  names <- names(df)
  basic_palette <- "Paired"

  # todo detect sep automatically later ;/,/tab
  plot(x=df[,x_column], y=df[,y_column], pch = symbol, col = primary_color,
       col.axis = secondary_color, col.lab = secondary_color,
    xlab=x_column , ylab = y_column
    )

}

#' line plot function
#'
#' Creates a line plot of the data.
#'
#' This function creates a plot based on the format of the data
#'
#'
#'
#' @param file A csv file with the source data
#' @param x_column a X as, _column a _ as, primaryColor, a primary color for the plot, secondaryColor, a secondary color for the plot
#' @author Yves R. Sagaert
#'
#'
#' @return A line plot
#' @export
#'
#' @examples
#'   \dontrun{
#'      line("testdata.csv")
#'   }
#'
#'

line <- function(file, x_column, y_column, symbol, line_type, primary_color, secondary_color) {
  df <- read.csv(file,header=TRUE,sep=';')
  if (substr(colnames(df)[1],2,3)== ".."){
    df <- read.csv(file,fileEncoding="UTF-8-BOM",header=TRUE,sep=';')
  }
  names <- names(df)
  basic_palette <- "Paired"

  # todo detect sep automatically later ;/,/tab
  plot(x=df[,x_column], y=df[,y_column], type = "b", lty=line_type, pch = symbol, col = primary_color,
       col.axis = secondary_color, col.lab = secondary_color,
       xlab=x_column , ylab = y_column
  )

  #scale_linetype_manual(values=c("twodash", "dotted"))
}

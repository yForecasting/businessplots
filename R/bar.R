#' function to plot bar plot
#'
#' Creates a bar plot of the data.
#'
#' This function creates a plot based on the format of the data
#'
#'
#'
#' @param file A csv file with the source data, x_column, y_column, horizontal, stacked and primary color to set the plot

#'
#' @author Yves R. Sagaert
#'
#'
#' @return A bar plot
#' @export
#'
#' @examples
#'   \dontrun{
#'      bar("testdata.csv")
#'   }
#'
#'

bar <- function(file, x_column, y_column, horizontal = TRUE, Stacked = FALSE, PrimaryColor){
  df <- read.csv(file,header=TRUE,sep=';')
  if (substr(colnames(df)[1],2,3)== ".."){
    df <- read.csv(file,fileEncoding="UTF-8-BOM",header=TRUE,sep=';')
  }
  names <- names(df)
  basic_color <- PrimaryColor
  basic_palette <- "Paired"
  pch <- 1
  if(Stacked){
    barplot(height=as.matrix(df), col = basic_color,
            xlab=x_column , ylab=y_column, beside= FALSE,
            horiz = horizontal )
  }
  # todo detect sep automatically later ;/,/tab
  else{
    if(horizontal){
      barplot(height=as.matrix(df[,y_column]), col = basic_color,
              xlab=y_column , ylab = x_column, beside= TRUE, names.arg = df[,x_column],
              horiz = horizontal
      )
    }
    else{
      barplot(height=as.matrix(df[,y_column]), col = basic_color,
              xlab=x_column , ylab = y_column, beside= TRUE, names.arg = df[,x_column],
              horiz = horizontal
      )
    }


  }
}

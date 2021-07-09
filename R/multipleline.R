#' multipleline plot function
#'
#' Creates a plot of the data with multiple lines.
#'
#' This function creates a plot with multiple lines based on the format of the data
#'
#'
#'
#' @param file A csv file with the source data
#' @param x_column Name of the column from the file you want to plot on the x-axis
#' @param y_column Name of the column from the file you want to plot on the y-axis
#' @param symbol Number of the symbol you want for the plot
#' @param line_type The type of line you want for the line plot
#' @param auto_fit Put false to start from 0, True shows the best fit.
#' @param primary_color First color for line plot
#' @param secondary_color Second color for line plot

#'
#' @author Emiel Creus
#'
#'
#' @return A multiple line plot
#' @export
#'
#' @examples
#'   \dontrun{
#'      multiline("testdata.csv", column_1 , c("column2", "column3", "column4"), 1, 1, "red","green","orange","yellow","blue","black")
#'   }
#'
#'

multiline <- function(file, x_column, y_columns, line_type, legend_pos, primary_color,
                      secondary_color, tertiary_color, quaternary_color, quinary_color, senary_color) {
  df <- read.csv(file,header=TRUE,sep=';')
  if (substr(colnames(df)[1],2,3)== ".."){
    df <- read.csv(file,fileEncoding="UTF-8-BOM",header=TRUE,sep=';')
  }
  names <- names(df)
  basic_palette <- "Paired"
  min_y <- min(df[,y_columns])
  max_y <- max(df[,y_columns])
  min_x <- min(df[,x_column])
  max_x <- max(df[,x_column])
  color <- c(primary_color,
             secondary_color, tertiary_color, quaternary_color, quinary_color, senary_color)
  y_lim <- c(min_y,max_y)
  j<-1
  i<-1
  pchlist <- c(0,1,2,3,4,5,6,8,15,16,17,18)
  plot(1,type='n',xlim=c(min_x,max_x),ylim=y_lim,col.axis = secondary_color, col.lab = secondary_color,
       xlab=x_column, ylab = "")
 for (column in y_columns){
   if(j>6){
     j<-1
   }
   if(i>12){
     i<-1
   }
    lines(df[,x_column], df[,column], type = "b", lty=line_type, pch = pchlist[i], ylim = y_lim,
          col = color[j])
    i <- i+1
    j <- j+1

 }
  legend(legend_pos, 0.92, legend=y_columns, col = color, pch=pchlist, bty = "n",
         lwd = 2, cex = 1.2,)

  #scale_linetype_manual(values=c("twodash", "dotted"))
}

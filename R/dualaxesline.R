#' multipleline plot function with multiple scales
#'
#' Creates a plot of the data with multiple lines that have a different scale.
#'
#' This function creates a plot with multiple lines and a different scale based on the format of the data
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
#' @return A multiple line plot with multiple scales
#' @export
#'
#' @examples
#'   \dontrun{
#'      multiline("testdata.csv", column_1 , c("column2", "column3", "column4"), 1, 1, "red","green","orange","yellow","blue","black")
#'   }
#'
#'

dualaxes <- function(file, x_column, y_columns, line_type, legend_pos, primary_color,
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
  j<-1
  i<-1
  line<-0
  line2<-2
  pchlist <- c(0,1,2,3,4,5,6,8,15,16,17,18)
  par(mar=c(4, 4, 4, 12) + 0.1)
  plot(df[,x_column], df[,y_columns[1]], axes=F, ylim=c(0,max(df[,y_columns[1]])), xlab="", ylab="",
       type="l",lty=line_type, main="",lwd=2, col = color[j])
  axis(2, ylim=c(0,max(df[,y_columns[1]])),lwd=2,line=line, col = secondary_color, col.axis = secondary_color)
  points(df[,x_column], df[,y_columns[1]],pch = pchlist[i], col = color[j])
  mtext(2,text=y_columns[1],line=line2, col=secondary_color)
  to <- length
  i <- i+1
  j <- j+1
  for (column in y_columns){
    if(column == y_columns[1]){
      next
    }
    if(j>6){
      j<-1
    }
    if(i>12){
      i<-1
    }
    par(new=T)
    plot(df[,x_column], df[,column], axes=F, ylim=c(0,max(df[,column])), xlab="", ylab="",
         type="l",lty=line_type, main="",lwd=2, col = color[j])
    axis(4, ylim=c(0,max(df[,column])),lwd=2,line=line, col = secondary_color, col.axis = secondary_color)
    points(df[,x_column], df[,column],pch = pchlist[i], col = color[j])
    mtext(4,text=column,line=line2, col=secondary_color)
    i <- i+1
    j <- j+1
    line <- line + 3.5
    line2 <- line2 + 3.5
  }
  legend(legend_pos, 0.92, legend=y_columns, col = color, pch=pchlist, bty = "n",
         lwd = 2, cex = 1.2,)

  #scale_linetype_manual(values=c("twodash", "dotted"))
}

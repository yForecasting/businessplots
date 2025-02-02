#' multiple line scatter plot function with multiple scales
#'
#' Creates a plot of the data with multiple scatter plots that have a different scale.
#'
#' This function creates a plot with multiple scatter plots and a different scale based on the format of the data
#'
#'
#'
#' @param file A csv file with the source data
#' @param x_column Name of the column from the file you want to plot on the x-axis
#' @param y_columns Name of the columns from the file you want to plot on the y-axes
#' @param legend_pos the position of the legend
#' @param primary_color First color for scatter plot
#' @param secondary_color Second color for scatter plot
#' @param tertiary_color Third color for scatter plot
#' @param quaternary_color Fourth color for scatter plot
#' @param quinary_color Fifth color for scatter plot
#' @param senary_color Sixth color for scatter plot

#'
#' @author Emiel Creus
#'
#'
#' @return A multiple line plot with multiple scales
#' @export
#'
#' @examples
#'   \dontrun{
#'      dualaxscatter("Testdata.csv", 
#' "Year", 
#' c("Quota", "Incidents","Complatins"),
#' "topleft","red","green","orange",
#' "yellow","blue","black")
#'   }
#'
#'

dualaxscatter <- function(file, x_column, y_columns, legend_pos, primary_color,
                     secondary_color, tertiary_color, quaternary_color, quinary_color, senary_color) {
  df <- utils::read.csv(file,header=TRUE,sep=';')
  if (substr(colnames(df)[1],2,3)== ".."){
    df <- utils::read.csv(file,fileEncoding="UTF-8-BOM",header=TRUE,sep=';')
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
  par(mar=c(4, 4, 4, 6) + 0.1)
  plot(df[,x_column], df[,y_columns[1]], ylim=c(0,max(df[,y_columns[1]])), xlab=x_column, ylab="",
       main="", pch = pchlist[i], col = color[j], col.axis = secondary_color, col.lab = secondary_color)
  axis(2, ylim=c(0,max(df[,y_columns[1]])),lwd=2,line=line, col = secondary_color, col.axis = secondary_color)
  mtext(2,text=y_columns[1],line=line2, col=secondary_color)
  to <- length
  i <- i+1
  j <- j+1
  line <- 2
  y_columns_axes <- y_columns[-1]
  y_data <- (df[,y_columns_axes])
  min_y = min(y_data)
  max_y = max(y_data)
  for (column in y_columns_axes){
    if(j>6){
      j<-1
    }
    if(i>12){
      i<-1
    }
    par(new=T)
    plot(df[,x_column], df[,column], axes=F, ylim=c(0,max_y),main="", xlab="", ylab="", pch = pchlist[i], col = color[j])
    mtext(4,text=column,line = line, col=secondary_color)
    i <- i+1
    j <- j+1
    line <- line + 1
  }
  axis(4, ylim=c(0,max(df[,column])),lwd=2, col = secondary_color, col.axis = secondary_color)

  legend(legend_pos, 0.92, legend=y_columns, col = color, pch=pchlist, bty = "n", cex = 1.2,)

  #scale_linetype_manual(values=c("twodash", "dotted"))
}

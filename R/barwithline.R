#' Bar plot with line
#'
#' Creates a bar plot with line
#'
#' This function plots a bar plot with line based on the data
#'
#'
#'
#' @param file A csv file with the source data
#' @param x_column Name of the column from the file you want to plot on the x-axis
#' @param y_column Name of the column from the file you want to plot on the y-axis
#' @param line_type The type of line you want for the line plot
#' @param primary_color First color for line plot
#' @param secondary_color Second color for line plot
#' @param tertiary_color Third color for scatter plot
#' @param quaternary_color Fourth color for scatter plot
#' @param quinary_color Fifth color for scatter plot
#' @param senary_color Sixth color for scatter plot
#'
#' @author Isolde Boussauw
#'
#'
#' @return A bar plot with line
#' @export
#'
#' @examples
#'   \dontrun{
#'      barwithline("testdata.csv",
#' column_1,
#' c("column2", "column3", "column4"),
#' 1, 1, "red","green",
#' "orange","yellow","
#' blue","black")
#'   }
#'
#'

barwithline <- function(file, x_column, y_column, line_type, legend_pos, primary_color,
                      secondary_color, tertiary_color, quaternary_color, quinary_color, senary_color) {
    df <- utils::read.csv(file, header = TRUE, sep = ';')
    if (substr(base::colnames(df)[1], 2, 3) == "..") {
        df <- utils::read.csv(file, fileEncoding = "UTF-8-BOM", header = TRUE, sep = ';')
    }
    names <- base::names(df)

    dfbar <- graphics::barplot(height = as.matrix(df[, y_columns[1]]), col = c(primary_color, secondary_color, tertiary_color,
                                                       quaternary_color, quinary_color, senary_color),
              xlab = x_column, ylab = y_columns[1], beside = TRUE, names.arg = df[, x_column],
              col.axis = secondary_color, col.lab = secondary_color
              )
    xlim <- c(base::floor(min(dfbar)), base::ceiling(base::max(dfbar)))
    color <- c(primary_color,
               secondary_color, tertiary_color, quaternary_color, quinary_color, senary_color)
    j<-1
    i<-1
    pchlist <- c(0,1,2,3,4,5,6,8,15,16,17,18)

    line_columns <- y_columns[-1]
    min_y <- base::min(df[, line_columns])
    max_y <- base::max(df[, line_columns])
    graphics::par(new=T)
    graphics::plot(x =  dfbar, y = df[, line_columns[1]], type = "o" , ylim=c(min_y, max_y),
         xaxt="n", yaxt="n", xlab="", ylab="", xlim = xlim, col = color[j], pch = pchlist[i])
    graphics::points(x =  dfbar, y = df[, line_columns[1]] , ylim=c(min_y, max_y), col = color[j], pch = pchlist[i])
    line_columns <- line_columns[-1]

    i <- i+1
    j <- j+1
    for (column in line_columns){
        if(j>6){
            j<-1
        }
        if(i>12){
            i<-1
        }

        graphics::lines(df[,x_column], df[,column], lty=line_type, pch = pchlist[i], xlim = xlim, ylim = c(min_y, max_y),
              col = color[j])
        graphics::points(x = dfbar, y = df[, column] , ylim=c(min_y, max_y), xlim = xlim,  col = color[j], pch = pchlist[i])
        i <- i+1
        j <- j+1

    }
    graphics::axis(4, ylim = c(min_y, max_y), lwd=2, col = secondary_color, col.axis = secondary_color)

}



#' function for bar plot with line
#'
#' Creates a bar plot with line
#'
#' this fuction plots a bar plot with line based on the data
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
#' @author Isolde Boussauw
#'
#'
#' @return A bar plot with line
#' @export
#'
#' @examples
#'   \dontrun{
#'      barwithline("testdata.csv", column_1 , c("column2", "column3", "column4"), 1, 1, "red","green","orange","yellow","blue","black")
#'   }
#'
#'

barwithline <- function(file, x_column, y_columns, line_type, legend_pos, primary_color,
                      secondary_color, tertiary_color, quaternary_color, quinary_color, senary_color) {
    df <- read.csv(file, header = TRUE, sep = ';')
    if (substr(colnames(df)[1], 2, 3) == "..") {
        df <- read.csv(file, fileEncoding = "UTF-8-BOM", header = TRUE, sep = ';')
    }
    names <- names(df)
    basic_palette <- "Paired"

    dfbar <- barplot(height = as.matrix(df[, y_columns[1]]), col = c(primary_color, secondary_color, tertiary_color,
                                                       quaternary_color, quinary_color, senary_color),
              xlab = x_column, ylab = y_columns[1], beside = TRUE, names.arg = df[, x_column],
              col.axis = secondary_color, col.lab = secondary_color
              )
    line_columns <- y_columns[-1]
    min_y <- min(df[, line_columns])
    max_y <- max(df[, line_columns])
    axis(4, ylim = c(min_y, max_y), lwd=2, col = secondary_color, col.axis = secondary_color)

    for (column in line_columns){
    lines(x =  dfbar, y = df[, column] , ylim=c(min_y, max_y))
    points(x =  dfbar, y = df[, column] , ylim=c(min_y, max_y))
    }
}

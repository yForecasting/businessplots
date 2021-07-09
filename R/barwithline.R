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

barwithline <- function(file, x_column, y_column, line_type, legend_pos, primary_color,
                      secondary_color, tertiary_color, quaternary_color, quinary_color, senary_color) {
    df <- read.csv(file, header = TRUE, sep = ';')
    if (substr(colnames(df)[1], 2, 3) == "..") {
        df <- read.csv(file, fileEncoding = "UTF-8-BOM", header = TRUE, sep = ';')
    }
    names <- names(df)
    basic_palette <- "Paired"
    dfbar <- barplot(height = as.matrix(df[, y_column]), col = c(primary_color, secondary_color, tertiary_color,
                                                       quaternary_color, quinary_color, senary_color),
              xlab = x_column, ylab = y_column, beside = TRUE, names.arg = df[, x_column],
              col.axis = secondary_color, col.lab = secondary_color
              )
    min_y <- min(df[, y_column])
    max_y <- max(df[, y_column])
    ylim = c(min_y, max_y)
    min_x <- min(df[, x_column])
    max_x <- max(df[, x_column])
    xlim = c(min_x, max_x)
    
    lines(x =  df[, x_column], y = df[, y_column])
    lines(x = xlim)
    points(x = xlim, y = ylim)
}
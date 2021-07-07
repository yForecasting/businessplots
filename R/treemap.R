#' generic businessplot function
#'
#' Creates a general plot of the data.
#'
#' This function creates a plot based on the format of the data
#' and this can be: treemap
#'
#'
#' @param file A csv file with the source data
#'
#' @author Yves R. Sagaert
#'
#' at import forecast
#' at import Mcomp
#'
#' @return A graphical plot
#' @export
#'
#' @examples
#'   \dontrun{
#'      bp("testdata.csv")
#'   }
#'
#'
#'

# plot a treemap
treemap <- function(file, indexColums, quantity_column, titel) {
    # file is the file with all original data to read
    # main_column is the name of the column with the column names of the pie chart
    # read file
    data <- read.csv(file, header = TRUE, sep = ';')
    # library
    library(treemap)
    treemap(data,
            index = indexColums,
            vSize = quantity_column,
            title = titel,
            type = "index" #todo  generiek maken
           )
}
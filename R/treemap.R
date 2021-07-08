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
create_treemap <- function(file, indexColumns, quantity_column) {
    # file is the file with all original data to read
    # index_columns are columns to group the tree

    # read file
    data <- read.csv(file, header = TRUE, sep = ';')
    if (substr(colnames(data)[1], 2, 3) == ".."){
        df <- read.csv(data, fileEncoding = "UTF-8-BOM", header = TRUE, sep = ';')
    }

    # library
    library(treemap)
    treemap(data,
            index=indexColumns,
            vSize = quantity_column)



}

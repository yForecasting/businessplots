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
create_treemap <- function(file, indexColums, quantity_column) {
    # file is the file with all original data to read
    # index_columns are columns to group the tree

    # read file
    data <- read.csv(file, header = TRUE, sep = ';')
    if (substr(colnames(data)[1], 2, 3) == ".."){
        df <- read.csv(data, fileEncoding = "UTF-8-BOM", header = TRUE, sep = ';')
    }
    # set index columns
    index_values <- data[, indexColums]
    # set quantity column
    quantity_values <- data[, quantity_column]
    print(quantity_values)    
    # library
    library(treemap)
    treemap(data,
            index=index_values)
            


}

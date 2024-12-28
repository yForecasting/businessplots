#' generic businessplot function
#'
#' Creates a general plot of the data.
#'
#' This function creates a plot based on the format of the data
#' and this can be: treemap
#'
#'
#' @param file A csv file with the source data
#' @param index Name of the index column from the file you want to plot
#' @param y_column Name of the quantity column from the file you want to plot
#' @param quantity Put True if you want to plot a horizontal bar plot, False for vertical plot.
#' @param primary_color First color for bar plot
#' @param secondary_color Second color for bar plot
#' @param tertiary_color Third color for bar plot
#' @param quaternary_color Fourth color for bar plot
#' @param quinary_color Fifth color for bar plot
#' @param senary_color Sixth color for bar plot
#'
#' @author Isolde Boussauw
#'
#' at import forecast
#' at import Mcomp
#'
#' @return A graphical plot
#' @export
#'
#' @examples
#'   \dontrun{
#'      create_treemap("testdata.csv",
#' "Year", "Quota",
#' "#004D9A", "#002142", "#0069D2",
#' "#0180FF", "#004D9A", "#002142")
#'   }
#'
#'
#'

# plot a treemap
create_treemap <- function(file, index, quantity, primary_color, secondary_color, tertiary_color, quaternary_color, quinary_color, senary_color) {
    # file is the file with all original data to read
    # index_columns are columns to group the tree

    # read file
    data <- utils::read.csv(file, header = TRUE, sep = ';')
    if (substr(colnames(data)[1], 2, 3) == ".."){
        df <- utils::read.csv(data, fileEncoding = "UTF-8-BOM", header = TRUE, sep = ';')
    }

    treemap::treemap(data,
            index=index,
            vSize = quantity,
            palette =  c(primary_color, secondary_color, tertiary_color, quaternary_color, quinary_color, senary_color))



}

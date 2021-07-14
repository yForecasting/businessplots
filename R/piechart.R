#' pie chart function
#'
#' Creates a pie chart of the data.
#'
#' This function creates a pie chart based on a column of a file with data.
#'
#' @param file A csv file with the source data
#' @param main Title of the pie chart
#' @param pie_label Name of the column of the file with the labels of the pie chart
#' @param pie_data_label Name of the column of the file with the data of the pie chart
#' @param primary_color First color for pie chart
#' @param secondary_color Second color for pie chart
#' @param tertiary_color Third color for pie chart
#' @param quaternary_color Fourth color for pie chart
#' @param quinary_color Fifth color for pie chart
#' @param senary_color Sixth color for pie chart
#'
#' @author Ruben Vanhecke
#'
#'
#' @return A pie chart
#' @export
#'
#' @examples
#'   \dontrun{
#'      piechart(
#' "testdata.csv", 
#' "Year", "Quota", 
#' "#004D9A", "#002142", 
#' "#0069D2", "#0180FF", 
#' "#4FA7FF", "#A7D3FF")
#'   }
#'
#'
#'

# plot pie chart
piechart <- function(file, main, pie_label, pie_data_label, primary_color, secondary_color, tertiary_color, quaternary_color, quinary_color, senary_color){
  # file is the file with all original data to read
  # pie_label is the name of the column of the file with the labels of the pie chart
  # main is the title of the pie chart
  # pie_data_label is the name of the column of the file with the data of the pie chart
  # primary_color is first color for pie chart
  # secondary_color is second color for pie chart
  # tertiary_color is third color for pie chart
  # quaternary_color is fourth color for pie chart
  # quinary_color is fifth color for pie chart
  # senary_color is sixth color for pie chart

  # read data
  data <- utils::read.csv(file,header=TRUE,sep=';')
  if (substr(colnames(data)[1],2,3)== ".."){
    data <- utils::read.csv(file,fileEncoding="UTF-8-BOM",header=TRUE,sep=';')
  }

  # set labels
  labels <- data[, pie_label]

  # set colors
  colors <- c(primary_color, secondary_color, tertiary_color, quaternary_color, quinary_color, senary_color)

  # set x for pie chart
  x <- as.numeric(gsub(",", ".", data[, pie_data_label]))

  # plot pie
  pie(x=x, labels=labels, main=main, col=colors, col.axis = secondary_color, col.lab = secondary_color)
}


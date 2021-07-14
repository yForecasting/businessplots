#' spider chart function
#'
#' Creates a spider chart of the data.
#'
#' This function creates a spider chart based on two columns of a file with data.
#'
#' @param file A csv file with the source data
#' @param spider_label Name of the column of the file with the labels of the spider chart
#' @param spider_data_label Name of the column of the file with the data of the spider chart
#' @param title Title of the spider chart
#' @param polygon_line_width Line width of the polygon
#' @param net_line_type Net line type
#' @param net_line_width Net width
#' @param primary_color First color for spider chart
#' @param secondary_color Second color for spider chart
#' @param tertiary_color Third color for spider chart
#' @param quaternary_color Fourth color for spider chart
#'
#' @author Ruben Vanhecke
#'
#' at import fmsb
#'
#' @return A spider chart
#' @export
#'
#' @examples
#'   \dontrun{
#'      spiderchart("testdata.csv", "
#' Year", "Quota", "
#' Annual quota", 4, 2, 2, 
#' "#004D9A", "#002142", 
#' "#0069D2", "#0180FF")
#'   }
#'
#'
#'

# plot spider chart
spiderchart <- function(file, spider_label, spider_data_label, title, polygon_line_width, net_line_type, net_line_width, primary_color, secondary_color, tertiary_color, quaternary_color){
  # file is a csv file with the source data
  # spider_label is the name of the column with the labels of the spider chart
  # spider_data_label is the name of the column with the data of the spider chart
  # title is the title of the spider chart
  # primary_color is first color for pie chart
  # secondary_color is second color for pie chart
  # tertiary_color is third color for pie chart
  # quaternary_color is fourth color for pie chart
  # polygon_line_width is the line width of the polygon
  # net_line_type is the net line type
  # net_line_width is the net width

  # library
  library(fmsb)

  # read data
  data <- utils::read.csv(file,header=TRUE,sep=';')
  if (substr(colnames(data)[1],2,3)== ".."){
    df <- utils::read.csv(data,fileEncoding="UTF-8-BOM",header=TRUE,sep=';')
  }

  # retrieve columns spider chart
  spider_columns <- data[, spider_label]

  # set empty data frame spider chart
  spider_data <- data.frame(matrix(ncol=length(spider_columns), nrow=1))

  # set columns empty data frame spider chart
  colnames(spider_data) <- c(spider_columns)

  # feed empty data frame spider chart with data
  for (i in 1:nrow(data)){
    spider_data[i] <- data[i,][spider_data_label]
  }

  # retrieve max value for spider data
  max_spider_data <- max(data[, spider_data_label])

  # retrieve min value for spider data
  min_spider_data <- min(data[, spider_data_label])

  # add max value and min value to spider chart
  spider_data <- rbind(rep(max_spider_data, min_spider_data) , rep(0, min_spider_data) , spider_data)

  # convert color to rgb for transparent background
  rgb <- col2rgb(secondary_color)

  # plot spider chart
  fmsb::radarchart(spider_data, pcol=primary_color, pfcol=rgb(rgb["red", ], rgb["green", ], rgb["blue", ], max = 255, alpha = 100, names = "blue50"), cglcol=tertiary_color, axislabcol=quaternary_color,
             plwd=polygon_line_width, cglty=net_line_type, cglwd=net_line_width, title=title)
}

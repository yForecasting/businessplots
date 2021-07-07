#' spider chart function
#'
#' Creates a spider chart of the data.
#'
#' This function creates a spider chart based on two columns of a file with data.
#'
#' @param file A csv file with the source data
#' @param main_column Name of the column of the file with the column names for the spider chart
#' @param data_column Name of the column of the file with the data for the spider chart
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
#'      spiderchart("testdata.csv", "Year", "Quota")
#'   }
#'
#'
#'

# plot spider chart
spiderchart <- function(file, main_column, data_column){
  # file is a csv file with the source data
  # main_column is the name of the column with the column names of the spider chart
  # data_column is the name of the column with the data of the spider chart

  # library
  library(fmsb)

  # read data
  data <- read.csv(file,header=TRUE,sep=';')
  if (substr(colnames(data)[1],2,3)== ".."){
    df <- read.csv(data,fileEncoding="UTF-8-BOM",header=TRUE,sep=';')
  }

  # retrieve columns spider chart
  spider_columns <- data[, main_column]

  # set empty data frame spider chart
  spider_data <- data.frame(matrix(ncol=length(spider_columns), nrow=1))

  # set columns empty data frame spider chart
  colnames(spider_data) <- c(spider_columns)

  # feed empty data frame spider chart with data
  for (i in 1:nrow(data)){
    spider_data[i] <- data[i,][data_column]
  }

  # retrieve max value for spider data
  max_spider_data <- max(data[, data_column])

  # retrieve min value for spider data
  min_spider_data <- min(data[, data_column])

  # add max value and min value to spider chart
  spider_data <- rbind(rep(max_spider_data, min_spider_data) , rep(0, min_spider_data) , spider_data)

  # plot spider chart
  radarchart(spider_data)
}

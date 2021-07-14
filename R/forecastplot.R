  #' gauge chart function
  #'
  #' Creates a forecast plot based the actual values and the predicted values.
  #'
  #' This function creates a forecast plot based on the actual values and the predicted values.
  #'
  #' @param file A csv file with source data
  #' @param X The column name of the actual values
  #' @param y The column name of the predicted values
  #'
  #' @author Ruben Vanhecke
  #'
  #' at import greybox
  #'
  #' @return A forecast plot
  #' @export
  #'
  #' @examples
  #'   \dontrun{
  #'        forecastplot("ProductionDataSample.csv", 
  #' "Weight", 
  #' "WeightPredicted")
  #'   }
  #'
  #'
  #'

  # plot forecast plot
  forecastplot <- function(file = "ProductionDataSample.csv", X, y){
    # file is the file with the source data
    # X is the column name of the actual values
    # y is the column name of the predicted values
    # return forecast plot

    # library
    library(greybox)

    # read data
    data <- utils::read.csv(file,header=TRUE,sep=';')
    if (substr(colnames(data)[1],2,3)== ".."){
      data <- utils::read.csv(file,fileEncoding="UTF-8-BOM",header=TRUE,sep=';')
    }

    # plot forecast plot
    greybox::graphmaker(data[, X], data[, y])
  }

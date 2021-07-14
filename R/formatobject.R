#' formatobject function
#'
#' Retrieve the styles for all the charts.
#'
#' This function retrieves all the styles (like colors, fonts, line thicknness...) for all the charts.
#'
#' @param file A csv file with the source data
#'
#' @author Ruben Vanhecke
#'
#'
#' @return Styles
#' @export
#'
#' @examples
#'   \dontrun{
#'      formatobject("testdata.csv")
#'      extract_data_formatobject(formatobject("testdata.csv"), "colors)
#'   }
#'
#'
#'

# read source data
formatobject <- function(file = "formatobject.csv"){

  # read data
  data <- utils::read.csv(file,header=TRUE,sep=';')
  if (substr(colnames(data)[1],2,3)== ".."){
    data <- utils::read.csv(file,fileEncoding="UTF-8-BOM",header=TRUE,sep=';')
  }

  # return data
  return(data)
}

# extract data
extract_data_formatobject <- function(formatobject = formatobject("formatobject.csv"), column_name = "colors") {
  data <- formatobject[, column_name]
  if(column_name == "colors"){
    return(as.list(strsplit(data, ",")[[1]]))
  }
  else{
    return(data)
    }

}

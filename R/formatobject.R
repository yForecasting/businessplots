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

# read source data
formatobject <- function(file = "formatobject.csv"){

  # read data
  data <- read.csv(file,header=TRUE,sep=';')
  if (substr(colnames(data)[1],2,3)== ".."){
    data <- read.csv(file,fileEncoding="UTF-8-BOM",header=TRUE,sep=';')
  }

  # return data
  return(data)
}

# extract data
extract_data_formatobject <- function(formatobject = formatobject("formatobject.csv"), column_name = "colors") {
  data <- formatobject[, column_name]
  return(as.list(strsplit(data, ",")[[1]]))
}

##############################################################
# SAMPLE IMPLEMENTATION - REMOVE BEFORE PRODUCTION RELEASE
##############################################################
# you can test the sample by running this file and filling in scatter() in the console. All formatobject lines are marked with comments

# read formatobject
myObject <- formatobject() # no param = default "formatobject.csv"
colors <- extract_data_formatobject(formatobject=myObject, "colors") # extract colors formatobject

sample_scatter <- function(file = "YearReport.csv", x_column = "Year", y_column = "Quota"){
  df <- read.csv(file,header=TRUE,sep=';')
  if (substr(colnames(df)[1],2,3)== ".."){
    df <- read.csv(file,fileEncoding="UTF-8-BOM",header=TRUE,sep=';')
  }

  names <- names(df)
  basic_color <- colors[[1]] # formatobject
  basic_palette <- "Paired"
  pch <- 1

  plot(x=df[,x_column], y=df[,y_column], pch = pch, col = basic_color,
       xlab=x_column , ylab = y_column
  )
}

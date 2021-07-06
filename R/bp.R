#' generic businessplot function
#'
#' Creates a general plot of the data.
#'
#' This function creates a plot based on the format of the data
#' and this can be: scatterplot, lineplot or bar plot.
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
scatter <- function(file, x_column, y_column){
  df <- read.csv(file,header=TRUE,sep=';')
  if (substr(colnames(df)[1],2,3)== ".."){
    df <- read.csv(file,fileEncoding="UTF-8-BOM",header=TRUE,sep=';')
  }
  names <- names(df)
  basic_color <- "#999999"
  basic_palette <- "Paired"
  pch <- 1

  # todo detect sep automatically later ;/,/tab
  plot(x=df[,x_column], y=df[,y_column], pch = pch, col = basic_color,
    xlab=x_column , ylab = y_column
    )
}

bar <- function(file, x_column, y_column, horizontal = TRUE, Stacked = FALSE){
  df <- read.csv(file,header=TRUE,sep=';')
  if (substr(colnames(df)[1],2,3)== ".."){
    df <- read.csv(file,fileEncoding="UTF-8-BOM",header=TRUE,sep=';')
  }
  names <- names(df)
  basic_color <- "#999999"
  basic_palette <- "Paired"
  pch <- 1
  if(Stacked){
    barplot(height=as.matrix(df), col = basic_color,
            xlab=x_column , ylab=y_column, beside= FALSE,
            horiz = horizontal )
  }
  # todo detect sep automatically later ;/,/tab
  else{
    if(horizontal){
      barplot(height=as.matrix(df[,y_column]), col = basic_color,
              xlab=y_column , ylab = x_column, beside= TRUE, names.arg = df[,x_column],
              horiz = horizontal
      )
    }
    else{
      barplot(height=as.matrix(df[,y_column]), col = basic_color,
              xlab=x_column , ylab = y_column, beside= TRUE, names.arg = df[,x_column],
              horiz = horizontal
      )
    }


  }
}

line <- function(file, x_column, y_column){
  df <- read.csv(file,header=TRUE,sep=';')
  if (substr(colnames(df)[1],2,3)== ".."){
    df <- read.csv(file,fileEncoding="UTF-8-BOM",header=TRUE,sep=';')
  }
  names <- names(df)
  basic_color <- "#999999"
  basic_palette <- "Paired"
  color <- "red"
  pch <- 1

  # todo detect sep automatically later ;/,/tab
  plot(x=df[,x_column], y=df[,y_column], type ="b", pch = pch, col = color,
       xlab=x_column , ylab = y_column
  )
  #scale_linetype_manual(values=c("twodash", "dotted"))
}

#' function to plot bar plot
#'
#' Creates a bar plot of the data.
#'
#' This function creates a plot based on the format of the data
#'
#'
#'
#' @param file A csv file with the source data, x_column, y_column, horizontal, stacked and primary color to set the plot

#'
#' @author Yves R. Sagaert
#'
#'
#' @return A bar plot
#' @export
#'
#' @examples
#'   \dontrun{
#'      bar("testdata.csv")
#'   }
#'
#'

bar <- function(file, x_column, y_column, horizontal = TRUE, Stacked = FALSE,
                primary_color, secondary_color, tertiary_color, quaternary_color, quinary_color, senary_color){
  df <- read.csv(file,header=TRUE,sep=';')
  if (substr(colnames(df)[1],2,3)== ".."){
    df <- read.csv(file,fileEncoding="UTF-8-BOM",header=TRUE,sep=';')
  }
  names <- names(df)
  basic_palette <- "Paired"
  if(Stacked){
    barplot(height=as.matrix(df), col = c(primary_color, secondary_color, tertiary_color,
                                          quaternary_color, quinary_color, senary_color),
            xlab=x_column , ylab=y_column, beside= FALSE,
            col.axis = secondary_color, col.lab = secondary_color,
            horiz = horizontal )
  }
  # todo detect sep automatically later ;/,/tab
  else{
    if(horizontal){
      barplot(height=as.matrix(df[,y_column]), col = c(primary_color, secondary_color, tertiary_color,
                                                       quaternary_color, quinary_color, senary_color),
              xlab=y_column , ylab = x_column, beside= TRUE, names.arg = df[,x_column],
              col.axis = secondary_color, col.lab = secondary_color,
              horiz = horizontal
      )
    }
    else{
      barplot(height=as.matrix(df[,y_column]), col = c(primary_color, secondary_color, tertiary_color,
                                                       quaternary_color, quinary_color, senary_color),
              xlab=x_column , ylab = y_column, beside= TRUE, names.arg = df[,x_column],
              col.axis = secondary_color, col.lab = secondary_color,
              horiz = horizontal
      )
    }


  }
}

#' bar plot function
#'
#' Creates a bar plot of the data.
#'
#' This function creates a bar plot based on two columns of a file with data.
#'
#' @param file A csv file with the source data
#' @param x_column Name of the column from the file you want to plot on the x-axis
#' @param y_column Name of the column from the file you want to plot on the y-axis
#' @param horizontal Put True if you want to plot a horizontal bar plot, False for vertical plot.
#' @param stacked Put True for a stacked plot, False for normal bar plot
#' @param primary_color First color for bar plot
#' @param secondary_color Second color for bar plot
#' @param tertiary_color Third color for bar plot
#' @param quaternary_color Fourth color for bar plot
#' @param quinary_color Fifth color for bar plot
#' @param senary_color Sixth color for bar plot
#'
#' @author Emiel Creus
#'
#'
#' @return A bar plot
#' @export
#'
#' @examples
#'   \dontrun{
#'      bar("testdata.csv",
	#' "column_1"
#' "column2",
#' TRUE, FALSE,
#' "#004D9A",
#' "#002142",
#' "#0069D2",
#' "#0180FF",
#' "#004D9A",
#' "#002142") )
#'   }
#'


bar <- function(file, x_column, y_column, horizontal = TRUE, stacked = FALSE,
                primary_color, secondary_color, tertiary_color, quaternary_color, quinary_color, senary_color){
  df <- utils::read.csv(file,header=TRUE,sep=';')
  if (substr(colnames(df)[1],2,3)== ".."){
    df <- utils::read.csv(file,fileEncoding="UTF-8-BOM",header=TRUE,sep=';')
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

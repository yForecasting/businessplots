#' box plot function
#'
#' Creates a box plot of the data.
#'
#' This function creates a box plot based on a column of a file with data.
#'
#' @param file A csv file with the source data
#' @param main_column Name of the column from the file you want to make a boxplot of.
#' @param mean_dot_symbol number representing the shape of the symbol for the mean dot.
#' @param mean_dot Put True if you want to plot the mean dot.
#' @param primary_color First color for bar plot
#' @param secondary_color Second color for bar plot
#' @param tertiary_color Third color for bar plot
#' @param quaternary_color Fourth color for bar plot
#' @param quinary_color Fifth color for bar plot
#' @param senary_color Sixth color for bar plo
#'
#' @author Emiel Creus
#'
#'
#' @return A box plot
#' @export
#'
#' @examples
#'   \dontrun{
#'      boxplt("testdata.csv", 
#' c("column_1","column_2"), 
#' 19, TRUE, 
#' "#004D9A", 
#' "#002142", 
#' "#0069D2", "#0180FF",
#' "#004D9A", "#002142") )
#'   }
#'

boxplt <- function(file, columns, mean_dot_symbol, mean_dot = FALSE,  primary_color, secondary_color,
                   tertiary_color, quaternary_color, quinary_color, senary_color){
  df <- utils::read.csv(file,header=TRUE,sep=';')
  if (substr(colnames(df)[1],2,3)== ".."){
    df <- utils::read.csv(file,fileEncoding="UTF-8-BOM",header=TRUE,sep=';')
  }
  names <- names(df)
  basic_color <- "#999999"
  basic_palette <- "Paired"
  mean_dot_position <- 1


  # todo detect sep automatically later ;/,/tab
  boxplot(x = df[,columns], xlab="", main = "",
          col=primary_color,
          medcol=secondary_color,
          whiskcol=tertiary_color,
          staplecol=quaternary_color,
          boxcol=primary_color,
          outcol=quinary_color,
          outpch=mean_dot_symbol,
          col.axis = secondary_color,
          col.lab = secondary_color)
  if(mean_dot){
    for(column in columns){
      points(x=mean_dot_position, y=mean(df[,column]), col = senary_color, pch=mean_dot_symbol)
      mean_dot_position <- mean_dot_position + 1
    }

  }
  #scale_linetype_manual(values=c("twodash", "dotted"))
}


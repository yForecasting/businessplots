#' histogram plot function
#'
#' Creates a histogram of the data.
#'
#' This function creates a histogram plot based on a column of a file with data.
#'
#' @param file A csv file with the source data
#' @param main_column Name of the column from the file you want to plot in the histogram
#' @param show_normal_distribution Put True if you want to show the normal distribution on top of the histogram
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
#' @return A histogram plot
#' @export
#'
#' @examples
#'   \dontrun{
#'      histogram("testdata.csv",
#' "column_1", TRUE,
#' "#004D9A",
#' "#002142",
#' "#0069D2",
#' "#0180FF","#004D9A",
#' "#002142")
#'   }
#'
histogram <- function(file, main_column, show_normal_distribution = FALSE, primary_color,
                      secondary_color, tertiary_color, quaternary_color, quinary_color, senary_color){
  df <- utils::read.csv(file,header=TRUE,sep=';')
  if (substr(colnames(df)[1],2,3)== ".."){
    df <- utils::read.csv(file,fileEncoding="UTF-8-BOM",header=TRUE,sep=';')
  }
  names <- names(df)
  basic_palette <- "Paired"

  # convert color to rgb for transparent background
  rgb <- col2rgb(secondary_color)



  # todo detect sep automatically later ;/,/tab
  h <- hist(x = df[,main_column], xlab=main_column,
            col = c(primary_color, secondary_color, tertiary_color, quaternary_color, quinary_color, senary_color)
            ,col.axis = secondary_color, col.lab = secondary_color, main="")

  if(show_normal_distribution){
  x <- df[,main_column]
  xfit<-seq(min(x),max(x),length=40)
  yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
  yfit <- yfit*diff(h$mids[1:2])*length(x)
  lines(xfit, yfit, col="Blue", lwd=2)
  polygon(c(xfit[xfit>=min(xfit)], max(xfit),
            min(xfit)), c(yfit[xfit>=min(xfit)], 0, 0),
          col=rgb(rgb["red", ], rgb["green", ], rgb["blue", ], max = 255, alpha = 100, names = "blue50"))


  }
  # TODO SHOW distribution in line plot
  #scale_linetype_manual(values=c("twodash", "dotted"))
}





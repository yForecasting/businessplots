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
bp <- function(file){
  df <- read.csv(file,header=TRUE,sep=';')
  if (substr(colnames(df)[1],2,3)== ".."){
    df <- read.csv(file,fileEncoding="UTF-8-BOM",header=TRUE,sep=';')
  }
  names <- names(df)
  basic_color <- "#999999"
  basic_palette <- "Paired"
  # todo detect sep automatically later ;/,/tab
  ggplot(df, aes(x=df[,1], y=df[,2]))+
    xlab(names[1])+ylab(names[2])+
    scale_color_brewer(palette=basic_palette)+
    theme_minimal() +
    theme(panel.grid.minor.x = element_blank())+
    theme(legend.position="none")+
    geom_line(aes(color=basic_color),size=1.5)+
    geom_point(aes(color=basic_color),size=3)+scale_x_continuous(breaks=df[,1])
    #scale_linetype_manual(values=c("twodash", "dotted"))
}

scatterplot <- function(file){
  df <- read.csv(file,header=TRUE,sep=';')
  if (substr(colnames(df)[1],2,3)== ".."){
    df <- read.csv(file,fileEncoding="UTF-8-BOM",header=TRUE,sep=';')
  }
  names <- names(df)
  basic_color <- "#999999"
  basic_palette <- "Paired"
  pch <- 1

  # todo detect sep automatically later ;/,/tab
  plot(x=df[,1], y=df[,2], pch = 0, col = basic_color,
    xlab=names[1] , ylab = names[2]
    )
}

bar <- function(file){
  df <- read.csv(file,header=TRUE,sep=';')
  if (substr(colnames(df)[1],2,3)== ".."){
    df <- read.csv(file,fileEncoding="UTF-8-BOM",header=TRUE,sep=';')
  }
  names <- names(df)
  basic_color <- "#999999"
  basic_palette <- "Paired"
  pch <- 1

  # todo detect sep automatically later ;/,/tab
  barplot(height=as.matrix(df[,2]), col = basic_color,
     xlab=names[1] , ylab = names[2], beside=TRUE, names.arg = df[,1],
     horiz = FALSE
)
}

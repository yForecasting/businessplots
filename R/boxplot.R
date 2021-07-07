boxplt <- function(file, main_column, mean_dot = FALSE, mean_dot_color){
  df <- read.csv(file,header=TRUE,sep=';')
  if (substr(colnames(df)[1],2,3)== ".."){
    df <- read.csv(file,fileEncoding="UTF-8-BOM",header=TRUE,sep=';')
  }
  names <- names(df)
  basic_color <- "#999999"
  basic_palette <- "Paired"



  # todo detect sep automatically later ;/,/tab
  boxplot(x = df[,main_column], xlab=main_column, main = "Boxplot")
  if(mean_dot){
  points(mean(df[,main_column]), col = "mean_dot_color", pch=19)
  }
  #scale_linetype_manual(values=c("twodash", "dotted"))
}


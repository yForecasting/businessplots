
histogram <- function(file, main_column){
  df <- read.csv(file,header=TRUE,sep=';')
  if (substr(colnames(df)[1],2,3)== ".."){
    df <- read.csv(file,fileEncoding="UTF-8-BOM",header=TRUE,sep=';')
  }
  names <- names(df)
  basic_color <- "#999999"
  basic_palette <- "Paired"



  # todo detect sep automatically later ;/,/tab
  hist(x = df[,main_column], xlab=main_column, main = "Histogram")
  curve(dnorm(x, mean=mean(df[,2]), sd=sd(df[,2])), add=TRUE)
  #scale_linetype_manual(values=c("twodash", "dotted"))
}





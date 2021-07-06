boxplt <- function(file){
  df <- read.csv(file,header=TRUE,sep=';')
  if (substr(colnames(df)[1],2,3)== ".."){
    df <- read.csv(file,fileEncoding="UTF-8-BOM",header=TRUE,sep=';')
  }
  names <- names(df)
  basic_color <- "#999999"
  basic_palette <- "Paired"



  # todo detect sep automatically later ;/,/tab
  boxplot(x = df[,2], xlab=names[2], main = "Boxplot")
  #scale_linetype_manual(values=c("twodash", "dotted"))
}

boxplt <- function(file, main_column, mean_dot_symbol, mean_dot = FALSE,  primary_color, secondary_color,
                   tertiary_color, quaternary_color, quinary_color, senary_color){
  df <- read.csv(file,header=TRUE,sep=';')
  if (substr(colnames(df)[1],2,3)== ".."){
    df <- read.csv(file,fileEncoding="UTF-8-BOM",header=TRUE,sep=';')
  }
  names <- names(df)
  basic_color <- "#999999"
  basic_palette <- "Paired"



  # todo detect sep automatically later ;/,/tab
  boxplot(x = df[,main_column], xlab=main_column, main = "",
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
  points(mean(df[,main_column]), col = senary_color, pch=mean_dot_symbol)
  }
  #scale_linetype_manual(values=c("twodash", "dotted"))
}


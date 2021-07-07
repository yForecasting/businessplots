
histogram <- function(file, main_column, show_normal_distribution = FALSE, primary_color,
                      secondary_color, tertiary_color, quaternary_color, quinary_color, senary_color){
  df <- read.csv(file,header=TRUE,sep=';')
  if (substr(colnames(df)[1],2,3)== ".."){
    df <- read.csv(file,fileEncoding="UTF-8-BOM",header=TRUE,sep=';')
  }
  names <- names(df)
  basic_palette <- "Paired"
  normal_distribution_color <- rgb(0, 0, 255, max = 255, alpha = 100, names = "blue50")



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
            min(xfit)), c(yfit[xfit>=min(xfit)], 0, 0), col=normal_distribution_color)


  }
  # todo
  #scale_linetype_manual(values=c("twodash", "dotted"))
}





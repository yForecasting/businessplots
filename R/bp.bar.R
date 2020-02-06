bp.bar <- function(file, valuesgiven=TRUE, horbars=FALSE){
  file= "YearReport.csv"
  file = "ProductionDataSample.csv"
  df <- read.csv(file,header=TRUE,sep=';')
  if (substr(colnames(df)[1],2,3)== ".."){
    df <- read.csv(file,fileEncoding="UTF-8-BOM",header=TRUE,sep=';')
  }
  names <- names(df)
  basic_color <- "#999999"
  basic_palette <- "Paired"
  if (horbars){
    # HORIZONTAL bar chart
    if (valuesgiven){
      # The values of the bar plot are already calculated
      ggplot(df, aes(x=df[,1], y=df[,2]))+
        xlab(names[1])+ylab(names[2])+
        scale_color_brewer(palette=basic_palette)+
        theme_minimal() +
        theme(panel.grid.minor.x = element_blank())+
        theme(legend.position="none")+
        geom_bar(color=basic_color,stat="identity")+
        coord_flip()
    } else {
      # Sample data is given which need to be summarized to bar graph
      ggplot(df, aes(x=df[,1]))+
        xlab(names[1])+ylab(names[2])+
        scale_color_brewer(palette=basic_palette)+
        theme_minimal() +
        theme(panel.grid.minor.x = element_blank())+
        theme(legend.position="none")+
        geom_bar(color=basic_color)+
        coord_flip()
    }
  } else {
    # VERTICAL bar chart
    if (valuesgiven){
    # The values of the bar plot are already calculated
    ggplot(df, aes(x=df[,1], y=df[,2]))+
      xlab(names[1])+ylab(names[2])+
      scale_color_brewer(palette=basic_palette)+
      theme_minimal() +
      theme(panel.grid.minor.x = element_blank())+
      theme(legend.position="none")+
      geom_bar(color=basic_color,stat="identity")
    } else {
    # Sample data is given which need to be summarized to bar graph
    ggplot(df, aes(x=df[,1]))+
      xlab(names[1])+ylab(names[2])+
      scale_color_brewer(palette=basic_palette)+
      theme_minimal() +
      theme(panel.grid.minor.x = element_blank())+
      theme(legend.position="none")+
      geom_bar(color=basic_color)
    }
  }
}

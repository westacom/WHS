plotLJ <- function(data, use_names=NULL){
  
  
  
  unique_names <- unique(data$SampleName)
  
  unique_QC <- grep("QC", data$SampleName)
  
  data_QC <- data[unique_QC, ]
  
  plot_list <- list()
  
  QC_names <- c("_30_", "_300_", "_3000_", "_800_") 
  
  QC_30 <- c("QC_30", "QC_30_end")
  QC_300 <- c("QC_300", "QC_300_end")
  QC_3000 <- c("QC_3000", "QC_3000_end")
  QC_800 <- c("QC_800", "QC_800_end")
  
  ff <- by(data=data, data$SampleName, function(x) FUN = {
    by(x, x$Compound, function(y) FUN = {
      tmp <- y
      tmp$Date <- as.POSIXlt(tmp$Date)
      p1 <- ggplot(data=tmp) +  geom_line(aes(x=Date,y=Concentration)) + geom_point(aes(x=Date,y=Concentration)) + 
        geom_line(aes(x=Date,y=mean(Concentration))) + geom_line(aes(x=Date, y=mean(Concentration)+2*sd(Concentration))) +
        geom_line(aes(x=Date, y=mean(Concentration)-2*sd(Concentration)))+
        geom_line(aes(x=Date, y=mean(Concentration)+1*sd(Concentration)), linetype=2)+
        geom_line(aes(x=Date, y=mean(Concentration)-1*sd(Concentration)), linetype=2)+
        geom_line(aes(x=Date, y=mean(Concentration)+3*sd(Concentration)), linetype="dotdash")+
        geom_line(aes(x=Date, y=mean(Concentration)-3*sd(Concentration)), linetype="dotdash")+
        scale_x_datetime(breaks=date_breaks("1 month")) + 
        labs(x="Date", y="Concentration (ng/ml)", title=paste(unique(tmp$SampleName), unique(tmp$Compound), sep=" "))
    })
  })
  
  ggplot()
  ggplot()
  
  return(ff)
  
}
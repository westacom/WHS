plotLJ <- function(data, use_names=NULL){
  require(ggplot2)
  require(reshape2)
  
  
  
  unique_names <- unique(data$SampleName)
  
  unique_QC <- grep("QC", data$SampleName)
  
  data_QC <- data[unique_QC, ]
  
  plot_list <- list()
  
  QC_names <- c("_30_", "_300_", "_3000_", "_800_") 
  #Get Start/End measruments of QC Samples
  QC_30 <- c("QC_30", "QC_30_end")
  QC_300 <- c("QC_300", "QC_300_end")
  QC_3000 <- c("QC_3000", "QC_3000_end")
  QC_800 <- c("QC_800", "QC_800_end")
  
  ind_30 <- which(data$SampleName %in% QC_30)
  ind_300 <- which(data$SampleName %in% QC_300)
  ind_3000 <- which(data$SampleName %in% QC_3000)
  ind_800 <- which(data$SampleName %in% QC_800)
  
  QC_30_data <- data[ind_30, ]
  QC_300_data <- data[ind_300, ]
  QC_3000_data <- data[ind_3000, ]
  QC_800_data <- data[ind_800, ]
  
  QC_30_data <- by(QC_30_data, QC_30_data$Compound, function(x) FUN = {
    by(x,x$Date, function(x2) FUN = {
      mean(x2$Concentration, na.rm=TRUE)
    })
  })

  
  QC_300_data <- by(QC_300_data, QC_300_data$Compound, function(x) FUN = {
    by(x,x$Date, function(x2) FUN = {
      mean(x2$Concentration, na.rm=TRUE)
    })
  })
  
  
  QC_3000_data <- by(QC_3000_data, QC_3000_data$Compound, function(x) FUN = {
    by(x,x$Date, function(x2) FUN = {
      mean(x2$Concentration, na.rm=TRUE)
    })
  })
  
  
  QC_800_data <- by(QC_800_data, QC_800_data$Compound, function(x) FUN = {
    by(x,x$Date, function(x2) FUN = {
      mean(x2$Concentration, na.rm=TRUE)
    })
  })
  
  QC_30_data <- melt(do.call(rbind, QC_30_data))
  QC_30_data$SampleName <- "QC_30"
  QC_300_data <- melt(do.call(rbind, QC_300_data))
  QC_300_data$SampleName <- "QC_300"
  QC_3000_data <- melt(do.call(rbind, QC_3000_data))
  QC_3000_data$SampleName <- "QC_3000"
  
  QC_800_data <- melt(do.call(rbind, QC_800_data))
  QC_800_data$SampleName <- "QC_800"
  
  merged_data <- rbind(QC_30_data, QC_300_data, QC_3000_data, QC_800_data)
  colnames(merged_data) <- c("Compound", "Date", "Concentration","SampleName")
  
  merged_data <- as.data.frame(merged_data)
  
  ff <- by(data=merged_data, merged_data$SampleName, function(x) FUN = {
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
        labs(x="Date", y="Concentration (ng/ml)", title=paste(unique(tmp$SampleName), unique(tmp$Compound), sep=" "))+
        theme(text=element_text(size=15), axis.text.x = element_text(size=15, angle = 45, hjust = 1))
    })
  })
  

  
  return(ff)
  
}
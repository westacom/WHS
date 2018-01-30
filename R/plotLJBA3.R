plotLJBA3 <- function(data, use_names=NULL, remove_bool=TRUE){
  require(ggplot2)
  require(reshape2)
  require(scales)
  require(gridExtra)
  require(lubridate)
  require(grid)
  require(cowplot)
  #require(kable)
  unique_names <- unique(data$SampleName)
  
  unique_QC <- grep("QC", data$SampleName)
  
  data_QC <- data[unique_QC, ]
  
  plot_list <- list()
  
  
  #BA3 has many more QC's switch to remove.
  
  
  
  QC_names <- c("_30_", "_300_", "_3000_", "_800_") 
  #Get Start/End measruments of QC Samples
  QC_40 <- c("QC_40", "QC_40_1")
  QC_160 <- c("QC_160", "QC_160_end")
  QC_3200 <-c("QC_3200")
  QC_8000 <- c("QC_8000", "QC_8000_end", "QC_8000_1")
  QC_30 <- c("QC_30", "QC_30_end", "QC_30_1")
  QC_300 <- c("QC_300", "QC_300_end", "QC_300_1")
  QC_3000 <- c("QC_3000", "QC_3000_end", "QC_3000_1")
  QC_800 <- c("QC_800", "QC_800_end", "QC_800_1")
  
  ind_40 <- which(data$SampleName %in% QC_40)
  ind_160 <- which(data$SampleName %in% QC_160)
  ind_3200 <- which(data$SampleName %in% QC_3200)
  ind_8000 <- which(data$SampleName %in% QC_8000)
  ind_30 <- which(data$SampleName %in% QC_30)
  ind_300 <- which(data$SampleName %in% QC_300)
  ind_3000 <- which(data$SampleName %in% QC_3000)
  ind_800 <- which(data$SampleName %in% QC_800)
  
  QC_30_data <- data[ind_30, ]
  QC_40_data <- data[ind_40, ]
  QC_160_data <- data[ind_160, ]
  QC_300_data <- data[ind_300, ]
  QC_3000_data <- data[ind_3000, ]
  QC_3200_data <- data[ind_3200, ]
  QC_800_data <- data[ind_800, ]
  QC_8000_data <- data[ind_8000, ]
  
  #Gerneral data munging and average values on same days
  QC_30_data <- by(QC_30_data, QC_30_data$Compound, function(x) FUN = {
    by(x,x$Date, function(x2) FUN = {
      mean(x2$Concentration, na.rm=TRUE)
    })
  })
  
  QC_40_data <- by(QC_40_data, QC_40_data$Compound, function(x) FUN = {
    by(x,x$Date, function(x2) FUN = {
      mean(x2$Concentration, na.rm=TRUE)
    })
  })

  QC_160_data <- by(QC_160_data, QC_160_data$Compound, function(x) FUN = {
    by(x,x$Date, function(x2) FUN = {
      mean(x2$Concentration, na.rm=TRUE)
    })
  })
  
  QC_3200_data <- by(QC_3200_data, QC_3200_data$Compound, function(x) FUN = {
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
  
  QC_8000_data <- by(QC_8000_data, QC_8000_data$Compound, function(x) FUN = {
    by(x,x$Date, function(x2) FUN = {
      mean(x2$Concentration, na.rm=TRUE)
    })
  })
  

  
  #Appending like data
 # QC_30_data <- melt(do.call(rbind, QC_30_data))
#  QC_30_data$SampleName <- "QC_30"
 # QC_300_data <- melt(do.call(rbind, QC_300_data))
#  QC_300_data$SampleName <- "QC_300"
  QC_3000_data <- melt(do.call(rbind, QC_3000_data))
  QC_3000_data$SampleName <- "QC_3000"
  
  QC_800_data <- melt(do.call(rbind, QC_800_data))
  QC_800_data$SampleName <- "QC_800"
  
  QC_8000_data <- melt(do.call(rbind, QC_8000_data))
  QC_8000_data$SampleName <- "QC_8000"
  
  QC_40_data <- melt(do.call(rbind, QC_40_data))
  QC_40_data$SampleName <- "QC_40"
  
  QC_160_data <- melt(do.call(rbind, QC_160_data))
  QC_160_data$SampleName <- "QC_160"
  
  QC_3200_data <- melt(do.call(rbind, QC_3200_data))
  QC_3200_data$SampleName <- "QC_3200"
  
  
  merged_data <- rbind( QC_40_data,QC_160_data, QC_3000_data, QC_3200_data, QC_800_data, QC_8000_data)
  colnames(merged_data) <- c("Compound", "Date", "Concentration","SampleName")
  #Setting up data frame, converting dates to machine readable
  merged_data <- as.data.frame(merged_data)
  merged_data$Year <- year(merged_data$Date)
  merged_data$Month <- month(merged_data$Date)
  merged_data$Date <- as.POSIXct(merged_data$Date)
  
  merged_data %<>% subset(Concentration!=0)
  #Section to calculate total mean, standard deviation
  
  QC_stats <- by(data=merged_data, merged_data$SampleName, function(x) FUN = {
    by(x, x$Compound, function(y) FUN = {
      tmp <- y
      tmp$Date <- as.POSIXlt(tmp$Date)
      #Outlier removal for stats
      tmp <- remove_outliers(tmp)
      mean_dat <- data.frame(low=mean(tmp$Concentration)-2*sd(tmp$Concentration), 
                             mean=mean(tmp$Concentration),high=mean(tmp$Concentration)+2*sd(tmp$Concentration))
    }
    )})
  #Yearly total plots
  if(TRUE){
    ff <- by(data=merged_data, merged_data$SampleName, function(x) FUN = {
      by(x, x$Compound, function(y) FUN = {
        tmp <- y
        tmp$Date <- as.POSIXlt(tmp$Date)
        tmp <- remove_outliers(tmp)
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
  }
  
  
 
  
  
  #Generate monthly plots
  if(FALSE){
    plots_only <- by(data = merged_data, merged_data$SampleName, function(x) FUN = {
      by(x,x$Compound, function(y) FUN = {
        by(y,y$Year, function(z) FUN = {
          by(z,z$Month, function(.z) FUN = {
            tmp <- .z
            p1 <- ggplot(data=tmp) +  geom_line(aes(x=Date,y=Concentration)) + geom_point(aes(x=Date,y=Concentration)) + 
              geom_line(aes(x=Date,y=mean(Concentration))) + geom_line(aes(x=Date, y=mean(Concentration)+2*sd(Concentration))) +
              geom_line(aes(x=Date, y=mean(Concentration)-2*sd(Concentration)))+
              geom_line(aes(x=Date, y=mean(Concentration)+1*sd(Concentration)), linetype=2)+
              geom_line(aes(x=Date, y=mean(Concentration)-1*sd(Concentration)), linetype=2)+
              geom_line(aes(x=Date, y=mean(Concentration)+3*sd(Concentration)), linetype="dotdash")+
              geom_line(aes(x=Date, y=mean(Concentration)-3*sd(Concentration)), linetype="dotdash")+
              scale_x_datetime(breaks=date_breaks("3 days")) + 
              labs(x="Date", y="Concentration (ng/ml)", title=paste(unique(tmp$SampleName), unique(tmp$Compound), sep=" "))+
              theme(text=element_text(size=15), axis.text.x = element_text(size=15, angle = 45, hjust = 1))
          })
        } )
      })
    })
  }
  #Generate montly plots and tables
  if(TRUE){
  plots_tables <- by(data = merged_data, merged_data$SampleName, function(x) FUN = {
    tmp_sn <- x
    by(x,x$Compound, function(y) FUN = {
      y <- remove_outliers(y)
      by(y,y$Year, function(z) FUN = {
        by(z,z$Month, function(.z) FUN = {
          low=mean(y$Concentration)-2*sd(y$Concentration)
          low1SD=mean(y$Concentration)-1*sd(y$Concentration)
          middle=mean(y$Concentration)
          high=mean(y$Concentration)+2*sd(y$Concentration)
          high1SD=mean(y$Concentration)+1*sd(y$Concentration)
          tmp <- .z
          p1 <- ggplot(data=tmp) +  geom_line(aes(x=Date,y=Concentration)) + geom_point(aes(x=Date,y=Concentration)) + 
            geom_line(aes(x=Date,y=middle)) + geom_line(aes(x=Date, y=low)) +
            geom_line(aes(x=Date, y=high))+
            geom_line(aes(x=Date,y=low1SD),linetype=2)+
            geom_line(aes(x=Date,y=high1SD), linetype=2)+
            #scale_x_datetime(breaks=tmp$Date,labels = tmp$Date)+
            scale_x_datetime(breaks=date_breaks("3 days")) + 
            labs(x="",y="Concentration (ng/ml)", title=paste(unique(tmp$SampleName), unique(tmp$Compound), sep=" "))+
            theme(text=element_text(size=15), axis.text.x = element_text(size=15, angle = 45, hjust = 1)) + theme_grey()
          rownames(tmp) <- seq(1:dim(tmp)[1])
          t1 <- tableGrob(tmp[, c("Date", "Concentration")])
          fail_row <- which(tmp$Concentration>high | 
                          tmp$Concentration < low)+1
          fail_val_ind <- mapply(function(x) FUN = findCell(t1,x,3), fail_row)
          fail_date_ind <- mapply(function(x) FUN = findCell(t1,x,2), fail_row)
          fail_ind <- c(fail_val_ind, fail_date_ind)
          t1 <- modifyCells(t1,fail_ind, gpar(fill="darkolivegreen1", col="red4", lwd=5, fontface="bold"))
          arrangeGrob(p1,t1, ncol=2)
          tmp_plot <- plot_grid(p1,t1,ncol=2,rel_widths  = c(3/4,1/4))
        })
      } )
    })
  })
  
  }
  

  
  return(list(QC_stats=QC_stats,plots_tables=plots_tables))
  
}

readFiles <- function(){
  
  BA1_path <- "Data/BA1/"
  BA2_path <- "Data/BA2/"
  BA3_path <- "Data/BA-3 Data/AllDat/"
  #Parse file names to dates
  BA1_Files <- list.files(path = "Data/BA1")
  BA2_Files <- list.files(path= "Data/BA2")
  BA3_Files <-list.files(path = "Data/BA-3 Data/AllDat/")
  
  BA1_Dates <- as.numeric(gsub("([0-9]+).*$", "\\1", BA1_Files))
  BA2_Dates <- as.numeric(gsub("([0-9]+).*$", "\\1", BA2_Files))
  BA3_Dates <- as.numeric(gsub("([0-9]+).*$", "\\1", BA3_Files))
  
  BA1_Dates <- paste("20", BA1_Dates, sep = "")
  BA2_Dates <- paste("20", BA2_Dates, sep = "")
  BA3_Dates <- paste("20", BA3_Dates, sep = "")
  
  
  BA1_Dates <- as.Date(BA1_Dates, "%Y%m%d")
  BA2_Dates <- as.Date(BA2_Dates, "%Y%m%d")
  BA3_Dates <- as.Date(BA3_Dates, "%Y%m%d")
  
  
  #Create data frame of all samples
  BA1_DF <- data.frame()
  for(i in 1:length(BA1_Dates)){
    x=BA1_Files[i]
    y=BA1_Dates[i]
    tmploc <- paste(BA1_path, x, sep = "")
    tmpdf <- as.data.frame(read.csv(tmploc))
    tmpdf$Date <- y
    BA1_DF <- rbind(BA1_DF, tmpdf)
  }
  
  BA2_DF <- data.frame()
  for(i in 1:length(BA2_Dates)){
    x=BA2_Files[i]
    y=BA2_Dates[i]
    tmploc <- paste(BA2_path, x, sep = "")
    tmpdf <- as.data.frame(read.csv(tmploc))
    names(tmpdf) <- c("SampleName", "Compound", "Concentration")
    tmpdf$Date <- y
    BA2_DF <- rbind(BA2_DF, tmpdf)
  }
  BA3_DF <- data.frame()
  
  for(i in 1:length(BA3_Files)){
    x=BA3_Files[i]
    y=BA3_Dates[i]
    tmploc <- paste(BA3_path, x, sep = "")
    tmpdf <- as.data.frame(read.csv(tmploc))
    names(tmpdf) <- c("SampleName", "Compound", "Concentration")
    tmpdf$Date <- y
    BA3_DF <- rbind(BA3_DF, tmpdf)
  }
  
  #Subset the data for Cal and QCs
  BA1_use <- BA1_DF[c(grep("Cal", BA1_DF$SampleName), grep("QC", BA1_DF$SampleName)), ]
  BA2_use <- BA2_DF[c(grep("Cal", BA2_DF$SampleName), grep("QC", BA2_DF$SampleName)), ]
  BA3_use <- BA3_DF[c(grep("Cal", BA3_DF$SampleName), grep("QC", BA3_DF$SampleName)), ]
  
  
  
  return(list(BA1_Data=BA1_use, BA2_Data=BA2_use, BA3_Data=BA3_use))
  
  
}
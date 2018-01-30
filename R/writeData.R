writeData <- function(dat, directory){
  
  setwd("Figures/")
  dir.create(directory)
  setwd(directory)
  QC_names <- names(dat[[2]])
  #This will probably be too many nested loops... oh well. 
  for (i in QC_names){
    dir.create(i)
    setwd(i)
    QC_dat <- dat[[1]]
    QC_dat <- QC_dat[[i]]
    tmp_data <- do.call(rbind,QC_dat)
    write.csv(tmp_data, file = paste(i,"stats.csv"))
    drug_names <- names(QC_dat)
    QC_plots <-dat[[2]][[i]]
    mapply(function(x) FUN = dir.create(x), drug_names)
    for(j in drug_names){
      dir.create(j)
      setwd(j)
      filename = paste(i,"-",j,".pdf")
      pdf(filename, width = 12,height = 12)
      drug_dat <- QC_plots[[j]]
      lapply(names(drug_dat), function(.x) FUN = {
        drug_year_dat <- drug_dat[[.x]]
        lapply(names(drug_year_dat), function(.y) FUN = {
          drug_ym_dat <- drug_year_dat[[.y]]
          print(drug_ym_dat)
        })
      })
      dev.off()
      setwd("../")
    }
    setwd("../")
  }
}
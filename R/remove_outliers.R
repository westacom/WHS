remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x$Concentration, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x$Concentration, na.rm = na.rm)
  y <- x
  
  y %<>% subset(Concentration<(qnt[2]+H) & Concentration > qnt[1]-H)
  y %<>% subset(Concentration!=0)
  return(y)
}
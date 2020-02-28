#' Function for ploting the transformations
#' 
#' This functions plots 4 posible transformations
#' @param variable name of the variable to transform, must be continous
PROC.plot.t <- function(variable){
  par(mfrow=c(2,2))
  logd <- density(log(variable),na.rm = T)
  plot(logd,main ="log transformation")
  polygon(logd,col="lightblue")
  
  log10d <- density(log10(variable),na.rm = T)
  plot(log10d,main ="log10 transformation")
  polygon(log10d,col="lightblue")
  
  sqrtd <- density(sqrt(variable),na.rm = T)
  plot(sqrtd,main ="square-root transformation")
  polygon(sqrtd,col="lightblue")
  
  d2 <- density((variable)^2,na.rm = T)
  plot(d2,main ="Square transformation")
  polygon(d2,col="lightblue")
}
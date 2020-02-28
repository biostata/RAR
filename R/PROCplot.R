#' Function for continuous variables plots.
#'
#' This function provides visualization for the distribution of a continous variable
#' @param variable variable to use, must be continous
PROC.plot <- function(variable){
  par(mfrow=c(2,2),oma=c(0,0,2,0))
  hist(variable,main = " ",col = "lightblue")
  boxplot(variable,col = "lightblue")
  d <- density(variable,na.rm = T)
  plot(d,main ="")
  polygon(d,col="lightblue")
  qqnorm(variable,col = "blue")
  title(main = paste("Distribution of",deparse(substitute(variable))),outer = T)
}
#' Function for transforming the data:
#'
#' Provides a table with some of the parameters for the transformations made (log10, ln, squrt and sqr)
#' @param variable name of the variable to transform
PROC.transformations <- function(variable){
  DF <- data.frame(rbind.data.frame(PROC.univar(log(variable+1)),PROC.univar(log10(variable+1)), PROC.univar(sqrt(variable)), PROC.univar((variable)^2)),row.names = c("ln","log10","sqrt","sqr"))
  return(DF[,-1])
}

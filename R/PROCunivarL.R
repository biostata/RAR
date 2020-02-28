#' Univariate for a list of variables
#'
#' Provides a summary descriptibe table for a list of variables
#' @param variables must be a string of the variables name as on the data
#' @param data name of the data from where the variables are from
PROC.univar.L <- function(variables,data){
  V <- as.vector(data[,variables[1]])
  DF <-PROC.univar(V)
  for(i in 2:length(variables)){
    V <- data[,variables[i]]
    df <- PROC.univar(V)
    DF <- rbind(DF,df)
  }
  DF[,1] <- variables
  return(DF)
}

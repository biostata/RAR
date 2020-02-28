#' Function for summary tables
#'
#' Results a summary descriptive table for a variable, could be either continous or categorical
PROC.univar <- function(variable,is.categorical=F){
  if (class(variable)=="integer" |class(variable)=="numeric") {
    if (is.categorical==F) {
      Variable <- deparse(substitute(variable))
      Mean <- mean(variable,na.rm = T)
      Median <- median(variable,na.rm = T)
      Variance <- var(variable,na.rm = T)
      NAs <- length(variable[is.na(variable)])
      Min <- min(variable,na.rm = T)
      Max <- max(variable,na.rm = T)
      Skewness <- moments::skewness(variable,na.rm = T)
      W <- shapiro.test(variable)$'statistic'
      p <- round(shapiro.test(variable)$'p.value',4)
      N <- length(variable)-NAs
      DF <- data.frame(Variable,N,Mean,Median,Variance,NAs,Min,Max,Skewness,W,p,row.names = NULL)
      return(DF)
    }
    else if(is.categorical==T){
      d <- data.frame(table(variable,useNA = "always"))
      d$Proportion <- round(d$Freq/length(variable),2)
      d$CumFreq <- cumsum(d$Freq)
      d$CumProportion <- round(d$CumFreq/length(variable),2)
      DF <- data.frame(d)
      return(DF)
    }
  }
  else if (class(variable)=="factor"){
    d <- data.frame(table(variable,useNA = "always"))
    d$Proportion <- round(d$Freq/length(variable),2)
    d$CumFreq <- cumsum(d$Freq)
    d$CumProportion <- round(d$CumFreq/length(variable),2)
    DF <- data.frame(d)
    return(DF)
  }

}

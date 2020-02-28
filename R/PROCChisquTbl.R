#' Table for multiple chi square tests
#'
#' This function provides a table with multiple chi suare tests results
#' @param variables a vector of variable names to be analyzed
#' @param Outcome the outcome of interest to be analyzed
#' @param data name of the data that contains the variables
#' @param alpha 0.05 by default
PROC.ChisqTbl <- function(variables,outcome,Data,alpha=0.05){
  o <- Data[,outcome]
  df <- cbind("Variable","T","p","smallCells")
  for(i in 1:length(variables)){
    v <- Data[,variables[i]]


    tbl <- xtabs(~o+v,Data)
    ch <- chisq.test(tbl)
    TS <- round(ch$statistic,4)
    pval <- round(ch$p.value,4)
    variable <- variables[i]
    s <- ifelse(pval<=alpha,"*"," ")
    pval <-paste(pval,s)
    #small number cells:
    SmallCells <- ifelse(length(which(as.vector(tbl)<=6))>=1,"No","Yes")

    d <- cbind(variable,TS,pval,SmallCells)

    df <- rbind(df,d)
  }
  return(data.frame(df[-1,],row.names = NULL))
}

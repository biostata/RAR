#' Multiple Bivariate regressions table
#'
#' This function provides a table of the estimates of a bivariate regression (either linear or logistic) with the exposure of interest
#' @param variables a vector of variable names to be analyzed
#' @param Outcome the outcome of interest to be analyzed
#' @param data name of the data that contains the variables
#' @param alpha 0.05 by default
#' @param family "gaussian" by default, specifies the type of regression. Currently supports "gaussian", "binomial", and "poisson"
#' @param Offset if family = "poisson", include the name of the variable that will be used as offset.
PROC.BivariateRegression <- function(variables,outcome,data,alpha = 0.05, family = "gaussian", Offset= Offset){
  o <- data[,outcome]
  df <- c("variable","Estimate","LCI","UCI","StdError","T.S","pval")

  if(family == "gaussian"){
    for (i in 1:length(variables)) {
      v <-  data[,variables[i]]
      reg <- lm(o~v,data=data)
      regs <- summary(reg)

      if (class(v)=="factor"){
        lvls <- levels(v)
        Variable <- paste(variables[i],lvls[-1])
        Estimate <- round(regs$coefficients[2:length(lvls),1],2)
        StdError <- round(regs$coefficients[2:length(lvls),2],2)
        CI <- round(confint(reg),2)
        LCI <- CI[2:length(lvls),1]
        UCI <- CI[2:length(lvls),2]
        Tval <- round(regs$coefficients[2:length(lvls),3],2)
        pval <- round(regs$coefficients[2:length(lvls),4],2)
        s <- ifelse(pval<=alpha,"*"," ")
        pval <-paste(pval,s)
      }
      else if(class(v)=="numeric"|class(v)=="integer"){
        Variable <- paste(variables[i])
        Estimate <- round(regs$coefficients[2,1],2)
        StdError <- round(regs$coefficients[2,2],2)
        CI <- round(confint(reg),2)
        LCI <- CI[2,1]
        UCI <- CI[2,2]
        Tval <- round(regs$coefficients[2,3],2)
        pval <- round(regs$coefficients[2,4],2)
        s <- ifelse(pval<=alpha,"*"," ")
        pval <-paste(pval,s)
      }

      d <- cbind(Variable,Estimate,LCI,UCI,StdError,Tval,pval)
      # return(data.frame(Estimate,StdError,Tval,pval,r2))
      df <- rbind(df,d)
    }
  }

  else if(family == "binomial"){
    for (i in 1:length(variables)) {
      v <-  data[,variables[i]]
      reg <- glm(o~data[,variables[i]],data=data,family = binomial())
      regs <- summary(reg)

      if (class(v)=="factor"){
        lvls <- levels(v)
        Variable <- paste(variables[i],lvls[-1])
        Estimate <- round(regs$coefficients[2:length(lvls),1],2)
        StdError <- round(regs$coefficients[2:length(lvls),2],2)
        CI <- round(confint(reg),2)
        LCI <- CI[2:length(lvls),1]
        UCI <- CI[2:length(lvls),2]
        Tval <- round(regs$coefficients[2:length(lvls),3],2)
        pval <- round(regs$coefficients[2:length(lvls),4],2)
        s <- ifelse(pval<=alpha,"*"," ")
        pval <-paste(pval,s)
      }
      else if(class(v)=="numeric"|class(v)=="integer"){
        Variable <- paste(variables[i])
        Estimate <- round(regs$coefficients[2,1],2)
        StdError <- round(regs$coefficients[2,2],2)
        CI <- round(confint(reg),2)
        LCI <- CI[2,1]
        UCI <- CI[2,2]
        Tval <- round(regs$coefficients[2,3],2)
        pval <- round(regs$coefficients[2,4],2)
        s <- ifelse(pval<=alpha,"*"," ")
        pval <-paste(pval,s)
      }
      d <- cbind(Variable,Estimate,LCI,UCI,StdError,Tval,pval)
      # return(data.frame(Estimate,StdError,Tval,pval,r2))
      df <- rbind(df,d)
    }
  }

  else if(family == "poisson"){
    Of <- data[,Offset]
    for (i in 1:length(variables)) {
      v <-  data[,variables[i]]
      reg <- glm(o~data[,variables[i]] + offset(log(Of)),data=data,family = poisson)
      regs <- summary(reg)

      if (class(v)=="factor"){
        lvls <- levels(v)
        Variable <- paste(variables[i],lvls[-1])
        Estimate <- round(regs$coefficients[2:length(lvls),1],2)
        StdError <- round(regs$coefficients[2:length(lvls),2],2)
        CI <- round(confint(reg),2)
        LCI <- CI[2:length(lvls),1]
        UCI <- CI[2:length(lvls),2]
        Tval <- round(regs$coefficients[2:length(lvls),3],2)
        pval <- round(regs$coefficients[2:length(lvls),4],2)
        s <- ifelse(pval<=alpha,"*"," ")
        pval <-paste(pval,s)
      }
      else if(class(v)=="numeric"|class(v)=="integer"){
        Variable <- paste(variables[i])
        Estimate <- round(regs$coefficients[2,1],2)
        StdError <- round(regs$coefficients[2,2],2)
        CI <- round(confint(reg),2)
        LCI <- CI[2,1]
        UCI <- CI[2,2]
        Tval <- round(regs$coefficients[2,3],2)
        pval <- round(regs$coefficients[2,4],2)
        s <- ifelse(pval<=alpha,"*"," ")
        pval <-paste(pval,s)
      }
      d <- cbind(Variable,Estimate,LCI,UCI,StdError,Tval,pval)
      # return(data.frame(Estimate,StdError,Tval,pval,r2))
      df <- rbind(df,d)
    }
  }

  return(data.frame(df,row.names = NULL)[-1,])
}

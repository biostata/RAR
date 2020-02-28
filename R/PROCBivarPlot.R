#' Bivariate plots
#'
#' Function for creating informative bivariate plots
#' @param Exposure Exposure of interest, must be a string of characters
#' @param Outcome Outcome of interest, must be a string of characters
#' @param Data Dataset to use
#' @param alpha 0.05 by default


PROC.Bivar.Plot <- function(Exposure,Outcome,Data,alpha=0.05){
  #Get the data (easier for printing names purposes)
  E <- Data[,Exposure]
  O <- Data[,Outcome]
  if(class(E)=="factor"& (class(O)=="numeric" | class(O)=="integer")){

    if (length(levels(E))<=2) {
      #get T-test:
      t <- t.test(O~E)
      #Boxplot:
      boxplot(O~E,col="light blue",border="darkblue",main= paste("Distribution of ",Exposure," by ",Outcome),xlab=Outcome,ylab=Exposure)
      #Add T-test and p-value
      p <- t$p.value
      TS <- t$statistic
      s <- ifelse(p>alpha,"","*")
      mtext(paste("p-val:",round(p,4),s,', ',"T-stat:",round(TS,4)),side=1,font=2,line=2,cex=0.8)
    }
    else if (length(levels(E))>=3){
      #get ANOVA:
      AOV <- aov(O~E)
      AOVs <- summary(AOV)
      #Boxplot:
      boxplot(O~E,col="light blue",border="darkblue",main= paste("Distribution of ",Exposure," by ",Outcome),xlab=Outcome,ylab=Exposure)
      #Add T-test and p-value
      p <- AOVs[[1]][[1,"Pr(>F)"]]
      TS <- AOVs[[1]][[1,"F value"]]
      s <- ifelse(p>alpha,"","*")
      mtext(paste("p-val:",round(p,4),s,', ',"T-stat:",round(TS,4)),side=1,font=2,line=2,cex=0.8)
    }
  }

  else if((class(E)=="numeric" |class(E)=="integer") & (class(O)=="numeric" |class(O)=="integer")){
    #correlation test:
    Ct <- cor.test(E,O)
    Ce <- Ct$estimate
    p <- Ct$p.value
    #Scatterplot:
    plot(O~E,col="darkblue",main=paste("Distribution of ",Exposure," by ",Outcome),xlab=Outcome,ylab=Exposure)
    mtext(paste("p-val:",round(p,4),', ',"Pearson correltion:",round(Ce,4)),side=1,font=2,line=2,cex=0.8)
  }

}

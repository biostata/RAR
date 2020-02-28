#'  Bivariate analysis for catevorical vs Categorical
#'
#'
#' Function for 2x2 tables
#' @param Outcome Outcome of interest, must be a string of characters
#' @param Exposure Exposure of interest, must be a string of characters
#' @param Data Dataset that includes the variables
#' @param alpha 0.05 by default
PROC.Bivar <- function(Outcome,Exposure,Data,alpha=0.05){
  O <- Data[,Outcome]
  E <- Data[,Exposure]
  tbl <- xtabs(~O+E,data=Data)
  chsqt <- chisq.test(tbl)
  TS <- chsqt$statistic
  p <- chsqt$p.value
  s <- ifelse(p>alpha,"","*")
  n <- length(levels(Data[,Exposure]))
  knitr::kable(tbl,align="c",format = 'latex',booktabs=T) %>% kableExtra::kable_styling(bootstrap_options = "stripped",full_width = F) %>% kableExtra::add_header_above(c(" ",Exposure=n)) %>% kableExtra::kable_styling(bootstrap_options = "stripped",full_width = F,position = 'center') %>% kableExtra::footnote(general=paste("Chi-square=",round(TS,4),',',"p-val=",round(p,4),s))
}

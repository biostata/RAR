#' Table formating
#'
#' Function for creating tables for kniting in a nice format
#'  @param table name of the table object
#'  @param Head if you want to show only the first observations, default is F
#'  @param n if Head=T specify the number of rows to show, default is 5
#'  @param scale scale to fit the margins, use when number of variables is large
#'  @keywords Table
#'  @examples
#'  Pable(Table)
Pable <- function(table,Head=F,n=5,scale=F,Format="markdown"){
  if(Head==F){
    if(scale==F){
      knitr::kable(table,format=Format,booktabs=T) %>% kableExtra::kable_styling()
    }
    else if(scale==T){
      knitr::kable(table,format=Format,booktabs=T) %>% kableExtra::kable_styling(latex_options="scale_down")
    }
  }
  else if(Head==T){
    if(scale==F){
      knitr::kable(head(table,n),format=Format,booktabs=T) %>% kableExtra::kable_styling()
    }
    else if(scale==T){
      knitr::kable(head(table,n),format=Format,booktabs=T) %>% kableExtra::kable_styling(latex_options="scale_down")
    }

  }
}

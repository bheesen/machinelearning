#' Function to evaluate a machine learning model the correlation for numeric variables
#'
#' @param modell is the name of a machine learning model
#' @param daten is a dataframe used for prediction
#' @param ziel.spalte is the column number of the variable to be predicted
#' @keywords machine learning, modeling, evaluation
#' @return model name, dataframe name, rmse & rsq
#' @example ml.eval(wfl.rg.lm.fit,autos.train)
#' @export
ml.eval <- function(modell,daten,ziel.spalte) {
  modell.name<-deparse(substitute(modell))
  daten.name <-deparse(substitute(daten))
  daten<-rename(daten,x=colnames(daten)[ziel.spalte])
  df<-modell %>%
    predict(daten) %>%
    bind_cols(daten %>% select(x))
  if (colnames(df[1]) == ".pred") {
    rmse<-yardstick::rmse(data=df,x,.pred) %>%
      select(-.estimator) 
    rsq<-yardstick::rsq(data=df,x,.pred) %>%
      select(-.estimator)
    kennzahlen<-as.data.frame(rbind(rmse,rsq)) 
    df <- data.frame(Modell=modell.name,
                     Daten=daten.name,
                     Kennzahl=kennzahlen$.metric,
                     Wert= kennzahlen$.estimate)
    return(df)
  } else {
    return("Funktion nur für Regressionsmodelle nutzbar")
  }  
}

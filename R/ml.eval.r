#' Function to evaluate a machine learning model the correlation for numeric variables
#'
#' @param modell is the name of a machine learning model
#' @param daten is a dataframe with data
#' @keywords machine learning, modeling, evaluation
#' @return model name, dataframe name, rmse & rsq
#' @example ml.eval(wfl.rg.lm.fit,autos.train)
#' @export
ml.eval <- function(modell,daten) {
  modell.name<-deparse(substitute(modell))
  daten.name <-deparse(substitute(daten))
  df<-modell %>%
    predict(daten) %>%
    bind_cols(daten %>% select(Preis)) 
  rmse<-yardstick::rmse(data=df,Preis,.pred) %>%
    select(-.estimator) 
  rsq<-yardstick::rsq(data=df,Preis,.pred) %>%
    select(-.estimator)
  kennzahlen<-as.data.frame(rbind(rmse,rsq)) 
  df <- data.frame(Modell=modell.name,
                   Daten=daten.name,
                   Kennzahl=kennzahlen$.metric,
                   Wert= kennzahlen$.estimate)
  return(df)
}
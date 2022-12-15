#' Function to evaluate the quality of a prediction for numeric variables (regression)
#'
#' @param df is a dataframe with two variables: reak values, estimated values
#' @param titel is the name of the variable to be used as subtitle on diagrams
#' @param xvar name of the first variable
#' @param yvar name of the second variable
#' @param scatter if TRUE a scatter-chart is created
#' @keywords machine learning, modeling, evaluation, regression
#' @return rmse & rsq
#' @example ml.eval.lr(autos.eval[,c("Preis","Vorhersage")],"Gebrauchtwagen","realer Preis","vorhergesagter Preis",scatter=T)
#' @export
ml.eval.lr<-function (df,titel="",xvar="",yvar="",scatter=F) 
  # df:             Dataframe mit 2 Variablen, 1.Wahrheit, 2.Schätzwerte
  # titel:          Untertitel 
  # xvar,yvar:      Variablennamen
  # scatter:        TRUE=Scatter-Chart visualisieren   
{
  var.rmse<-NA
  colnames(df)<-c("x","y")
  rmse<-yardstick::rmse(df,truth=x,estimate=y) %>%
    select(-.estimator) 
  rsq <-yardstick::rsq(df,truth=x,estimate=y)%>%
    select(-.estimator) 
  if (scatter==TRUE){
    p.scatter<-ggplot(df)+                          
      aes(x=x,y=y)+
      geom_point(alpha=0.1)+
      geom_abline(lty=1)+
      labs(title="Evaluation des Modells",
           subtitle=paste0(titel,
                           " (RMSE= ",round(rmse$.estimate,2),
                           ", RSQ= ",round(rsq$.estimate,2),")"),
           x=xvar,y=yvar)
    grid.arrange(p.scatter,nrow=1,ncol=1)
    var.ev<-paste("Modellevaluations-Plot fehlerfrei")
  }
  kennzahlen<-as.data.frame(rbind(rmse,rsq)) 
  df <- data.frame(Kennzahl=kennzahlen$.metric,
                   Wert= kennzahlen$.estimate)
  return(df)
}
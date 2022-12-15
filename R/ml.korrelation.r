#' Function to calculate the correlation for numeric variables
#'
#' @param df is a dataframe with numeric variables
#' @param titel is the name of the variable to be used as subtitle on diagrams
#' @param var is a vector with the names of all variables in the dataframe df
#' @keywords descriptive, statistics, correlation, matrix
#' @return text with feedback to the function
#' @example ml.korrelation(mpg[,c(3,9,1)],"Autos",variablen3)
#' @export
ml.korrelation<-function (df,titel,var) 
  # df:             Dataframe mit n Variablen, die analysiert werden sollen
  # titel:          Untertitel 
  # var:            Variablennamen
{
  var.kor<- new("character")
  p.kor<-   new("character")
  farbe<-   0L
  if (length(var)>8 | ncol(df)>8){
    var.kor<-paste("Korrelations-Matrix nicht möglich: Mehr als 8 Variablen")
    return(var.kor)
  }
  if (length(var) != ncol(df)){
    var.kor<-paste("Korrelations-Matrix nicht möglich: Ungleiche Anzahl Variablen in df und var")
    return(var.kor)
  }
  else {
    var.kor<-paste("Korrelations-Matrix fehlerfrei")
    n<-length(var)
    for(i in 1:n) {
      if (is.character(df[[i]])) {
        df[[i]]<-as.factor(df[[i]])
        df[[i]]<-fct_infreq(df[[i]])
      }
      if (is.factor(df[[i]]) & length(levels(df[[i]])) > 10) {
        i.top10<-levels(df[[i]])[1:10]
        df<-filter(df,df[[i]] %in% i.top10)
        df[[i]]<-droplevels(df[[i]])
        var.kor<-paste("Korrelations-Matrix auf Top10 Levels beschränkt")
      } 
      if (is.factor(df[[i]])) {
        farbe<-i
      }
    }
    colnames(df)<-var
    if (farbe > 0){
      colnames(df)[farbe]<-"Kategorie"
      p.kor<-GGally::ggpairs(df,title=titel,
                             aes(color=Kategorie),
                             upper=list(continuous=GGally::wrap("cor",size=2)),
                             diag =list(continuous=GGally::wrap("densityDiag",alpha=0.5)))
    }
    else {
      p.kor<-GGally::ggpairs(df,title=titel)
    }
    #   cowplot::plot_grid(ggmatrix_gtable(p.kor),nrow=1)
  }  
  # cowplot::plot_grid(ggmatrix_gtable(p.kor),nrow=1)
  return(p.kor)
}
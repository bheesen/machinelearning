#' Function to calculate descriptive statistics for a variable
#'
#' @param variable is a vector of the variable
#' @param titel is the name of the variable to be used as title on diagrams
#' @param achse is the description of the axis on diagrams
#' @param bar if TRUE a bar-chart is created
#' @param barminmax defines how many bars shall be displayed
#' @param box if TRUE a box-plot is created
#' @param rel if TRUE a histogram with the relative values in & is created
#' @keywords descriptive, statistics, summary
#' @return a vector with a list of descriptive statistics
#' @example ml.summary(autos$Modell,"Autos","Modell","Anzahl",bar=T,barminmax=20)
#' @export
ml.summary<-function (variable,titel,achse,bar=F,barminmax=0,box=F,rel=F) 
  # variable: Variable, die analysiert und visualisiert werden soll
  # titel:    Untertitel
  # achse:    Achsenbeschriftung
  # bar:      TRUE=Bar-Chart visualisieren
  # barminmax:Anzahl der Bottom-x und Top-x häufigsten Werte
  # box:      TRUE=Boxplot visualisieren
  # rel:      TRUE=Relative Häufigkeit visualisieren
{
  var <- as.data.frame(table(variable))
  var.n <- length(variable)
  var.na <- sum(is.na(variable))
  var.maxanzahl <- max(var$Freq)
  var.suche <- dplyr::filter(var, var$Freq == var.maxanzahl)
  var.modus <- as.character(var.suche$variable)
  if (is.numeric(variable)) {
    var.median<- stats::median(variable,na.rm=TRUE)
    var.q1<-round(stats::quantile(variable,0.25,na.rm=TRUE),1)
    var.q3<-round(stats::quantile(variable,0.75,na.rm=TRUE),1)
    var.iqr<-var.q3-var.q1
    var.whisker.min<-var.q1-1.5*var.iqr
    var.whisker.max<-var.q3+1.5*var.iqr
    var.mean<-round(mean(variable,na.rm=TRUE),2)
    var.min <- min(variable,na.rm=TRUE)
    var.max <- max(variable,na.rm=TRUE)
    var.sd <-  round(stats::sd(variable,na.rm=TRUE),2)
    var.skewness <- round(e1071::skewness(variable, na.rm = TRUE),2)
    var.skewness.txt <- new("character")
    if (!is.na(var.skewness)) {
      if (var.skewness < 0) {
        var.skewness.txt <- paste("Linke Schiefe:", var.skewness, 
                                  "< 0, negative Schiefe,linksschief,rechtssteil")
      }
      else if (var.skewness > 0) {
        var.skewness.txt <- paste("Rechte Schiefe:", var.skewness, 
                                  "> 0, positive Schiefe,linkssteil,rechtsschief")
      }
      else {
        var.skewness.txt <- paste("Näherungsweise Normalverteilt. Schiefe:",
                                  var.skewness)
      }
    }
    else {
      var.skewness.txt <- NA
    }
    var.kurtosis <- round(e1071::kurtosis(variable, na.rm = TRUE),2)
    if (!is.na(var.kurtosis)) {
      if (var.kurtosis < 0) {
        var.kurtosis.txt <- paste("Flachgipflig mit Exzess Kurtosis", 
                                  var.kurtosis, "< 0")
      }
      else if (var.kurtosis > 0) {
        var.kurtosis.txt <- paste("Steilgipflig mit Exzess Kurtosis", 
                                  var.kurtosis, "> 0")
      }
      else {
        var.kurtosis.txt <- paste(
          "Näherungsweise Normalverteilt mit Exzess Kurtosis", 
          var.kurtosis)
      }
    }
    else {
      var.kurtosis.txt <- NA
    }
    #- Histogramm-----------------------------------------------------------------
    variable<-variable[!is.na(variable)]
    df<-as.data.frame(variable)
    colnames(df)<-c("daten")
    p.abs<-ggplot2::ggplot(df)+                               # Absolute Häufigkeit
      ggplot2::aes(x=daten)+                       
      ggplot2::geom_histogram(bins=100,col="white")+                       
      ggplot2::labs(title="Histogramm",
                    subtitle=titel,x=achse,y="Anzahl (Absolute Häufigkeit)")
    if (rel==TRUE){
      p.rel<-p.abs+ggplot2::aes(y=..density..)+               # Relative Häufikeit
        ggplot2::labs(y="Anteil (Relative Häufigkeit") 
      gridExtra::grid.arrange(p.abs,p.rel,nrow=1,ncol=2)
    }
    else {
      gridExtra::grid.arrange(p.abs,nrow=1,ncol=1)
    }
    #- Box-Plot-------------------------------------------------------------------
    if (box==TRUE){
      p.box<-ggplot2::ggplot(df)+                             # Boxplot
        ggplot2::aes(y=daten)+  
        ggplot2::labs(title="Box-Plot",
                      subtitle=titel,y=achse) +
        ggplot2::geom_boxplot() +
        ggplot2::theme(axis.title.x=element_blank(),          # x-Achse ohne
                       axis.text.x=element_blank(),           # Beschriftung
                       axis.ticks.x=element_blank())
      gridExtra::grid.arrange(p.box,nrow=1,ncol=1)
    }
  }  
  else{
    var.median<-NA
    var.q1<-NA
    var.q3<-NA
    var.iqr<-NA
    var.whisker.min<-NA
    var.whisker.max<-NA
    var.mean<-NA
    var.min<-NA
    var.max<-NA
    var.sd<-NA
    var.skewness<-NA
    var.skewness.txt<-NA
    var.kurtosis<-NA
    var.kurtosis.txt<-NA
  }
  #- Bar-Chart------------------------------------------------------------------
  if (bar==TRUE){
    df<-as.data.frame(variable)
    colnames(df)<-c("daten")
    tab.daten<-table(df$daten)
    df.x<-data.frame(x=tab.daten)
    colnames(df.x)<-c("y","x")
    p.bar<-ggplot2::ggplot(df.x)+                          
      ggplot2::aes(x=x,y=reorder(y,x),fill=y)+
      ggplot2::geom_bar(position="dodge",stat="identity")+
      ggplot2::labs(title="Bar-Chart",subtitle=titel,x="Anzahl",y=achse)+
      ggplot2::theme(legend.position = "none")
    gridExtra::grid.arrange(p.bar,nrow=1,ncol=1)
    if (barminmax>0){
      df.x.sort<-arrange(df.x,x)[1:barminmax,]
      p.bar<-ggplot2::ggplot(df.x.sort)+                          
        ggplot2::aes(x=x,y=reorder(y,x),fill=y)+
        ggplot2::geom_bar(position="dodge",stat="identity")+
        ggplot2::labs(title=paste0("Bar-Chart"),
             subtitle=paste0(titel," (Bottom ",barminmax," von ",nrow(df.x),")"),x="Anzahl",y=achse)+
        ggplot2::theme(legend.position = "none")
      gridExtra::grid.arrange(p.bar,nrow=1,ncol=1)
      df.x.sort<-arrange(df.x,x)[(nrow(df.x)-barminmax):nrow(df.x),]
      p.bar<-ggplot2::ggplot(df.x.sort)+                          
        ggplot2::aes(x=x,y=reorder(y,x),fill=y)+
        ggplot2::geom_bar(position="dodge",stat="identity")+
        ggplot2::labs(title=paste0("Bar-Chart"),
             subtitle=paste0(titel," (Top ",barminmax," von ",nrow(df.x),")"),x="Anzahl",y=achse)+
        ggplot2::theme(legend.position = "none")
      gridExtra::grid.arrange(p.bar,nrow=1,ncol=1)
    }
    var.bar<-paste("Barchart fehlerfrei")
  }
  var.gesamt <- rbind(var.n, var.na, 
                      var.modus, var.median, var.mean, 
                      var.min, var.max, var.sd,
                      var.q1, var.q3, var.iqr,
                      var.whisker.min, var.whisker.max,
                      var.skewness, var.skewness.txt, 
                      var.kurtosis, var.kurtosis.txt)
  rownames(var.gesamt) <- c("anzahl", "anzahl.na",
                            "modus", "median", "mean",
                            "min", "max", "sd",
                            "q1", "q3", "iqr",
                            "whisker.min", "whisker.max",
                            "skewness", "skewness.txt", 
                            "kurtosis", "kurtosis.txt")
  return(var.gesamt)
}
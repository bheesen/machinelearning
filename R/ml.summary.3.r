#' Function to calculate descriptive statistics for three variables
#'
#' @param df is a dataframe with three variables
#' @param titel is the name of the variable to be used as subtitle on diagrams
#' @param xvar name of the first variable
#' @param yvar name of the second variable
#' @param zvar name of the third variable
#' @param hist if TRUE a histogram is created
#' @param bar if TRUE a bar-chart is created
#' @param line if TRUE a line-chart is created
#' @param box if TRUE a box-plot is created
#' @param scatter if TRUE a scatter-chart is created
#' @param kor if TRUE a correlation matrix is created
#' @keywords descriptive, statistics, correlation, plot
#' @return text with feedback to the function
#' @example ml.summary.3(mpg[,c(3,1,11)],"Autos","Motorgröße in Liter","Marke","Fahrzeugtyp",box=T)
#' @export
ml.summary.3<-function (df,titel,xvar,yvar,zvar,
                        hist=F,bar=F,line=F,box=F,scatter=F,kor=F) 
  # df:             Dataframe mit 3 Variablen, die analysiert werden sollen
  # titel:          Untertitel 
  # xvar,yvar,zvar: Variablen
  # hist:           TRUE=Histogramm visualisieren
  # bar:            TRUE=Bar-Chart visualisieren
  # line:           TRUE=Line-Chart visualisieren
  # box:            TRUE=Boxplot visualisieren
  # scatter:        TRUE=Scatter-Chart visualisieren
  # kor:            TRUE=Korrelations-Matrix visualisieren
{
  var.hist<-   new("character")
  var.bar<-    new("character")
  var.line<-   new("character")
  var.box<-    new("character")
  var.scatter<-new("character")
  var.kor<-    new("character")
  colnames(df)<-c("x","y","z")
  if (is.character(df$x)) {
    df$x<-as.factor(df$x)
    df$x<-fct_infreq(df$x)
  }
  if (is.character(df$y)) {
    df$y<-as.factor(df$y)
    df$y<-fct_infreq(df$y)
  }
  if (is.character(df$z)) {
    df$z<-as.factor(df$z)
    df$z<-fct_infreq(df$z)
  }
  if (is.numeric(df$x)) {
    #- Histogramm-----------------------------------------------------------------
    if (hist==TRUE){
      if (is.factor(df$y) & is.factor(df$z)) {
        if (length(levels(df$y)) > 10) {
          y.top10<-levels(df$y)[1:10]
          df.top10<-filter(df,df$y %in% y.top10)
          df.top10$y<-droplevels(df.top10$y)
          var.hist<-paste("Histogramm auf Top10 Levels beschränkt")
        }
        else {
          df.top10<-df
          var.hist<-paste("Histogramm fehlerfrei")
        }
        df.top10<-df.top10 %>% group_by(y,z)
        p.hist<-ggplot(df.top10)+           
          aes(x=x,fill=y)+
          geom_histogram(bins=20,col="white")+                       
          labs(title="Histogramm",
               subtitle=titel,
               x=xvar,y="Anzahl")+
          scale_fill_manual(name="Kategorie:",
                            values=colour.own.nomin.1)+
          facet_grid(y~z)
        grid.arrange(p.hist,nrow=1,ncol=1)
      }
      else {
        var.hist<-paste("Histogramm nicht möglich:",
                        "Zweite oder dritte Variable nicht nominal")
      }
    }
    #- Bar-Chart------------------------------------------------------------------
    if (bar==TRUE){
      if (is.factor(df$y) & is.factor(df$z)) {
        if (length(levels(df$y)) > 10 | length(levels(df$z)) > 10) {
          y.top10<-levels(df$y)[1:10]
          z.top10<-levels(df$z)[1:10]
          df.top10<-filter(df,df$y %in% y.top10 & df$z %in% z.top10)
          df.top10$y<-droplevels(df.top10$y)
          df.top10$z<-droplevels(df.top10$z)
          var.bar<-paste("Bar-Chart auf Top10 Levels beschränkt")
        }
        else {
          df.top10<-df
          var.bar<-paste("Bar-Chart fehlerfrei")
        }
        df.top10$x2 <- cut_number(df.top10$x,10)
        tab.x<-table(df.top10$x2)
        df.top10.x<-data.frame(x=tab.x,y=df.top10$y,z=df.top10$z)
        colnames(df.top10.x)<-c("x.kat","x","y","z")
        df.top10.n<-df.top10.x %>% group_by(x.kat,y,z) %>%
          summarize(n = n(),.groups="keep")
        p.bar<-ggplot(df.top10.n)+                          
          aes(x=n,y=x.kat,fill=y)+
          geom_bar(position="dodge",stat="identity")+
          labs(title="Bar-Chart",
               subtitle=titel, 
               x="Anzahl",y=paste("Kategorie",xvar))+
          scale_fill_manual(name="Kategorie:",
                            values=colour.own.nomin.1)+
          guides(fill=guide_legend(reverse=TRUE))
        grid.arrange(p.bar,nrow=1,ncol=1)
        p.bar<-p.bar+                          
          aes(x=n,y=x.kat,fill=z)+
          labs(x="Anzahl",y=paste("Kategorie",xvar))
        grid.arrange(p.bar,nrow=1,ncol=1)
        p.bar<-p.bar+                          
          aes(x=n,y=y,fill=x.kat)+
          labs(x="Anzahl",y=paste("Kategorie",yvar))
        grid.arrange(p.bar,nrow=1,ncol=1)
        p.bar<-p.bar+                          
          aes(x=n,y=z,fill=x.kat)+
          labs(x="Anzahl",y=paste("Kategorie",zvar))
        grid.arrange(p.bar,nrow=1,ncol=1)
        p.bar<-p.bar+                          
          aes(x=n,y=y,fill=z)+
          labs(x="Anzahl",y=paste("Kategorie",yvar))
        grid.arrange(p.bar,nrow=1,ncol=1)
        p.bar<-p.bar+                          
          aes(x=n,y=z,fill=y)+
          labs(x="Anzahl",y=paste("Kategorie",zvar))
        grid.arrange(p.bar,nrow=1,ncol=1)
      }
      else {
        var.bar<-paste("Barchart nicht möglich:",
                       "Zweite oder dritte Variable nicht nominal")
      }
    }
    if (line==TRUE){
      if (is.numeric(df$y) & is.factor(df$z)) {
        if (length(levels(df$z)) > 10) {
          z.top10<-levels(df$z)[1:10]
          df.top10<-filter(df,df$z %in% z.top10)
          df.top10$z<-droplevels(df.top10$z)
          var.line<-paste("Line-Chart auf Top10 Levels beschränkt")
        }
        else {
          df.top10<-df
          var.bar<-paste("Line-Chart fehlerfrei")
        }
        p.line<-ggplot(df.top10)+                          
          aes(x=x,y=y,color=z)+
          geom_line()+                       
          labs(title="Line-Chart",
               subtitle=titel,
               x=xvar,y=yvar)+
          scale_colour_manual(name="Kategorie:",
                              values=colour.own.nomin.1)+
          guides(fill=guide_legend(reverse=TRUE))
        grid.arrange(p.line,nrow=1,ncol=1)
      }
      else {
        var.line<-paste("Line-Chart nicht möglich: Zweite Variable nicht numerisch oder dritte nicht nominal")
      }
    }
    if (box==TRUE){
      if (is.factor(df$y) & is.factor(df$z)) {
        if (length(levels(df$y)) > 10 | length(levels(df$z)) > 10) {
          y.top10<-levels(df$y)[1:10]
          z.top10<-levels(df$z)[1:10]
          df.top10<-filter(df,df$y %in% y.top10 & df$z %in% z.top10)
          df.top10$y<-droplevels(df.top10$y)
          df.top10$z<-droplevels(df.top10$z)
          var.bar<-paste("Box-Plot auf Top10 Levels beschränkt")
        }
        else {
          df.top10<-df
          var.bar<-paste("Box-Plot fehlerfrei")
        }
        p.box<-ggplot(df.top10)+                          
          aes(x=x,y=y,fill=y)+
          geom_boxplot()+
          labs(title="Box-Plot",
               subtitle=titel, 
               x=xvar, y=yvar)+
          scale_fill_manual(name="Kategorie:",
                            values=colour.own.nomin.1)+
          guides(fill=guide_legend(reverse=TRUE))
        grid.arrange(p.box,nrow=1,ncol=1)
        p.box<-ggplot(df.top10)+                          
          aes(x=x,y=z,fill=z)+
          geom_boxplot()+
          labs(title="Box-Plot",
               subtitle=titel, 
               x=xvar, y=zvar)+
          scale_fill_manual(name="Kategorie:",
                            values=colour.own.nomin.1)+
          guides(fill=guide_legend(reverse=TRUE))
        grid.arrange(p.box,nrow=1,ncol=1)
      }
      else {
        var.bar<-paste("Box-Plot nicht möglich: Zweite oder dritte Variable nicht nominal")
      }
    }
    if (scatter==TRUE){
      if (is.numeric(df$y) & is.factor(df$z)) {
        if (length(levels(df$z)) > 10) {
          z.top10<-levels(df$z)[1:10]
          df.top10<-filter(df,df$z %in% z.top10)
          df.top10$z<-droplevels(df.top10$z)
          var.line<-paste("Scatter-Plot auf Top10 Levels beschränkt")
        }
        else {
          df.top10<-df
          var.bar<-paste("Scatter-Plot fehlerfrei")
        }
        p.scatter<-ggplot(df.top10)+                          
          aes(x=x,y=y,color=z)+
          geom_point()+                       
          labs(title="Scatter-Plot",
               subtitle=titel,
               x=xvar,y=yvar)+
          scale_colour_manual(name="Kategorie:",
                              values=colour.own.nomin.1)+
          guides(fill=guide_legend(reverse=TRUE))
        grid.arrange(p.scatter,nrow=1,ncol=1)
        p.scatter<-p.scatter+                          
          geom_point(alpha=0.1)+                       
          labs(title="Scatter-Plot mit Regression",
               subtitle=titel,
               x=xvar,y=yvar)+
          geom_smooth(se=FALSE)
        grid.arrange(p.scatter,nrow=1,ncol=1)
        p.scatter<-ggplot(df.top10)+                          
          aes(x=x,y=y,color=z)+
          geom_point()+                       
          labs(title="Scatter-Plot",
               subtitle=titel,
               x=xvar,y=yvar)+
          scale_colour_manual(name="Kategorie:",
                              values=colour.own.nomin.1)+
          guides(fill=guide_legend(reverse=TRUE))+
          facet_wrap(~z,nrow=3)                       
        grid.arrange(p.scatter,nrow=1,ncol=1)
        p.scatter<-p.scatter+                       
          geom_point(alpha=0.1)+                       
          labs(title="Scatter-Plot mit Regression und Konfidenzintervall",
               subtitle=titel,
               x=xvar,y=yvar)+
          geom_smooth()+
          facet_wrap(~z,nrow=3)                       
        grid.arrange(p.scatter,nrow=1,ncol=1)
      }
      else {
        var.line<-paste("Scatter-Plot nicht möglich: Zweite Variable nicht numerisch oder dritte nicht nominal")
      }
    }
    if (kor==TRUE){
      var.kor<-paste("Korrelations-Matrix nicht möglich: Eine Variable nicht numerisch")
    }
  }  
  else{
    y<-1
  }
  var.text<-"Kommentare"
  var.gesamt<-rbind(var.text,var.hist,var.bar,var.line,var.box,
                    var.scatter,var.kor)
  return(var.gesamt)
}
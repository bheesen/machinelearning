#' Function to calculate descriptive statistics for two variables
#'
#' @param df is a dataframe with two variables
#' @param titel is the name of the variable to be used as subtitle on diagrams
#' @param xvar name of the first variable, a numeric variable
#' @param yvar name of the second variable
#' @param npos defines the x-value at which the number of items per category is displayed in a box-plot
#' @param dec defines the number of decimals behind the period
#' @param bw uses only black and white in a box-plot
#' @param hist if TRUE a histogram is created
#' @param bar if TRUE a bar-chart is created
#' @param line if TRUE a line-chart is created
#' @param box if TRUE a box-plot is created
#' @param scatter if TRUE a scatter-chart is created
#' @param kor if TRUE a correlation matrix is created
#' @keywords descriptive, statistics, correlation, plot
#' @return text with feedback to the function
#' @example ml.summary.2(mpg[,c(3,1)],"Autos","Motorgröße in Liter","Marke",box=T)
#' @export
ml.summary.2<-function (df,titel,xvar,yvar,npos=0,dec=0,bw=F,
                        hist=F,bar=F,line=F,box=F,scatter=F,kor=F) 
    # df:             Dataframe mit 2 Variablen, die analysiert werden sollen
    # titel:          Untertitel 
    # xvar,yvar:      Variablen
    # npos:           x-Wert, bei dem die Anzahl der Datensätze angezeigt wird
    # bw:             Schwarz/Weiß ohne Farbe
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
    colnames(df)<-c("x","y")
    if (is.character(df$x)) {
      df$x<-as.factor(df$x)
      df$x<-fct_infreq(df$x)
    }
    if (is.character(df$y)) {
      df$y<-as.factor(df$y)
      df$y<-fct_infreq(df$y)
    }
    if (is.numeric(df$x)) {
      #- Histogramm-----------------------------------------------------------------
      if (hist==TRUE){
        if (is.factor(df$y)) {
          p.hist<-ggplot(df)+                          
            aes(x=x,fill=y)+
            geom_histogram(bins=20,col="white")+                       
            labs(title="Histogramm",
                 subtitle=titel,
                 x=xvar,y="Anzahl")+
            scale_fill_manual(name="Kategorie:",
                              values=colour.own.nomin.1)+
            facet_wrap(~y)
          grid.arrange(p.hist,nrow=1,ncol=1)
          var.hist<-paste("Histogramm fehlerfrei")
        }
        else {
          var.hist<-paste("Histogramm nicht möglich: Zweite Variable nicht nominal")
        }
      }
      #- Bar-Chart------------------------------------------------------------------
      if (bar==TRUE){
        if(dim(table(df$x))>10){
          df$x2 <- cut_number(df$x,round(dim(table(df$x)/2,0))) # Gruppen bilden
        }
        else {
          df$x2 <- df$x
        }
        tab.x<-table(df$x2)
        df.x<-data.frame(x=tab.x)
        colnames(df.x)<-c("y","x")
        p.bar<-ggplot(df.x)+                          
          aes(x=x,y=y,fill=y)+
          geom_bar(position="dodge",stat="identity")+
          labs(title="Bar-Chart",
               subtitle=titel, 
               x="Anzahl",y=paste("Kategorie",xvar))+
          scale_fill_manual(name="Kategorie:",
                            values=colour.own.nomin.1)+
          guides(fill=guide_legend(reverse=TRUE))
        grid.arrange(p.bar,nrow=1,ncol=1)
        if (is.factor(df$y)) {
          tab.y<-table(df$y)
          df.y<-data.frame(x=tab.y)
          colnames(df.y)<-c("y","x")
          if (bw != TRUE) {
            p.bar<-ggplot(df.y)+                          
              aes(x=x,y=y,fill=y)+
              geom_bar(position="dodge",stat="identity")+
              labs(title="Bar-Chart",
                   subtitle=titel, 
                   x="Anzahl", y=paste("Kategorie",yvar))+
              scale_fill_manual(name="Kategorie:",
                                values=colour.own.nomin.1)+
              guides(fill=guide_legend(reverse=TRUE))
          } else {
            if (!is.ordered(df$y)) {
              df<- df %>% mutate(y = fct_reorder(y,x, .fun="max"))
            }
            p.bar<-ggplot(df)+                          
              aes(x=x,y=y)+
              geom_bar(position="dodge",stat="identity")+
              labs(title="Bar-Chart",
                   subtitle=titel, 
                   x="Anzahl", y=paste("Kategorie",yvar))+
              scale_fill_manual(name="Kategorie:")+
              guides(fill=guide_legend(reverse=TRUE))            
          }
          grid.arrange(p.bar,nrow=1,ncol=1)
          var.bar<-paste("Barchart fehlerfrei")
        }
        else {
          var.bar<-paste("Barchart nicht möglich: Zweite Variable nicht nominal")
        }
      }
      if (line==TRUE){
        if (is.numeric(df$y)) {
          p.line<-ggplot(df)+                          
            aes(x=x,y=y)+
            geom_line()+                       
            labs(title="Line-Chart",
                 subtitle=titel,
                 x=xvar,y=yvar)
          grid.arrange(p.line,nrow=1,ncol=1)
          var.line<-paste("Line-Chart fehlerfrei")
        }
        else {
          var.line<-paste("Line-Chart nicht möglich: Eine Variable nicht numerisch")
        }
      }
      if (box==TRUE){
        if (is.factor(df$y)) {
          df<-droplevels(df)
          if (!is.ordered(df$y)) {
            df<- df %>% mutate(y = fct_reorder(y,x, .fun="mean"))
          }
          df.x.mw<-round(mean(df$x,na.rm=TRUE),2)
          if (length(levels(df$y)) > 10 & bw!=TRUE) {
            von<-length(levels(df$y))-4
            bis<-length(levels(df$y))
            y.top10<-levels(df$y)[c(1:5,von:bis)]
            df.top10<-filter(df,df$y %in% y.top10)
            df.top10$y<-droplevels(df.top10$y)
            var.box<-paste("Box-Plot auf Top10 Levels beschränkt (Top5 + Bottom5)")
            df.top10.sum <- df.top10 %>% 
              group_by(y) %>% 
              summarise(n=n(),
                        mw=round(mean(x,na.rm=TRUE),dec))
            if (npos==0) {
              p.box<-ggplot(df.top10)+                          
                aes(x=x,y=y,fill=y)+
                geom_boxplot()+
                labs(title="Box-Plot",
                     subtitle=paste0(titel," (Mittelwert=",df.x.mw,")"), 
                     x=xvar, y=yvar)+
                scale_fill_manual(name="Kategorie:",
                                  values=colour.own.nomin.1)+
                guides(fill=guide_legend(reverse=TRUE))+
                geom_vline(xintercept=df.x.mw,linewidth=1.5,alpha=0.2)
            }
            else {
              p.box<-ggplot(df.top10)+                          
                aes(x=x,y=y,fill=y)+
                geom_boxplot()+
                labs(title="Box-Plot",
                     subtitle=paste0(titel," (Mittelwert=",df.x.mw,")"), 
                     x=xvar, y=yvar)+
                scale_fill_manual(name="Kategorie:",
                                  values=colour.own.nomin.1)+
                guides(fill=guide_legend(reverse=TRUE))+
                geom_vline(xintercept=df.x.mw,linewidth=1.5,alpha=0.2)+
                geom_label(data=df.top10.sum,aes(x=npos,label=paste("N:",n,"Mw:",mw)),fill=NA,size=rel(3),label.size=0.5)
            }
            grid.arrange(p.box,nrow=1,ncol=1)
          }
          else {
            df.top10<-df
            var.box<-paste("Box-Plot fehlerfrei")
            df.top10.sum <- df.top10 %>% 
              group_by(y) %>% 
              summarise(n=n(),
                        mw=round(mean(x,na.rm=TRUE),dec))
            if (npos==0) {
              p.box<-ggplot(df.top10)+                          
                aes(x=x,y=y)+
                geom_boxplot()+
                labs(title="Box-Plot",
                     subtitle=paste0(titel," (Mittelwert=",df.x.mw,")"), 
                     x=xvar, y=yvar)+
                geom_vline(xintercept=df.x.mw,linewidth=1.5,alpha=0.2)
            }
            else {  
              p.box<-ggplot(df.top10)+                          
                aes(x=x,y=y)+
                geom_boxplot()+
                labs(title="Box-Plot",
                     subtitle=paste0(titel," (Mittelwert=",df.x.mw,")"), 
                     x=xvar, y=yvar)+
                geom_vline(xintercept=df.x.mw,linewidth=1.5,alpha=0.2)+
                geom_label(data=df.top10.sum,aes(x=npos,label=paste("N:",n,"Mw:",mw)),fill=NA,size=rel(3),label.size=0.5)
            }
            grid.arrange(p.box,nrow=1,ncol=1)
          }
          var.box<-paste("Box-Plot fehlerfrei")
        }
        else {
          var.box<-paste("Box-Plot nicht möglich: Zweite Variable nicht nominal")
        }
      }
      if (scatter==TRUE){
        if (is.numeric(df$y)) {
          p.scatter<-ggplot(df)+                          
            aes(x=x,y=y)+
            geom_point()+                       
            labs(title="Scatter-Plot",
                 subtitle=titel,
                 x=xvar,y=yvar)
          grid.arrange(p.scatter,nrow=1,ncol=1)
          p.scatter<-p.scatter+
            labs(title="Scatter-Plot mit Regression und Konfidenzintervall",
                 subtitle=titel,
                 x=xvar,y=yvar)+
            geom_smooth()
          grid.arrange(p.scatter,nrow=1,ncol=1)
          var.scatter<-paste("Scatter-Plot fehlerfrei")
        }
        else {
          var.scatter<-paste("Scatter-Plot nicht möglich: Eine Variable nicht numerisch")
        }
      }
      if (kor==TRUE){
        if (is.numeric(df$y)) {
          colnames(df)<-c(xvar,yvar)
          p.kor<-GGally::ggpairs(df)
          cowplot::plot_grid(ggmatrix_gtable(p.kor),nrow=1)
          var.kor<-paste("Korrelations-Matrix fehlerfrei")
          return(p.kor)
        }
        else {
          var.kor<-paste("Korrelations-Matrix nicht möglich: Eine Variable nicht numerisch")
        }
      }
    }  
    else{
      y<-1 # nicht genutzt
    }
    var.text<-"Kommentare:"
    var.gesamt<-rbind(var.text,var.hist,var.bar,var.line,var.box,
                      var.scatter,var.kor)
    return(var.gesamt)
}

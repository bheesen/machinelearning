#' Function to evaluate a machine learning model the correlation for numeric variables
#'
#' @param wflset is the name of a trained machine learning workflow set
#' @param daten is a dataframe used for prediction
#' @keywords machine learning, modeling, evaluation
#' @return performance indicators rmse & rsq
#' @example ml.eval.wflowset(mod.wflset.1,"autos.fold")
#' @export
ml.eval.wflset <- function(modelle,daten) {
  kz<-collect_metrics(modelle)
  rmse<-filter(kz,.metric=="rmse") 
  rmse<-rmse %>%
    arrange(mean) %>%
    mutate(Platzierung=1:nrow(rmse))
  rmse<-rmse %>%
    mutate(Modell=paste0(Platzierung,". Platz\n",wflow_id))
  rmse<-rmse %>%
    mutate(Model=paste0(model,"\n",preproc)) %>%
    select(Platzierung,Modell,RMSE=mean,WorkflowID=wflow_id,Model)
  rsq <-filter(kz,.metric=="rsq")
  rsq<-rsq %>% 
    arrange(desc(mean)) %>%
    mutate(Platzierung=1:nrow(rsq))
  rsq<-rsq %>%
    mutate(Modell=paste0(Platzierung,". Platz\n",wflow_id))
  rsq<-rsq %>%
    mutate(Model=paste0(model,"\n",preproc)) %>%
    select(Platzierung,Modell,RSQ=mean,WorkflowID=wflow_id,Model)
  p.rmse<-ggplot(data=rmse,
                 aes(x=RMSE,
                     y=reorder(Modell,Platzierung,decreasing=T),
                     color=Model)) +
    geom_point(size=2)+
    geom_label(label=round(rmse$RMSE,2),
               nudge_x=(max(rmse$RMSE)-min(rmse$RMSE))/12,
               nudge_y=(max(rmse$Platzierung)-min(rmse$Platzierung))/
                 max(rmse$Platzierung)/3) +
    labs(x="RMSE",y="Modelle",
         title="Ranking der Modelle nach RMSE",  
         subtitle=paste("Datenbasis:",daten))+
    guides(fill=guide_legend(title="Model"))+
    scale_colour_manual(values=colour.own.nomin.2)
  p.rsq<-ggplot(data=rsq,
                aes(x=RSQ,
                    y=reorder(Modell,Platzierung,decreasing=T),
                    color=Model)) +
    geom_point(size=2)+
    geom_label(label=round(rsq$RSQ,2),
               nudge_x=(max(rsq$RSQ)-min(rsq$RSQ))/12,
               nudge_y=(max(rmse$Platzierung)-min(rmse$Platzierung))/
                 max(rmse$Platzierung)/3) +
    labs(x="RSQ",y="Modelle",
         title="Ranking der Modelle nach RSQ",  
         subtitle=paste("Datenbasis:",daten))+
    guides(fill=guide_legend(title="Model"))+
    scale_colour_manual(values=colour.own.nomin.2)
  kz<-merge(rmse,rsq,by.x="WorkflowID",by.y="WorkflowID") %>%
    select(WorkflowID,Platzierung.RMSE=Platzierung.x,RMSE,Platzierung.RSQ=Platzierung.y,RSQ,Model=Model.x) %>%
    mutate(Daten=daten) %>%
    arrange(RMSE)
  ergebnis<-list(Kennzahlen=kz,rmse=p.rmse,rsq=p.rsq)
  return(ergebnis)
}  

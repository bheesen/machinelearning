#' Function to show the tuning parameters for a workflow or workflow set
#'
#' @param wfl is the name of a workflow or workflow set
#' @param wfl.name is the name of the workflow or workflow set as title
#' @keywords parameter, tuning, machine learning
#' @return a text with a message
#' @example ml.tunepar(wfl,wfl.name="Workflowname")
#' @export
ml.tunepar<-function (wfl,wfl.name="Workflowname") # Zeigt Tuningparameter von wfl/wflset an
  # workflow: Workflow oder Workflow-Set
  # wfl.name: Name des Workflows (bei Workflow-Set wird ID verwendet)
{
  par.text<-data.frame(matrix(ncol=6,nrow=0))
  colnames(par.text)<-c("Workflow","Paramter-Name","Parameter","Datentyp","Intervall","Werte")
  if (class(wfl)[1] == "workflow") {
    wfl.par<-extract_parameter_set_dials(wfl)
    for(i in 1:nrow(wfl.par)) {
      par.name<-wfl.par %>% extract_parameter_dials(wfl.par[i,]$name)
      par.text[i,]<-c(wfl.name,
                      attributes(par.name$label)$names,
                      sub("#","",par.name$label),
                      par.name$type,
                      paste0(par.name$range$lower,"-",par.name$range$upper),
                      paste0(par.name$values,collapse = ','))
    }
  }
  else if (class(wfl)[1] == "workflow_set") {
    wflset<-wfl
    for(j in 1:nrow(wflset)) {
      wflow.id<-wflset$wflow_id[j]
      wfl.name<-wflow.id
      wfl<-wflset %>% extract_workflow(id=wflow.id)
      wfl.par<-extract_parameter_set_dials(wfl)
      par.anzahl<-nrow(par.text)
      for(i in 1:nrow(wfl.par)) {
        par.name<-wfl.par %>% extract_parameter_dials(wfl.par[i,]$name)
        par.text[i+par.anzahl,]<-c(wfl.name,
                                   attributes(par.name$label)$names,             
                                   sub("#","",par.name$label),
                                   par.name$type,
                                   paste0(par.name$range$lower,"-",par.name$range$upper),
                                   paste0(par.name$values,collapse = ','))
      }
    }
  }
  else{
    par.text<-"Fehler: 1. Parameter weder Workflow noch Workflow-Set."
  }
  return(par.text)
}
#
# Autor : Bernd Heesen
# Skript: Utilizing Own Package MachineLearning
#
### Vorbereitung 

##-Pakete laden-------------------------------------------------------------------
packages <- c("devtools","roxygen2", "tidyverse")
lapply(packages,library, character.only = TRUE, warn.conflicts = FALSE)
rm(packages)             #Liste der Pakete löschen, da sie ja jetzt aktiviert sind

##-Paket installieren und testen--------------------------------------------------
devtools::install_github("bheesen/machinelearning", force = TRUE)
library(machinelearning)
#setwd("~/A-R/machinelearning/data")
# Passworte anzeigen: Passwort für Tutorials von März 2022 - Mai 2022: lh3mt7gw
b<-ds.tutorial.pw() 
b
ds.tutorial(name = "ds.syntax")

##-Funktion ds.tutorial-----------------------------------------------------------
ds.tutorial <- function(name){
  t <- Sys.Date()
  z<-as.numeric(format(t, format="%Y"))-1362
  s<-as.numeric(format(t, format="%m"))
  if (s>2) {
    x<-s-2
    b<-ax[z,x:s] }
  else if (s==2) {
    x<-s-1
    y<-z-1
    b<-c(ax[z,x:s],ax[y,12]) }
  else {
    y<-z-1
    b<-c(ax[z,s],ax[y,11:12])
  }
  z<-as.numeric(format(t, format="%Y"))-1462
  if (s %in% c(9,10,11,12)) {
    s<-9 }
  else if (s %in% c(1,2)) {
    z<-z-1
    s<-9 }
  else {
    s<-3 }
  b<-c(b,ax[z,s])
  passwort <- rstudioapi::askForPassword(
      prompt="Passwort erforderlich! Bitte geben Sie das Berechtigungspasswort ein, welches Sie von Professor Heesen erhalten haben.")
  string<-paste(name,z,s,b[1],b[2],b[3],b[4],passwort)
  print(string)
  if (passwort %in% b)
    learnr::run_tutorial(name, package = "datascience")
  else
    return("Ungültiges Passwort!")
}  

##-Funktion zur Anzeige der Passworte---------------------------------------------
ds.tutorial.pw <- function(){
  t <- Sys.Date()
  z<-as.numeric(format(t, format="%Y"))-1362
  s<-as.numeric(format(t, format="%m"))
  if (s>2) {
    x<-s-2
    b<-ax[z,x:s] }
  else if (s==2) {
    x<-s-1
    y<-z-1
    b<-c(ax[z,x:s],ax[y,12]) }
  else {
    y<-z-1
    b<-c(ax[z,s],ax[y,11:12])
  }
  string<-paste("Gültige Passworte:",b[1],b[2],b[3],"Jahr:",z+1362,z,"Monat:",s)
  string2<-paste("Aktuell Externe:",ax[z,s])
  z<-as.numeric(format(t, format="%Y"))-1462
  if (s %in% c(9,10,11,12)) {
    s<-9 }
  else if (s %in% c(1,2)) {
    z<-z-1
    s<-9 }
  else {
    s<-3 }
  b<-c(b,ax[z,s])
  string3<-paste("Aktuell Studierende:",b[4])
  print(string)
  print(string2)
  print(string3)
  return(b)
}  

##-Erzeugen von 12x1000 Codes -> speichern in ax.RData----------------------------
zeichen<-c(0,1,2,3,4,5,6,7,8,9,"a","b","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z")
c<-new("data.frame")
col.n<-12
row.n<-1000
for (s in 1:col.n){
  for (z in 1:row.n){
    code<-sample(zeichen,size=8,replace=T)
    code<-paste0(code[1],code[2],code[3],code[4],code[5],code[6],code[7],code[8])
    c[z,s]<-code
  }
}
ax<-c
setwd("~/A-R/machinelearning")
#save(ax,file="ax.RData")

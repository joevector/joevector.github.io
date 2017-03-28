
#DESENLACE
#  BMI (RECH5 HA40)
#MUJER
#  Edad (REC0111 V012)
#  Maximo nivel educativo alcanzado (REC0111 - V109)
#  Etnicidad-1 (Solo con V131)
#  Etnicidad-2 (REC0111 - V131 con REC91 y las 119 de REC91)
#  Residente/migrante (residente de zona rural, de zona urbana, migrante rural a urbana)
#HOGAR
#  Wealth index (REC0111 )
#CONTEXTO
#  Region o Region natural (verificar en analisis exploratorio el numero de obs por celda)
#  Anno

#Interacciones: Region x Anno, Migrante x anno

#Primera pregunta: como defino migrantes? 
#Tomar aquellos que nacieron en la ruralidad Y vienen de la ruralidad pero ahora viven en urbano

#Evaluemos dos definiciones:
# 1 - Lugar de residencia: nativo urbano, nativo rural, migrante rural-urbano
# 2 - Etnicidad: habla quechua en casa (etniquisimo), habla spanish pero padres no (reciente desentinco), spanishparlante antiguo

#Sintaxis para extraer ID de mujer de RECH5; Este paso hay que hacerlo cuando ya se tenga el dataset unido
dhs$`2005-RECH5`[1:100,.(HHID,cluster=as.numeric(HHID %>% as.character %>% substr(1,nchar(.)-3)),
                                  hhnumber=as.numeric(HHID %>% as.character %>% substr(nchar(.)-2,nchar(.))))]

dhs$`2005-RECH5`[,.(cluster=as.numeric(HHID %>% as.character %>% substr(1,nchar(.)-3)),
                    hhnumber=as.numeric(HHID %>% as.character %>% substr(nchar(.)-2,nchar(.))),
                    hhline=HA0,bmi=HA40,pregnant=HA54)]

dhs$`2005-REC0111`[,.(cluster=V001,hhnumber=V002,hhline=V003,sampwgt=V005,V007)]


#########################################################################
library(rio)
library(magrittr)
library(lubridate)
library(data.table)

loadDHS <- function(workdir, countfiles=TRUE){
  dhsFiles <- dir(workdir, recursive = TRUE, pattern = "((\\.[dD][tT][aA])|(\\.[dD][bB][fF]))$")
  #dhsFiles <- dhsFiles[grep(pattern = "((rech[0o]\\.)|(rech1\\.)|(rech4\\.)|(rech23\\.)|(rec91[ab]?\\.)|(rec0111\\.))",tolower(dhsFiles))]
  dhsFiles <- dhsFiles[grep(pattern = "(rech5\\.)|(rec91[ab]?\\.|(rec?22313?2\\.)|(rec0111\\.))",tolower(dhsFiles))]
  
  if(countfiles){ #Run this to verify you have all the files you want before actually loading them
    table(sapply(strsplit(dhsFiles,"/"),function(x){x[[1]]})) %>% print
    table(sapply(strsplit(dhsFiles,"/"),function(x){x[[3]]}) %>% tolower %>% substr(1,nchar(.)-4)) %>% print
    
    return(dhsFiles)
  } else {
    actualData <- lapply(paste0(workdir,dhsFiles),import,setclass="data.table")
    names(actualData) <- dhsFiles %>% strsplit("/") %>% sapply(function(x){paste(x[[1]],x[[3]],sep="-") %>% substr(1,nchar(.)-4)})
    return(actualData)
  }
}

#dhs <- loadDHS(sourcedir)
dhs <- loadDHS(sourcedir, countfiles = FALSE)

rbind(dhs$`2005-REC0111`[,.(CASEID,sampwt=V005,age=V012,
                      region=V024,urbrur=V025,educl=V106,educt=V107,ethn=V131,wi=V190)] %>%
  merge(dhs$`2005-RECH5`[,.()], by="CASEID") %>%
  merge(dhs$`2005-REC91`[,.(CASEID,SREGION,S119A,S119C)], by="CASEID") %>%
  merge(dhs$`2005-REC22312`[,.(CASEID,V313)], by="CASEID") %>% cbind(year=2005),

dhs$`2006-REC0111`[,.(CASEID,sampwt=V005,age=V012,
                      region=V024,urbrur=V025,educl=V106,educt=V107,ethn=V131,wi=V190)] %>%
  merge(dhs$`2006-REC91`[,.(CASEID,SREGION,S119A,S119C)], by="CASEID") %>%
  merge(dhs$`2006-RE223132`[,.(CASEID,V313)], by="CASEID") %>% cbind(year=2006),

dhs$`2007-REC0111`[,.(CASEID,sampwt=V005,age=V012,
                      region=V024,urbrur=V025,educl=V106,educt=V107,ethn=V131,wi=V190)] %>%
  merge(dhs$`2007-REC91A`[,.(CASEID,SREGION,S119A,S119C)], by="CASEID") %>%
  merge(dhs$`2007-REC22312`[,.(CASEID,V313)], by="CASEID") %>% cbind(year=2007),

dhs$`2008-REC0111`[,.(CASEID,sampwt=V005,age=V012,
                      region=V024,urbrur=V025,educl=V106,educt=V107,ethn=V131,wi=V190)] %>%
  merge(dhs$`2008-REC91`[,.(CASEID,SREGION,S119A,S119C)], by="CASEID") %>%
  merge(dhs$`2008-RE223132`[,.(CASEID,V313)], by="CASEID") %>% cbind(year=2008),

#No 2009, 2010, 2011, 2012
#En 2013 esta pregunta pero no variable!!!

dhs$`2014-rec0111`[,.(CASEID,sampwt=V005,age=V012,
                      region=V024,urbrur=V025,educl=V106,educt=V107,ethn=V131,wi=V190)] %>%
  merge(dhs$`2014-REC91`[,.(CASEID,SREGION,S119A=Q119NA,S119C=Q119NB)], by="CASEID") %>%
  merge(dhs$`2014-re223132`[,.(CASEID,V313)], by="CASEID") %>% cbind(year=2014),

dhs$`2015-rec0111`[,.(CASEID=caseid,sampwt=v005,age=v012,
                      region=v024,urbrur=v025,educl=v106,educt=v107,ethn=v131,wi=v190)] %>%
  merge(dhs$`2015-rec91`[,.(CASEID=caseid,SREGION=sregion,S119A=q119na,S119C=q119nb)], by="CASEID") %>%
  merge(dhs$`2015-re223132`[,.(CASEID=caseid,V313=v313)], by="CASEID") %>% cbind(year=2015)) -> dhs

dhs[is.element(V313,0:2),method:="not modern"]
dhs[V313==3,method:="modern"]

dhs[ethn==1,ethn_cat:="spanish speaker"]
dhs[is.element(S119A,2:4)|is.element(S119C,2:4),ethn_cat:="ethnic parents"]
#dhs[is.element(ethn,2:4),ethn_cat:="ethnic self"]

dhs[,.(year,age,educ=educl,wealth=wi,region=SREGION,ethn_cat,contracep=method)] %>%
write.csv("final.csv",row.names=FALSE)

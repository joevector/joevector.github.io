
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
#res_ vars: have to do with residence
#qc_ vars: have to do with quality control (e.g. exclusions)
#ethnic_ vars: have to do with the subject's ethnicity
# > In particular, self is self-reported, dad/mom is that of parents and gpdad/gpmom is that of grandparents


rbind(dhs$`2005-RECH5`[,.(cluster=as.numeric(HHID %>% as.character %>% substr(1,nchar(.)-3)),
                    hhnumber=as.numeric(HHID %>% as.character %>% substr(nchar(.)-2,nchar(.))),
                    hhline=as.numeric(HA0),bmi=HA40,qc_pregnant=HA54)] %>%
  merge(dhs$`2005-REC0111`[,.(cluster=V001,hhnumber=V002,hhline=V003,sampwgt=V005,year=V007,
                              age=V012,age5cat=V013,education=V149,region=V024,res_ur=V025,
                              res_type=V026,res_child=V103,res_time=V104,res_prev=V105,ethnic_self=V131,
                              qc_visitor=V135,wealth=V190)], by=c("cluster","hhnumber","hhline"),all=TRUE) %>%
  merge(dhs$`2005-REC91`[,.(cluster=as.numeric(CASEID %>% as.character %>% substr(1,nchar(.)-6)),
                            hhnumber=as.numeric(CASEID %>% as.character %>% substr(nchar(.)-5,nchar(.)-3)),
                            hhline=as.numeric(CASEID %>% as.character %>% substr(nchar(.)-2,nchar(.))),
                            natregion=SREGION,ethnic_dad=S119A,ethnic_gpdad=S119B,ethnic_mom=S119C,
                            ethnic_gpmom=S119D)], by=c("cluster","hhnumber","hhline"),all=TRUE),

dhs$`2006-RECH5`[,.(cluster=as.numeric(HHID %>% as.character %>% substr(1,nchar(.)-3)),
                    hhnumber=as.numeric(HHID %>% as.character %>% substr(nchar(.)-2,nchar(.))),
                    hhline=as.numeric(HA0),bmi=HA40,qc_pregnant=HA54)] %>%
  merge(dhs$`2006-REC0111`[,.(cluster=V001,hhnumber=V002,hhline=V003,sampwgt=V005,year=V007,
                              age=V012,age5cat=V013,education=V149,region=V024,res_ur=V025,
                              res_type=V026,res_child=V103,res_time=V104,res_prev=V105,ethnic_self=V131,
                              qc_visitor=V135,wealth=V190)], by=c("cluster","hhnumber","hhline"),all=TRUE) %>%
  merge(dhs$`2006-REC91`[,.(cluster=as.numeric(CASEID %>% as.character %>% substr(1,nchar(.)-6)),
                            hhnumber=as.numeric(CASEID %>% as.character %>% substr(nchar(.)-5,nchar(.)-3)),
                            hhline=as.numeric(CASEID %>% as.character %>% substr(nchar(.)-2,nchar(.))),
                            natregion=SREGION,ethnic_dad=S119A,ethnic_gpdad=S119B,ethnic_mom=S119C,
                            ethnic_gpmom=S119D)], by=c("cluster","hhnumber","hhline"),all=TRUE),

dhs$`2007-RECH5`[,.(cluster=as.numeric(HHID %>% as.character %>% substr(1,nchar(.)-3)),
                    hhnumber=as.numeric(HHID %>% as.character %>% substr(nchar(.)-2,nchar(.))),
                    hhline=as.numeric(HA0),bmi=HA40,qc_pregnant=HA54)] %>%
  merge(dhs$`2007-REC0111`[,.(cluster=V001,hhnumber=V002,hhline=V003,sampwgt=V005,year=V007,
                              age=V012,age5cat=V013,education=V149,region=V024,res_ur=V025,
                              res_type=V026,res_child=V103,res_time=V104,res_prev=V105,ethnic_self=V131,
                              qc_visitor=V135,wealth=V190)], by=c("cluster","hhnumber","hhline"),all=TRUE) %>%
  merge(dhs$`2007-REC91A`[,.(cluster=as.numeric(CASEID %>% as.character %>% substr(1,nchar(.)-6)),
                            hhnumber=as.numeric(CASEID %>% as.character %>% substr(nchar(.)-5,nchar(.)-3)),
                            hhline=as.numeric(CASEID %>% as.character %>% substr(nchar(.)-2,nchar(.))),
                            natregion=SREGION,ethnic_dad=S119A,ethnic_gpdad=S119B,ethnic_mom=S119C,
                            ethnic_gpmom=S119D)], by=c("cluster","hhnumber","hhline"),all=TRUE),

dhs$`2008-RECH5`[,.(cluster=as.numeric(HHID %>% as.character %>% substr(1,nchar(.)-3)),
                    hhnumber=as.numeric(HHID %>% as.character %>% substr(nchar(.)-2,nchar(.))),
                    hhline=as.numeric(HA0),bmi=HA40,qc_pregnant=HA54)] %>%
  merge(dhs$`2008-REC0111`[,.(cluster=V001,hhnumber=V002,hhline=V003,sampwgt=V005,year=V007,
                              age=V012,age5cat=V013,education=V149,region=V024,res_ur=V025,
                              res_type=V026,res_child=V103,res_time=V104,res_prev=V105,ethnic_self=V131,
                              qc_visitor=V135,wealth=V190)], by=c("cluster","hhnumber","hhline"),all=TRUE) %>%
  merge(dhs$`2008-REC91`[,.(cluster=as.numeric(CASEID %>% as.character %>% substr(1,nchar(.)-6)),
                            hhnumber=as.numeric(CASEID %>% as.character %>% substr(nchar(.)-5,nchar(.)-3)),
                            hhline=as.numeric(CASEID %>% as.character %>% substr(nchar(.)-2,nchar(.))),
                            natregion=SREGION,ethnic_dad=S119A,ethnic_gpdad=S119B,ethnic_mom=S119C,
                            ethnic_gpmom=S119D)], by=c("cluster","hhnumber","hhline"),all=TRUE),

dhs$`2009-rech5`[,.(cluster=as.numeric(HHID %>% as.character %>% substr(1,nchar(.)-3)),
                    hhnumber=as.numeric(HHID %>% as.character %>% substr(nchar(.)-2,nchar(.))),
                    hhline=as.numeric(HA0),bmi=HA40,qc_pregnant=HA54)] %>%
  merge(dhs$`2009-REC0111`[,.(cluster=V001,hhnumber=V002,hhline=V003,sampwgt=V005,year=V007,
                              age=V012,age5cat=V013,education=V149,region=V024,res_ur=V025,
                              res_type=V026,res_child=V103,res_time=V104,res_prev=V105,ethnic_self=V131,
                              qc_visitor=V135,wealth=V190)], by=c("cluster","hhnumber","hhline"),all=TRUE) %>%
  merge(dhs$`2009-REC91`[,.(cluster=as.numeric(CASEID %>% as.character %>% substr(1,nchar(.)-6)),
                            hhnumber=as.numeric(CASEID %>% as.character %>% substr(nchar(.)-5,nchar(.)-3)),
                            hhline=as.numeric(CASEID %>% as.character %>% substr(nchar(.)-2,nchar(.))),
                            natregion=SREGION,ethnic_dad=NA,ethnic_gpdad=NA,ethnic_mom=NA,
                            ethnic_gpmom=NA)], by=c("cluster","hhnumber","hhline"),all=TRUE),

dhs$`2010-RECH5`[,.(cluster=as.numeric(HHID %>% as.character %>% substr(1,nchar(.)-3)),
                    hhnumber=as.numeric(HHID %>% as.character %>% substr(nchar(.)-2,nchar(.))),
                    hhline=as.numeric(HA0),bmi=HA40,qc_pregnant=HA54)] %>%
  merge(dhs$`2010-REC0111`[,.(cluster=V001,hhnumber=V002,hhline=V003,sampwgt=V005,year=V007,
                              age=V012,age5cat=V013,education=V149,region=V024,res_ur=V025,
                              res_type=V026,res_child=V103,res_time=V104,res_prev=V105,ethnic_self=V131,
                              qc_visitor=V135,wealth=V190)], by=c("cluster","hhnumber","hhline"),all=TRUE) %>%
  merge(dhs$`2010-REC91`[,.(cluster=as.numeric(CASEID %>% as.character %>% substr(1,nchar(.)-6)),
                            hhnumber=as.numeric(CASEID %>% as.character %>% substr(nchar(.)-5,nchar(.)-3)),
                            hhline=as.numeric(CASEID %>% as.character %>% substr(nchar(.)-2,nchar(.))),
                            natregion=SREGION,ethnic_dad=NA,ethnic_gpdad=NA,ethnic_mom=NA,
                            ethnic_gpmom=NA)], by=c("cluster","hhnumber","hhline"),all=TRUE),

dhs$`2011-rech5`[,.(cluster=as.numeric(HHID %>% as.character %>% substr(1,nchar(.)-3)),
                    hhnumber=as.numeric(HHID %>% as.character %>% substr(nchar(.)-2,nchar(.))),
                    hhline=as.numeric(HA0),bmi=HA40,qc_pregnant=HA54)] %>%
  merge(dhs$`2011-rec0111`[,.(cluster=V001,hhnumber=V002,hhline=V003,sampwgt=V005,year=V007,
                              age=V012,age5cat=V013,education=V149,region=V024,res_ur=V025,
                              res_type=V026,res_child=V103,res_time=V104,res_prev=V105,ethnic_self=V131,
                              qc_visitor=V135,wealth=V190)], by=c("cluster","hhnumber","hhline"),all=TRUE) %>%
  merge(dhs$`2011-REC91`[,.(cluster=as.numeric(CASEID %>% as.character %>% substr(1,nchar(.)-6)),
                            hhnumber=as.numeric(CASEID %>% as.character %>% substr(nchar(.)-5,nchar(.)-3)),
                            hhline=as.numeric(CASEID %>% as.character %>% substr(nchar(.)-2,nchar(.))),
                            natregion=SREGION,ethnic_dad=NA,ethnic_gpdad=NA,ethnic_mom=NA,
                            ethnic_gpmom=NA)], by=c("cluster","hhnumber","hhline"),all=TRUE),

dhs$`2012-rech5`[,.(cluster=as.numeric(HHID %>% as.character %>% substr(1,nchar(.)-5)),
                      hhnumber=as.numeric(HHID %>% as.character %>% substr(nchar(.)-4,nchar(.))),
                      hhline=as.numeric(HA0),bmi=HA40,qc_pregnant=HA54)] %>%
    merge(dhs$`2012-rec0111`[,.(cluster=as.numeric(CASEID %>% as.character %>% substr(1,nchar(.)-8)),
                                hhnumber=as.numeric(CASEID %>% as.character %>% substr(nchar(.)-7,nchar(.)-3)),
                                hhline=as.numeric(CASEID %>% as.character %>% substr(nchar(.)-2,nchar(.))),
                                sampwgt=V005,year=V007,age=V012,age5cat=V013,education=V149,region=V024,res_ur=V025,
                                res_type=V026,res_child=V103,res_time=V104,res_prev=V105,ethnic_self=V131,
                                qc_visitor=V135,wealth=V190)], by=c("cluster","hhnumber","hhline"),all=TRUE) %>%
    merge(dhs$`2012-REC91`[,.(cluster=as.numeric(CASEID %>% as.character %>% substr(1,nchar(.)-8)),
                              hhnumber=as.numeric(CASEID %>% as.character %>% substr(nchar(.)-7,nchar(.)-3)),
                              hhline=as.numeric(CASEID %>% as.character %>% substr(nchar(.)-1,nchar(.))),
                              natregion=SREGION,ethnic_dad=NA,ethnic_gpdad=NA,ethnic_mom=NA,
                              ethnic_gpmom=NA)], by=c("cluster","hhnumber","hhline"),all=TRUE),

dhs$`2013-rech5`[,.(cluster=as.numeric(HHID %>% as.character %>% substr(1,nchar(.)-5)),
                    hhnumber=as.numeric(HHID %>% as.character %>% substr(nchar(.)-4,nchar(.))),
                    hhline=as.numeric(HA0),bmi=HA40,qc_pregnant=HA54)] %>%
  merge(dhs$`2013-rec0111`[,.(cluster=as.numeric(CASEID %>% as.character %>% substr(1,nchar(.)-8)),
                              hhnumber=as.numeric(CASEID %>% as.character %>% substr(nchar(.)-7,nchar(.)-3)),
                              hhline=as.numeric(CASEID %>% as.character %>% substr(nchar(.)-2,nchar(.))),
                              sampwgt=V005,year=V007,age=V012,age5cat=V013,education=V149,region=V024,res_ur=V025,
                              res_type=V026,res_child=V103,res_time=V104,res_prev=V105,ethnic_self=V131,
                              qc_visitor=V135,wealth=V190)], by=c("cluster","hhnumber","hhline"),all=TRUE) %>%
  merge(dhs$`2013-REC91`[,.(cluster=as.numeric(CASEID %>% as.character %>% substr(1,nchar(.)-8)),
                            hhnumber=as.numeric(CASEID %>% as.character %>% substr(nchar(.)-7,nchar(.)-3)),
                            hhline=as.numeric(CASEID %>% as.character %>% substr(nchar(.)-2,nchar(.))),
                            natregion=SREGION,ethnic_dad=Q119NB,ethnic_gpdad=NA,ethnic_mom=Q119NA,
                            ethnic_gpmom=NA)], by=c("cluster","hhnumber","hhline"),all=TRUE),

dhs$`2014-rech5`[,.(cluster=as.numeric(HHID %>% as.character %>% substr(1,nchar(.)-5)),
                    hhnumber=as.numeric(HHID %>% as.character %>% substr(nchar(.)-4,nchar(.))),
                    hhline=as.numeric(HA0),bmi=HA40,qc_pregnant=HA54)] %>%
  merge(dhs$`2014-rec0111`[,.(cluster=as.numeric(CASEID %>% as.character %>% substr(1,nchar(.)-8)),
                              hhnumber=as.numeric(CASEID %>% as.character %>% substr(nchar(.)-7,nchar(.)-3)),
                              hhline=as.numeric(CASEID %>% as.character %>% substr(nchar(.)-2,nchar(.))),
                              sampwgt=V005,year=V007,age=V012,age5cat=V013,education=V149,region=V024,res_ur=V025,
                              res_type=V026,res_child=V103,res_time=V104,res_prev=V105,ethnic_self=V131,
                              qc_visitor=V135,wealth=V190)], by=c("cluster","hhnumber","hhline"),all=TRUE) %>%
  merge(dhs$`2014-REC91`[,.(cluster=as.numeric(CASEID %>% as.character %>% substr(1,nchar(.)-8)),
                            hhnumber=as.numeric(CASEID %>% as.character %>% substr(nchar(.)-7,nchar(.)-3)),
                            hhline=as.numeric(CASEID %>% as.character %>% substr(nchar(.)-2,nchar(.))),
                            natregion=SREGION,ethnic_dad=Q119NB,ethnic_gpdad=NA,ethnic_mom=Q119NA,
                            ethnic_gpmom=NA)], by=c("cluster","hhnumber","hhline"),all=TRUE),

dhs$`2015-rech5`[,.(cluster=as.numeric(hhid %>% as.character %>% substr(1,nchar(.)-5)),
                    hhnumber=as.numeric(hhid %>% as.character %>% substr(nchar(.)-4,nchar(.))),
                    hhline=as.numeric(ha0),bmi=ha40,qc_pregnant=ha54)] %>%
  merge(dhs$`2015-rec0111`[,.(cluster=as.numeric(caseid %>% as.character %>% substr(1,nchar(.)-8)),
                              hhnumber=as.numeric(caseid %>% as.character %>% substr(nchar(.)-7,nchar(.)-3)),
                              hhline=as.numeric(caseid %>% as.character %>% substr(nchar(.)-2,nchar(.))),
                              sampwgt=v005,year=v007,age=v012,age5cat=v013,education=v149,region=v024,res_ur=v025,
                              res_type=v026,res_child=v103,res_time=v104,res_prev=v105,ethnic_self=v131,
                              qc_visitor=v135,wealth=v190)], by=c("cluster","hhnumber","hhline"),all=TRUE) %>%
  merge(dhs$`2015-rec91`[,.(cluster=as.numeric(caseid %>% as.character %>% substr(1,nchar(.)-8)),
                            hhnumber=as.numeric(caseid %>% as.character %>% substr(nchar(.)-7,nchar(.)-3)),
                            hhline=as.numeric(caseid %>% as.character %>% substr(nchar(.)-2,nchar(.))),
                            natregion=sregion,ethnic_dad=q119nb,ethnic_gpdad=NA,ethnic_mom=q119nb,
                            ethnic_gpmom=NA)], by=c("cluster","hhnumber","hhline"),all=TRUE))

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

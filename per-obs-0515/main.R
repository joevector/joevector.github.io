sourcedir <- "D:/Trabajo/Bases INEI/ENDES/"

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

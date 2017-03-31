loadDHS <- function(workdir, countfiles=TRUE){
  
  library(data.table)
  library(magrittr)
  library(rio)
  
  #Regular expressions to fetch the required DHS filepaths
  #(note tolower() is only used for regexp matching; this preserves path case for case-sensitive systems)
  dhsFiles <- dir(workdir, recursive = TRUE, pattern = "((\\.[dD][tT][aA])|(\\.[dD][bB][fF]))$")
  dhsFiles <- dhsFiles[grep(pattern = "(rech5\\.)|(rec91[ab]?\\.|(rec?22313?2\\.)|(rec0111\\.))",tolower(dhsFiles))]
  
  #By default, the function only tells you how many files it found so you can spot issues before loading everything
  if(countfiles){ 
    
    table(sapply(strsplit(dhsFiles,"/"),function(x){x[[1]]})) %>% print
    table(sapply(strsplit(dhsFiles,"/"),function(x){x[[3]]}) %>% tolower %>% substr(1,nchar(.)-4)) %>% print
    
    return(dhsFiles)
    
  } else {
    
    dhs <- lapply(paste0(workdir,dhsFiles),import,setclass="data.table")
    names(dhs) <- dhsFiles %>% strsplit("/") %>% sapply(function(x){paste(x[[1]],x[[3]],sep="-") %>% substr(1,nchar(.)-4)})
    
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
                                      ethnic_gpmom=NA)], by=c("cluster","hhnumber","hhline"),all=TRUE)) -> dhs
    dhs[!duplicated(dhs)] %>% return
  }
}

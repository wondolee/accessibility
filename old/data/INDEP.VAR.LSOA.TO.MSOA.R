##Basic parameter for workspace
Sys.setlocale(locale="English_United Kingdom")
setwd("d:/WORKSPACE/GIT/accessibility/data/")
path<-"d:/WORKSPACE/GIT/accessibility/data/"
lsoa.to.msoa<-read.csv("en.lsoa.to.msoa.lookup.csv")[2:5]

###6791 MSOAs in England
require(tidyverse)
require(plyr)
require(reshape2)

##income level
inc.paye.lsoa<-read.csv("D:/WORKSPACE/COVID19/MODEL/REVISE_20200531/PAYE_LSOA_GRADE_HH.csv")
colnames(inc.paye.lsoa)[1]<-"LSOA11CD";inc.paye.lsoa<-inc.paye.lsoa[c(1,2,13,24:26)]
tbl.inc.paye.msoa<-left_join(lsoa.to.msoa,inc.paye.lsoa,by=c("LSOA11CD","LSOA11NM"))
tbl.inc.paye.msoa<-as.data.frame(tbl.inc.paye.msoa %>% group_by(MSOA11CD, MSOA11NM) %>% summarise_at(vars(HHs,INC_20,INC_80,INC_MED), sum))
tbl.inc.paye.msoa<-transform(tbl.inc.paye.msoa,INC_20=INC_20/HHs,INC_80=INC_80/HHs,INC_MED=INC_MED/HHs);tbl.inc.paye.msoa<-tbl.inc.paye.msoa[c(-3)]
write.csv(tbl.inc.paye.msoa,paste0(path,"/temp/income.msoa.csv"))

##education and skills
tbl.edu.no<-read.csv("d:/WORKSPACE/COVID19/MODEL/M5_20200525/edu.no.qual.csv")
tbl.en.edu.no<-subset(tbl.edu.no,grepl("E",LSOA11CD))
en.tbl.edu.no.ccg.info<-left_join(tbl.en.edu.no,lsoa.to.msoa,by=c("LSOA11CD","LSOA11NM"))
tbl.en.edu.no<-as.data.frame(en.tbl.edu.no.ccg.info %>% group_by(MSOA11CD, MSOA11NM) %>% summarise_at(vars(NO.QUAL, RES.POP), sum))
tbl.en.edu.no$NO.EDU.PER<-tbl.en.edu.no$NO.QUAL/tbl.en.edu.no$RES.POP
en.msoa.pop<-tbl.en.edu.no[c(1,2,4)]
tbl.no.edu<-tbl.en.edu.no[c(1,2,5)]
write.csv(tbl.en.edu.no,paste0(path,"/temp/no.qual.msoa.csv"))

tbl.non.eng.speak<-read.csv("d:/WORKSPACE/COVID19/MODEL/M5_20200525/NON.SPEAK.ENG.csv")
tbl.en.non.eng.speak<-subset(tbl.non.eng.speak,grepl("E",LSOA11CD))
en.tbl.edu.non.eng.speak.msoa.info<-left_join(tbl.en.non.eng.speak,lsoa.to.msoa,by=c("LSOA11CD","LSOA11NM"))
tbl.en.edu.non.eng.speak<-as.data.frame(en.tbl.edu.non.eng.speak.msoa.info %>% group_by(MSOA11CD, MSOA11NM) %>% summarise_at(vars(NON.SPEAK.ENG, RES.POP), sum))
tbl.en.edu.non.eng.speak$NO.ENG.SPEAK.PER<-tbl.en.edu.non.eng.speak$NON.SPEAK.ENG/tbl.en.edu.non.eng.speak$RES.POP
tbl.non.eng.speak<-tbl.en.edu.non.eng.speak[c(1,2,5)]
write.csv(tbl.non.eng.speak,paste0(path,"/temp/non.Eng.speak.msoa.csv"))

##social grade
soc.g<-read.csv("d:/WORKSPACE/COVID19/MODEL/REVISE_20200531/NS-SeC.csv")
tbl.soc.g<-left_join(lsoa.to.msoa,soc.g,by=c("LSOA11CD","LSOA11NM"))
soc.g.msoa<-as.data.frame(tbl.soc.g %>% group_by(MSOA11CD, MSOA11NM) %>% summarise_at(vars(Eco_All:SG_DE), sum))
soc.g.msoa<-transform(soc.g.msoa,SG_AB=SG_AB/Eco_All,SG_C1=SG_C1/Eco_All,SG_C2=SG_C2/Eco_All,SG_DE=SG_DE/Eco_All)
tbl.soc.g<-soc.g.msoa[c(-3)]
write.csv(tbl.soc.g,paste0(path,"/temp/social.grade.msoa.csv"))

##housing type
housing.ten<-read.csv("D:/WORKSPACE/COVID19/MODEL/M5_20200525/DATA/PROCESS/t.housing.tenure.lsoa.csv")
housing.ten.to.msoa<-left_join(housing.ten,lsoa.to.msoa,by=c("LSOA11CD","LSOA11NM"))
tbl.ten.msoa<-as.data.frame(housing.ten.to.msoa %>% group_by(MSOA11CD, MSOA11NM) %>% summarise_at(vars(Total.HHs,Owned,Shared.ownership,Social.rented,Private.rented,Living.rent.free), sum))
tbl.ten.msoa<-transform(tbl.ten.msoa,T.01=Owned/Total.HHs,T.02=Shared.ownership/Total.HHs,
                               T.03=Social.rented/Total.HHs,T.04=Private.rented/Total.HHs,T.05=Living.rent.free/Total.HHs)
tbl.ten.msoa<-tbl.ten.msoa[c(1,2,9:13)]
tbl.ten<-na.omit(tbl.ten.msoa)
tbl.ten<-tbl.ten[c(1,2,5)]
write.csv(tbl.ten.msoa,paste0(path,"housing.ten.msoa.csv"))

beds<-read.csv("D:/WORKSPACE/COVID19/MODEL/M5_20200525/DATA/PROCESS/t.num.beds.lsoa.csv")
beds.to.msoa<-left_join(beds,lsoa.to.msoa,by=c("LSOA11CD","LSOA11NM"))
tbl.beds.msoa<-as.data.frame(beds.to.msoa %>% group_by(MSOA11CD, MSOA11NM) %>% summarise_at(vars(Total.Beds,B.Nan,B.1,B.2,B.3,B.4,B.5.), sum))
tbl.beds.msoa$T.B3<-tbl.beds.msoa$B.3+tbl.beds.msoa$B.4+tbl.beds.msoa$B.5.
tbl.beds.msoa<-transform(tbl.beds.msoa,B.00=B.Nan/Total.Beds,B.01=B.1/Total.Beds,B.02=B.2/Total.Beds,
                        B.03=B.3/Total.Beds,B.04=B.4/Total.Beds,B.05=B.5./Total.Beds,T.B03=T.B3/Total.Beds)
write.csv(tbl.beds.msoa,paste0(path,"/temp/num.of.beds.msoa.csv"))
tbl.beds<-tbl.beds.msoa[c(1,2,17)]

##res. pop density (own + neighboring)
msoa.area<-read.csv("en.msoa.area.csv");colnames(msoa.area)[1]<-"MSOA11CD"
msoa.area$Area<-msoa.area$ST_AREA/1000000
tbl.pop.den<-left_join(en.msoa.pop,msoa.area,by=c("MSOA11CD","MSOA11NM"))
tbl.pop.den$res.den<-(tbl.pop.den$RES.POP/100000)/tbl.pop.den$Area
tbl.pop.den<-tbl.pop.den[c(-3,-4)]
write.csv(tbl.pop.den,paste0(path,"/temp/res.pop.den.msoa.csv"))

##number of vehicles
vehicles<-read.csv("D:/WORKSPACE/COVID19/MODEL/M5_20200525/DATA/PROCESS/t.num.vehicles.lsoa.csv")
vehicles.to.msoa<-left_join(vehicles,lsoa.to.msoa,by=c("LSOA11CD","LSOA11NM"))
tbl.vehicles.msoa<-as.data.frame(vehicles.to.msoa %>% group_by(MSOA11CD,MSOA11NM) %>% summarise_at(vars(Total.HHs,V_Nan,V01,V02,V03,V04.), sum))
tbl.vehicles.msoa<-transform(tbl.vehicles.msoa,V.00=V_Nan/Total.HHs,V.01=V01/Total.HHs,V.02=V02/Total.HHs,
                            V.03=V03/Total.HHs,V.04=V04./Total.HHs)
tbl.veh<-tbl.vehicles.msoa[c(1,2,9:13)]
write.csv(tbl.veh,paste0(path,"/temp/num.veh.msoa.csv"))

##GPs (excluded due to the data limitation)
gp.lsoa<-read.csv("D:/WORKSPACE/GIT/accessibility/data/temp/gp-reg-pat-prac-lsoa-male-female-July-21.zip/gp-reg-pat-prac-lsoa-all.csv")
en.gp.lsoa<-subset(gp.lsoa,grepl("E0",LSOA_CODE))
gp.lsoa.freq<-ddply(en.gp.lsoa,.(LSOA_CODE),summarise,total.reg=sum(NUMBER_OF_PATIENTS),total.num=length(PRACTICE_NAME))
colnames(gp.lsoa.freq)<-c("LSOA11CD","total.patients","total.gp")
gp.msoa<-left_join(gp.lsoa.freq,lsoa.to.msoa,by="LSOA11CD")
tbl.gp.msoa<-as.data.frame(gp.msoa %>% group_by(MSOA11CD,MSOA11NM) %>% summarise_at(vars(total.patients,total.gp), sum))
tbl.gp.msoa$gp.per<-tbl.gp.msoa$total.gp/(tbl.gp.msoa$total.patients/1000)
tbl.gp.msoa<-tbl.gp.msoa[c(1,2,5)]
write.csv(tbl.gp.msoa,paste0(path,"/temp/gp.msoa.csv"))

##Hospitals 
#premises<-read.csv("d:/OneDrive - Nexus365/WORK/Document/RESEARCH/COVID19/04_UPDATE_FINAL_DATA_20200327/POI/remain.poi.with.lsoa.info.gcs.csv")
#hospital<-subset(premises,grepl(5280,code))
#hospital<-subset(hospital,code==5280369 | code==5280371 | code==5280780 | code==5280812)
#hospital.lsoa<-plyr::count(hospital,c("lsoa11cd","lsoa11nm"))
#colnames(hospital.lsoa)<-c("LSOA11CD","LSOA11NM","T.HOSPITAL")

hospital.lsoa<-read.csv("D:/WORKSPACE/COVID19/MODEL/M5_20200525/DATA/PROCESS/t.premise.hospital.lsoa.csv")
hospital.msoa<-left_join(lsoa.to.msoa,hospital.lsoa,by=c("LSOA11CD","LSOA11NM"))
hospital.msoa<-left_join(hospital.msoa,en.msoa.pop,by=c("MSOA11CD","MSOA11NM"))
hospital.msoa[is.na(hospital.msoa)]<-0
tbl.hospital.msoa<-as.data.frame(hospital.msoa %>% group_by(MSOA11CD,MSOA11NM) %>% summarise_at(vars(T.HOSPITAL,RES.POP), sum))
tbl.hospital.msoa$hospital.per<-tbl.hospital.msoa$T.HOSPITAL/(tbl.hospital.msoa$RES.POP/100000)
tbl.hospital.msoa<-tbl.hospital.msoa[c(1,2,5)]
write.csv(tbl.hospital.msoa,paste0(path,"/temp/hospital.msoa.csv"))

##Parks (own + neighbouring)
park.msoa<-read.csv("D:/WORKSPACE/COVID19/MODEL/M5_20200525/DATA/PROCESS/en.park.msoa.freq.csv");colnames(park.msoa)[1]<-"MSOA11CD"
tbl.park<-left_join(en.msoa.pop,park.msoa,by=c("MSOA11CD"))
tbl.park$park.per<-tbl.park$total.parks/(tbl.park$RES.POP/100000)
tbl.park<-tbl.park[c(1,2,5)]
tbl.park$park.per[is.na(tbl.park$park.per)]<-0
write.csv(tbl.park,paste0(path,"/temp/park.msoa.csv"))

##Supermarkets (own + neighboruing)
#premises<-read.csv("d:/OneDrive - Nexus365/WORK/Document/RESEARCH/COVID19/04_UPDATE_FINAL_DATA_20200327/POI/remain.poi.with.lsoa.info.gcs.csv")
#supermarket<-subset(premises,grepl(94706,code))
#supermarket.lsoa<-plyr::count(supermarket,c("lsoa11cd","lsoa11nm"))
#colnames(supermarket.lsoa)<-c("LSOA11CD","LSOA11NM","T.MARKET")

supermarket.lsoa<-read.csv("D:/WORKSPACE/COVID19/MODEL/M5_20200525/DATA/PROCESS/t.premise.supermarket.lsoa.csv")
supermarket.msoa<-left_join(lsoa.to.msoa,supermarket.lsoa,by=c("LSOA11CD","LSOA11NM"))
supermarket.msoa<-left_join(supermarket.msoa,en.msoa.pop,by=c("MSOA11CD","MSOA11NM"))
tbl.market.msoa<-as.data.frame(supermarket.msoa %>% group_by(MSOA11CD,MSOA11NM) %>% summarise_at(vars(T.MARKET,RES.POP), sum))
tbl.market.msoa[is.na(tbl.market.msoa)]<-0
tbl.market.msoa$market.per<-tbl.market.msoa$T.MARKET/(tbl.market.msoa$RES.POP/100000)
tbl.market.msoa<-tbl.market.msoa[c(1,2,5)]
write.csv(tbl.market.msoa,paste0(path,"/temp/supermarket.msoa.csv"))

##Population health - good and bad
health.stat<-read.csv("D:/WORKSPACE/COVID19/MODEL/M5_20200525/DATA/PROCESS/t.health.status.lsoa.csv")
health.stat.to.msoa<-left_join(health.stat,lsoa.to.msoa,by=c("LSOA11CD","LSOA11NM"))
tbl.pop.health<-as.data.frame(health.stat.to.msoa %>% group_by(MSOA11CD, MSOA11NM) %>% summarise_at(vars(Total,Very.good.health,Good.health,Fair.health,Bad.health,Very.bad.health), sum))
tbl.pop.health<-transform(tbl.pop.health,H.G=(Very.good.health+Good.health)/Total,H.F=Fair.health/Total,
                          H.B=(Bad.health+Very.bad.health)/Total)
tbl.pop.health<-tbl.pop.health[c(1,2,9:11)]
tbl.pop.health<-na.omit(tbl.pop.health)
write.csv(tbl.pop.health,paste0(path,"/temp/pop.health.msoa.csv"))

##Employment - part-time, full-time, and self-employed
emp<-read.csv("d:/WORKSPACE/COVID19/MODEL/REVISE_20200531/Economic activity.csv")
tbl.emp<-left_join(lsoa.to.msoa,emp,by=c("LSOA11CD","LSOA11NM"))
tbl.emp.msoa<-as.data.frame(tbl.emp %>% group_by(MSOA11CD, MSOA11NM) %>% summarise_at(vars(Tot.act:not_.work_other), sum))
tbl.emp.msoa<-transform(tbl.emp.msoa,part_time=part_time/Tot.act,full_time=full_time/Tot.act,self_employed=self_employed/Tot.act,
                       unemployed=unemployed/Tot.act,students=students/Tot.act,retired=retired/Tot.act,housewife=housewife/Tot.act,
                       long_disabled=long_disabled/Tot.act,NaN_other=not_.work_other/Tot.act)
tbl.emp.msoa<-tbl.emp.msoa[c(-3,-12)]
tbl.emp.msoa<-tbl.emp.msoa[c(1:5)]
write.csv(tbl.emp.msoa,paste0(path,"/temp/emp.status.msoa.csv"))

##Ethnicity - Pakistani, Bangladeshi, African, Carribean
ethnic<-read.csv("D:/WORKSPACE/COVID19/NEW_20200821/CASE/KS201EW_ETHNIC_GROUP_PRE.csv")
ethnic.to.msoa<-left_join(ethnic,lsoa.to.msoa,by=c("LSOA11CD","LSOA11NM"))
tbl.ethnic.msoa<-as.data.frame(ethnic.to.msoa %>% group_by(MSOA11CD, MSOA11NM) %>% 
                                summarise_at(vars(All.usual.residents, White, Mixed, Indian, Pakistani, Bangladeshi, Chinese, Other.Asian, African, Caribbean, Other.Black, Other.ethnic.group), sum))
tbl.ethnic.msoa<-transform(tbl.ethnic.msoa,E.WH=White/All.usual.residents, E.MIX=Mixed/All.usual.residents, E.IN=Indian/All.usual.residents,
                          E.PA=Pakistani/All.usual.residents,E.BA=Bangladeshi/All.usual.residents,
                          E.CH=Chinese/All.usual.residents, E.O.A=Other.Asian/All.usual.residents, 
                          E.AF=African/All.usual.residents, E.CAR=Caribbean/All.usual.residents, 
                          E.O.B=Other.Black/All.usual.residents, E.O= Other.ethnic.group/All.usual.residents)
tbl.ethnic<-tbl.ethnic.msoa[c(1,2,15:25)]
tbl.ethnic<-na.omit(tbl.ethnic)
#tbl.ethnic$E.BAME<-1-tbl.ethnic$E.WH
write.csv(tbl.ethnic,paste0(path,"/temp/ethnicity.msoa.csv"))

indep.var<-join_all(list(tbl.inc.paye.msoa,tbl.no.edu,tbl.non.eng.speak,tbl.soc.g,tbl.ten,tbl.beds,
                         tbl.pop.den,tbl.veh,tbl.hospital.msoa,tbl.park,tbl.market.msoa,tbl.emp.msoa,
                         tbl.pop.health,tbl.ethnic),by=c("MSOA11CD","MSOA11NM"),type="left",match="all")
                    
indep.var$V.03<-indep.var$V.03+indep.var$V.04
indep.var<-indep.var[c(-20)] #drop the V.04 
saveRDS(indep.var,"indep.var.msoa.rda")
rm(list=ls())

Sys.setlocale(locale="English_United Kingdom")
rm(list=ls())
setwd("d:/OneDrive - Nexus365/WORK/Document/RESEARCH/COVID19/Accessibility analysis/4th_modelling/")
path<-"d:/OneDrive - Nexus365/WORK/Document/RESEARCH/COVID19/Accessibility analysis"

indep.data<-readRDS(paste0(path,"/3rd_modelling/indep.data.re.rda"))
dep.over25<-readRDS(paste0(path,"/3rd_modelling/vac.uptake.over25.rda"))
require(dplyr)
require(sf)
input.data<-left_join(dep.over25,indep.data,by="MSOA11CD")
input.data$V_YES<-1-input.data$V.00

#Investgating the GOF between V.00 and V.03
temp<-as.data.frame(input.data[c(1,9:50)])
temp<-temp[c(-11,-42)] ##Area, geometry

require(BBmisc)
temp.std<-normalize(temp, method = "standardize", range = c(0, 1), margin = 1L, on.constant = "quiet")

require(MASS)
input.data.re<-input.data[c(1,4:8,49)]
input.data.re<-left_join(input.data.re,temp.std,by="MSOA11CD")

test.nb<-glm.nb(v.1st.over25 ~ INC_20+ SG_C1+ V.03+ res.den+
                  part_time+ E.IN+ E.CAR+ NO_OTHER+ 
                  med.age+ offset(log(p.over25)),data=input.data.re) %>% stepAIC()

require(AER)
vif(test.nb)

test.nb.01<-glm.nb(v.1st.over25 ~ INC_20+ SG_C1+ V.00+ res.den+
                  part_time+ E.IN+ E.CAR+ NO_OTHER+ 
                  med.age+ offset(log(p.over25)),data=input.data.re) ##AIC = 95431.63

test.nb.02<-glm.nb(v.1st.over25 ~ INC_20+ SG_C1+ V.03+ res.den+
                     part_time+ E.IN+ E.CAR+ NO_OTHER+ 
                     med.age+ offset(log(p.over25)),data=input.data.re) ##AIC = 95807.3


test.nb.03<-glm.nb(v.1st.over25 ~ INC_20+ SG_C1+ V_YES + res.den+
                     part_time+ E.IN+ E.CAR+ NO_OTHER+ 
                     med.age+ offset(log(p.over25)),data=input.data.re) ##AIC = 95807.3

anova(test.nb.01,test.nb.02, test.nb.03)

anova(test.nb, test.nb.01)
fit.nb<-test.nb

##updating the accessibility indiciator
car.acc<-read.csv(paste0(path,"/OTP result/msoa.car.acc.join.re.m.csv"))
pub.acc<-read.csv(paste0(path,"/OTP result/msoa.pub.tuesday.12pm.m.csv"))

car.acc.45<-car.acc[c("MSOA11CD.row","car.min45")]
pub.acc.60<-pub.acc[c("MSOA11CD.row","tuesday.12pm60")]

msoa<-st_read(paste0(path,"/Data/Administrative boundary/MSOA_2011_SUPER_GEN/msoa.super.gen.fix.geometry.geojson"))
msoa$MSOA11CD.row<-rownames(msoa);msoa$MSOA11CD.row<-as.numeric(msoa$MSOA11CD.row)
acc<-left_join(msoa,car.acc[c(2,5)],by="MSOA11CD.row")
acc<-left_join(acc,pub.acc[c(2,6)],by="MSOA11CD.row")
colnames(acc)<-c("MSOA11","MSOA11CD","MSOA11CD.row","car.min45","pub.min60","geometry")
acc$car.min45[is.na(acc$car.min45)]<-0
acc$pub.min60[is.na(acc$pub.min60)]<-0
acc.std<-normalize(as.data.frame(acc[c(4,5)]), method = "standardize", range = c(0, 1), margin = 1L, on.constant = "quiet")
acc.std$MSOA11CD<-acc$MSOA11CD;acc.std<-acc.std[-c(3)]
input.data.re<-left_join(input.data.re,acc.std,by="MSOA11CD")

#input.data.re$E<-input.data.re$p.over25 * (sum(input.data.re$v.1st.over25)/sum(input.data.re$p.over25)) ##expected vaccine uptake
fit.nb.acc<-glm.nb(v.1st.over25 ~ INC_20+ SG_C1+ V.03+ res.den+
                     part_time+ E.IN+ E.CAR+ NO_OTHER+ 
                     med.age+ car.min45 + pub.min60 + offset(log(p.over25)),data=input.data.re)

require(RANN)
require(spdep)
coords <- coordinates(as(input.data.re,"Spatial"))
knear4 <- knn2nb(knearneigh(coords, k=4))
knear4nb <- nb2listw(knear4, style="W")  

require(spdep)
input.data.re$lag=lag.listw(x=knear4nb, var=input.data.re$v.1st.over25/input.data.re$p.over25)

fit.nb.lag<-glm.nb(v.1st.over25 ~ INC_20+ SG_C1+ V.03+ res.den+
                     part_time+E.IN+ E.CAR+ NO_OTHER+ 
                     med.age+ lag + offset(log(p.over25)),data=input.data.re) %>% stepAIC()

fit.nb.acc<-glm.nb(v.1st.over25 ~ INC_20+ SG_C1+ V.03+ res.den+
                         part_time+E.IN+ E.CAR+ NO_OTHER+ 
                         med.age+ car.min45+pub.min60+offset(log(p.over25)),data=input.data.re) %>% stepAIC()


fit.nb.acc.lag<-glm.nb(v.1st.over25 ~ INC_20+ SG_C1+ V.03+ res.den+
                         part_time+E.IN+ E.CAR+ NO_OTHER+ 
                         med.age+ car.min45+pub.min60+lag + offset(log(p.over25)),data=input.data.re) %>% stepAIC()

lm.morantest(fit.nb,listw=knear4nb)
lm.morantest(fit.nb.acc,listw=knear4nb)
lm.morantest(fit.nb.lag,listw=knear4nb)
lm.morantest(fit.nb.acc.lag,listw=knear4nb)

require(mfx)
irr.test.nb=negbinirr(fit.nb,input.data.re)
irr.test.lag.nb=negbinirr(fit.nb.lag,input.data.re)
irr.test.acc.nb=negbinirr(fit.nb.acc,input.data.re)
irr.test.both.nb=negbinirr(fit.nb.acc.lag,input.data.re)

require(texreg)
require(stargazer)
irr.t1=stargazer(list(fit.nb,fit.nb.acc,fit.nb.lag,fit.nb.acc.lag), type="html", stars = c(0.001, 0.01, 0.05), bold = 0.01,
                 title = "Global NB model for 1st dose vaccine coverage over 25",out="irr.nb.global.model.17Nov2021.html")

me.fit<-ME(v.1st.over25 ~ INC_20+SG_C1+V.03+res.den+
             part_time+ E.IN+E.CAR+NO_OTHER+
             med.age+car.min60+pub.min60+lag+offset(log(p.over25)),input.data.re,
           family=negative.binomial(417.22), listw = knear4nb, verbose=T,alpha=.05)

summary(fit.nb.acc.lag, corr=TRUE)

require(GWmodel)
require(spatialreg)
dist<-gw.dist(coordinates(as(input.data.re,"Spatial")))

start<-Sys.time()
bw.fl<-bw.ggwr(v.1st.over25 ~ INC_20+SG_C1+V.03+res.den+
                 part_time+ E.IN+E.CAR+NO_OTHER+
                 med.age+car.min60+pub.min60+offset(log(p.over25)),as(input.data.re,"Spatial"),dMat=dist,kernel="bisquare",adaptive = TRUE,family="poisson",longlat=FALSE)
end<-Sys.time()
dur.bw<-end-start
start<-Sys.time()
fit.gwpr<-ggwr.basic(v.1st.over25 ~ INC_20+SG_C1+V.03+res.den+
                       part_time+ E.IN+E.CAR+NO_OTHER+
                       med.age+car.min60+pub.min60+lag+offset(log(p.over25)),bw=bw.fl,as(input.data.re,"Spatial"),dMat=dist,kernel="bisquare",adaptive=TRUE,cv=TRUE,family="poisson")
end<-Sys.time()
gwpr.est<-end-start

ggwr.po.est<-fit.gwpr$SDF
capture.output(print(fit.gwpr),file="summary_GWRP_8Nov2021.doc")
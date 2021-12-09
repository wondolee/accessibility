Sys.setlocale(locale="English_United Kingdom")
rm(list=ls())
setwd("d:/OneDrive - Nexus365/WORK/Document/RESEARCH/COVID19/Accessibility analysis/4th_modelling")
require(sf)
full.input<-st_read("full.input.data.re.geojson")
full.input.tm<-st_transform(full.input,27700)

## dataset
library(spdep)
K <- 4    # number of nearest neighbour
Y <- as.vector(full.input$v.1st.over25)
X<-as.data.frame(full.input[c("INC_20","SG_C1","V.03","res.den", "part_time", "E.IN", "E.CAR", "NO_OTHER","med.age","car.min45","pub.min60")])
X<-X[c(-12)]
X <- as.matrix(X)
Z<-as.vector(log(full.input$p.over25))

full.input.po<-st_centroid(full.input,of_largest_polygon = TRUE)
full.input.po<-as_Spatial(full.input.po)
full.input.re<-as.data.frame(full.input.po)

Sp <- as.matrix(cbind(full.input.re$coords.x1, full.input.re$coords.x2))
knn <- knn2nb(knearneigh(Sp, k=K))
listw_10nn_dates <- nb2listw(knn)
W <- as(as_dgRMatrix_listw(listw_10nn_dates), "CsparseMatrix")*K

## fitting
source("d:/WORKSPACE/GIT/SCR/SCR-function.R")
fit.scr.05.200.cl7 <- SCR(Y, X, W, Sp, offset=Z, Phi=0.5,G=7, family="NB",maxitr=200)
fit.sfcr.05.200.cl7 <- SCR(Y, X, W, Sp, offset=Z, Phi=0.5,fuzzy=T,G=7, family="NB",maxitr=200)

fit.scr.06.200.cl7 <- SCR(Y, X, W, Sp, offset=Z, Phi=0.6,G=7, family="NB",maxitr=200)
fit.sfcr.06.200.cl7 <- SCR(Y, X, W, Sp, offset=Z, Phi=0.6,fuzzy=T,G=7, family="NB",maxitr=200)

fit.scr.075.200.cl7 <- SCR(Y, X, W, Sp, offset=Z, Phi=0.75,G=7, family="NB",maxitr=200)
fit.sfcr.075.200.cl7 <- SCR(Y, X, W, Sp, offset=Z, Phi=0.75,fuzzy=T,G=7, family="NB",maxitr=200)

full.input.tm$G7<-as.factor(fit.sfcr.05.200.cl7$group)
levels(full.input.tm$G7)<-factor(c("G1","G2","G3","G4","G5","G6","G7"))
#full.input$G<-factor(full.input$G,levels=c("G1","G2","G3","G4","G5","G6","G7","G8"))

require(ggplot2)
require(RColorBrewer)
require(classInt)
require(ggrepel)
require(scales)
require(rgdal)
require(ggspatial)
require(extrafont)
require(viridis)
loadfonts(device = "win")
font_import(pattern = 'Raleway',prompt=FALSE)

eng.la<-st_read("D:/WORKSPACE/COVID19/REVISE_LAST/ENGLAND_REGIONS.shp")
la.coord<- st_read("D:/WORKSPACE/COVID19/REVISE_LAST/ENGLAND_REGIONS_POINTS.shp")
la.coords <- as.data.frame(sf::st_coordinates(la.coord))
la.coords$NAME <- eng.la$RGN11NM

final.cl.map<-ggplot()+
  geom_sf(data=full.input.tm, aes(fill = factor(G7)), colour = NA)+
  labs(fill = "Spatially fuzzy clusters of 1st vaccine uptake over 25 (ϕ=0.5)") + 
  theme_bw(base_size=11)+
  theme(legend.position = "bottom",legend.box="vertical",legend.margin=margin(),axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  guides(fill=guide_legend(nrow=2,byrow=TRUE))+
  annotation_scale(location = "br", height = unit(0.3, "cm"))+
  annotation_north_arrow(location = "tl", 
                         style = north_arrow_nautical, 
                         height = unit(2, "cm"),
                         width = unit(2, "cm"))+
  geom_sf(data=eng.la,fill=NA,color="black",size=0.3,show.legend=FALSE)+
  geom_label_repel(data=la.coords, aes(X, Y, label = NAME),
                   colour = "Black",size = NA, 
                   alpha = 0.8,label.padding=0.1,family="Raleway",fontface="bold")
ggsave("final.cl.map.phi=0.5.png",final.cl.map,width=200, height=200, units = "mm", dpi = 300, bg = "white")

require(dplyr)
census.lookup<-read.csv("d:/OneDrive - Nexus365/WORK/Document/RESEARCH/COVID19/Accessibility analysis/SCR exercise/RE/Census_bodies_skywalk_table_England_DEC_2020.csv")[c(5:10)]
msoa.to.la<-distinct(census.lookup);eng.msoa.to.la<-msoa.to.la[grepl("E0",msoa.to.la$MSOA11CD),]
msoa.in.london<-subset(eng.msoa.to.la,RGN20NM=="London")
full.input.tm<-left_join(full.input.tm,eng.msoa.to.la,by="MSOA11CD")

london.lad<-distinct(subset(eng.msoa.to.la[c(3:6)],RGN20NM=="London"))
eng.lad<-st_read("D:/OneDrive - Nexus365/WORK/Document/RESEARCH/COVID19/Accessibility analysis/SCR exercise/Local_Authority_Districts_(December_2020)_UK_BFC.geojson")
re.london.lad<-right_join(eng.lad,london.lad,by=c("LAD20CD","LAD20NM"))
re.london.lad<-st_transform(re.london.lad,27700)

london.name <- as.data.frame(st_coordinates((sf::st_centroid(re.london.lad))))
london.name$NAME <- re.london.lad$LAD20NM

london.final.cl.map<-ggplot()+
  geom_sf(data=subset(full.input.tm,RGN20NM=="London"), aes(fill = factor(G7)), colour = NA)+
  labs(fill = "Spatially fuzzy clusters of 1st vaccine uptake over 25 in London (ϕ=0.5)") + 
  theme_bw(base_size=11)+
  theme(legend.position = "bottom",legend.box="vertical",legend.margin=margin(),axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  guides(fill=guide_legend(nrow=2,byrow=TRUE))+
  annotation_scale(location = "br", height = unit(0.3, "cm"))+
  annotation_north_arrow(location = "tl", 
                         style = north_arrow_nautical, 
                         height = unit(2, "cm"),
                         width = unit(2, "cm"))+
  geom_sf(data=re.london.lad,fill=NA,color="black",size=0.3,show.legend=FALSE)+
  geom_label_repel(data=london.name, aes(X, Y, label = NAME),
                   colour = "Black",size = rel(3.5), 
                   alpha = 0.8,max.overlaps = 1000,label.padding=0.1,box.padding = 0.80, point.padding = 0.2,family="Raleway",fontface="bold",na.rm=TRUE)

ggsave("london.final.cl.map.phi=0.5.png",london.final.cl.map,width=200, height=200, units = "mm", dpi = 300, bg = "white")

covariates<-readRDS("D:/OneDrive - Nexus365/WORK/Document/RESEARCH/COVID19/Accessibility analysis/3rd_modelling/indep.data.re.rda")

des.input<-as.data.frame(full.input.tm)
des.input<-des.input[c(-56)]
des.input<-des.input[c(1:6,50)]
des.input<-left_join(des.input,covariates,by="MSOA11CD")

##updating the accessibility indiciator
path<-"d:/OneDrive - Nexus365/WORK/Document/RESEARCH/COVID19/Accessibility analysis"
car.acc<-read.csv(paste0(path,"/OTP result/msoa.car.acc.join.re.m.csv"))
pub.acc<-read.csv(paste0(path,"/OTP result/msoa.pub.tuesday.12pm.m.csv"))

car.acc.45<-car.acc[c("MSOA11CD.row","car.min45")]
pub.acc.60<-pub.acc[c("MSOA11CD.row","tuesday.12pm60")]

msoa<-st_read(paste0(path,"/Data/Administrative boundary/MSOA_2011_SUPER_GEN/msoa.super.gen.fix.geometry.geojson"))
msoa$MSOA11CD.row<-rownames(msoa);msoa$MSOA11CD.row<-as.numeric(msoa$MSOA11CD.row)
acc<-left_join(msoa,car.acc[c(2,5)],by="MSOA11CD.row")
acc<-left_join(acc,pub.acc[c(2,6)],by="MSOA11CD.row")
colnames(acc)<-c("MSOA11","MSOA11CD","MSOA11CD.row","car.min45","pub.min60","geometry")

des.input<-left_join(des.input,as.data.frame(acc),by="MSOA11CD")
des.input$car.min45[is.na(des.input$car.min45)]<-0
des.input$pub.min60[is.na(des.input$pub.min60)]<-0
des.input<-des.input[c(-48,-49,-52)]
saveRDS(des.input,"final.input.data.for.descriptive.analysis.06Dec2021.rda")

require(psych)
describe(v.1st.over25.rate~G7, data=des.input)
describe(car.min45~G7, data=des.input)
describe(pub.min60~G7, data=des.input)
describe(med.age~G7, data=des.input)
describe(INC_20~G7, data=des.input)
describe(NO_OTHER~G7, data=des.input)
pop.by.g7<-as.data.frame(des.input %>% group_by(G7) %>% summarise(sum.pop.over25=sum(p.over25)) %>% ungroup ())

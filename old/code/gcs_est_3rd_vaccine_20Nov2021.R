Sys.setlocale(locale="English_United Kingdom")
rm(list=ls())
setwd("d:/OneDrive - Nexus365/WORK/Document/RESEARCH/COVID19/Accessibility analysis/SCR exercise/")
require(sf)
full.input<-st_read("D:/OneDrive - Nexus365/WORK/Document/RESEARCH/COVID19/Accessibility analysis/SCR exercise/full.input.data.geojson")
full.input.tm<-st_transform(full.input,27700)

## dataset
library(spdep)
K <- 4    # number of nearest neighbour
Y <- as.vector(full.input$v.1st.over25)
X<-as.data.frame(full.input[c("INC_20","SG_C1","V.03","res.den", "part_time", "E.IN", "E.CAR", "NO_OTHER","med.age","car.min60","pub.min60")])
X<-X[c(-12)]
X <- as.matrix(X)
Z<-as.vector(full.input$p.over25)

full.input.po<-st_centroid(full.input,of_largest_polygon = TRUE)
full.input.po<-as_Spatial(full.input.po)
full.input.re<-as.data.frame(full.input.po)

Sp <- as.matrix(cbind(full.input.re$coords.x1, full.input.re$coords.x2))
knn <- knn2nb(knearneigh(Sp, k=K))
listw_10nn_dates <- nb2listw(knn)
W <- as(as_dgRMatrix_listw(listw_10nn_dates), "CsparseMatrix")*K

## fitting
source("d:/WORKSPACE/GIT/SCR/SCR-function.R")
select.50 <- SCR.select(Y, X, W, Sp, offset=log(Z), G.set=2:8, family="NB", maxitr=50)   # selection of the number of clusters
select.100<- SCR.select(Y, X, W, Sp, offset=log(Z), G.set=2:8, family="NB", maxitr=100)
select.150<- SCR.select(Y, X, W, Sp, offset=log(Z), G.set=2:8, family="NB", maxitr=150)
select.200<-SCR.select(Y, X, W, Sp, offset=log(Z), G.set=2:8, family="NB", maxitr=200)

fit.scr.50.cl6 <- SCR(Y, X, W, Sp, offset=log(Z), G=6, family="NB",maxitr=50)  # estimation with the selected G
fit.scr.100.cl6 <- SCR(Y, X, W, Sp, offset=log(Z), G=6, family="NB",maxitr=100)
fit.scr.150.cl6 <- SCR(Y, X, W, Sp, offset=log(Z), G=6, family="NB",maxitr=150)
fit.scr.200.cl6 <- SCR(Y, X, W, Sp, offset=log(Z), G=6, family="NB",maxitr=200)

fit.sfcr.50.cl6 <- SCR(Y, X, W, Sp, offset=log(Z), fuzzy=T, G=6, family="NB",maxitr=50)  # estimation with the selected G
fit.sfcr.100.cl6 <- SCR(Y, X, W, Sp, offset=log(Z), fuzzy=T, G=6, family="NB",maxitr=100)
fit.sfcr.150.cl6 <- SCR(Y, X, W, Sp, offset=log(Z), fuzzy=T,G=6, family="NB",maxitr=150)
fit.sfcr.200.cl6 <- SCR(Y, X, W, Sp, offset=log(Z), fuzzy=T,G=6, family="NB",maxitr=200)

fit.scr.50.cl7 <- SCR(Y, X, W, Sp, offset=log(Z), G=7, family="NB",maxitr=50)  # estimation with the selected G
fit.scr.100.cl7 <- SCR(Y, X, W, Sp, offset=log(Z), G=7, family="NB",maxitr=100)
fit.scr.150.cl7 <- SCR(Y, X, W, Sp, offset=log(Z), G=7, family="NB",maxitr=150)
fit.scr.200.cl7 <- SCR(Y, X, W, Sp, offset=log(Z), G=7, family="NB",maxitr=200)

fit.sfcr.50.cl7 <- SCR(Y, X, W, Sp, offset=log(Z), fuzzy=T, G=7, family="NB",maxitr=50)  # estimation with the selected G
fit.sfcr.100.cl7 <- SCR(Y, X, W, Sp, offset=log(Z), fuzzy=T, G=7, family="NB",maxitr=100)
fit.sfcr.150.cl7 <- SCR(Y, X, W, Sp, offset=log(Z), fuzzy=T,G=7, family="NB",maxitr=150)
fit.sfcr.200.cl7 <- SCR(Y, X, W, Sp, offset=log(Z), fuzzy=T,G=7, family="NB",maxitr=200)

fit.scr.50.cl8 <- SCR(Y, X, W, Sp, offset=log(Z), G=8, family="NB",maxitr=50)  # estimation with the selected G
fit.scr.100.cl8 <- SCR(Y, X, W, Sp, offset=log(Z), G=8, family="NB",maxitr=100)
fit.scr.150.cl8 <- SCR(Y, X, W, Sp, offset=log(Z), G=8, family="NB",maxitr=150)
fit.scr.200.cl8 <- SCR(Y, X, W, Sp, offset=log(Z), G=8, family="NB",maxitr=200)

fit.sfcr.50.cl8 <- SCR(Y, X, W, Sp, offset=log(Z), fuzzy=T, G=8, family="NB",maxitr=50)  # estimation with the selected G
fit.sfcr.100.cl8 <- SCR(Y, X, W, Sp, offset=log(Z), fuzzy=T, G=8, family="NB",maxitr=100)
fit.sfcr.150.cl8 <- SCR(Y, X, W, Sp, offset=log(Z), fuzzy=T,G=8, family="NB",maxitr=150)
fit.sfcr.200.cl8 <- SCR(Y, X, W, Sp, offset=log(Z), fuzzy=T,G=8, family="NB",maxitr=200)

fit.sfcr.50.cl9 <- SCR(Y, X, W, Sp, offset=log(Z), fuzzy=T, G=9, family="NB",maxitr=50)  # estimation with the selected G
fit.sfcr.100.cl9 <- SCR(Y, X, W, Sp, offset=log(Z), fuzzy=T, G=9, family="NB",maxitr=100)
fit.sfcr.150.cl9 <- SCR(Y, X, W, Sp, offset=log(Z), fuzzy=T,G=9, family="NB",maxitr=150)
fit.sfcr.200.cl9 <- SCR(Y, X, W, Sp, offset=log(Z), fuzzy=T,G=9, family="NB",maxitr=200)

fit.sfcr.50.cl10 <- SCR(Y, X, W, Sp, offset=log(Z), fuzzy=T, G=10, family="NB",maxitr=50)  # estimation with the selected G
fit.sfcr.100.cl10 <- SCR(Y, X, W, Sp, offset=log(Z), fuzzy=T, G=10, family="NB",maxitr=100)
fit.sfcr.150.cl10 <- SCR(Y, X, W, Sp, offset=log(Z), fuzzy=T,G=10, family="NB",maxitr=150)
fit.sfcr.200.cl10 <- SCR(Y, X, W, Sp, offset=log(Z), fuzzy=T,G=10, family="NB",maxitr=200)

full.input.tm$G<-as.factor(fit.sfcr.200.cl8$group)
levels(full.input.tm$G)<-factor(c("G1","G2","G3","G4","G5","G6","G7","G8"))
full.input$G<-factor(full.input$G,levels=c("G1","G2","G3","G4","G5","G6","G7","G8"))

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

map.1st.vac.clu<-ggplot()+
  geom_sf(data=full.input.tm, aes(fill = factor(G)), colour = NA)+
  labs(fill = "Spatially fuzzy clusters of 1st vaccine uptake over 25") + 
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

ggsave("final.gcs.sfcr.c8.iter200.png",map.1st.vac.clu,width=200, height=200, units = "mm", dpi = 300, bg = "white")

require(dplyr)
census.lookup<-read.csv("Census_bodies_skywalk_table_England_DEC_2020.csv")[c(5:10)]
msoa.to.la<-distinct(census.lookup);eng.msoa.to.la<-msoa.to.la[grepl("E0",msoa.to.la$MSOA11CD),]
#msoa.in.london<-subset(eng.msoa.to.la,RGN20NM=="London")
full.input.tm<-left_join(full.input.tm,eng.msoa.to.la,by="MSOA11CD")

london.lad<-distinct(subset(eng.msoa.to.la[c(3:6)],RGN20NM=="London"))
eng.lad<-st_read("D:/OneDrive - Nexus365/WORK/Document/RESEARCH/COVID19/Accessibility analysis/SCR exercise/Local_Authority_Districts_(December_2020)_UK_BFC.geojson")
re.london.lad<-right_join(eng.lad,london.lad,by=c("LAD20CD","LAD20NM"))
re.london.lad<-st_transform(re.london.lad,27700)

london.name <- as.data.frame(st_coordinates((sf::st_centroid(re.london.lad))))
london.name$NAME <- re.london.lad$LAD20NM

map.london.1st.vac.clu<-ggplot()+
  geom_sf(data=subset(full.input.tm,RGN20NM=="London"), aes(fill = factor(G)), colour = NA)+
  labs(fill = "Spatially fuzzy clusters of 1st vaccine uptake over 25 in London") + 
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

ggsave("final.london.gcs.sfcr.c8.iter200.png",map.london.1st.vac.clu,width=200, height=200, units = "mm", dpi = 300, bg = "white")


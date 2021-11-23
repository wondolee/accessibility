Sys.setlocale(locale="English_United Kingdom")
rm(list=ls())
setwd("d:/OneDrive - Nexus365/WORK/Document/RESEARCH/COVID19/Accessibility analysis/SCR exercise/")
path<-"d:/OneDrive - Nexus365/WORK/Document/RESEARCH/COVID19/Accessibility analysis"

require(sf)
car.acc<-read.csv(paste0(path,"/OTP result/msoa.car.acc.join.re.m.csv"))
pub.acc<-read.csv(paste0(path,"/OTP result/msoa.pub.tuesday.12pm.m.csv"))

car.acc.60<-car.acc[c("MSOA11CD.row","car.min60")]
pub.acc.60<-pub.acc[c("MSOA11CD.row","tuesday.12pm60")]

msoa<-st_read(paste0(path,"/Data/Administrative boundary/MSOA_2011_SUPER_GEN/msoa.super.gen.fix.geometry.geojson"))
msoa$MSOA11CD.row<-rownames(msoa);msoa$MSOA11CD.row<-as.numeric(msoa$MSOA11CD.row)

require(dplyr)
acc<-left_join(msoa,car.acc[c(2,6)],by="MSOA11CD.row")
acc<-left_join(acc,pub.acc[c(2,6)],by="MSOA11CD.row")
colnames(acc)<-c("MSOA11","MSOA11CD","MSOA11CD.row","car.min60","pub.min60","geometry")
acc<-st_transform(acc,27700)
acc[is.na(acc)]<-0

require(sf)
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

v.sites<-st_read("D:/OneDrive - Nexus365/WORK/Document/RESEARCH/COVID19/Accessibility analysis/1st_modelling/vac.centres.with.region.geojson")
v.sites<-subset(v.sites,Type=="Vaccination centres")
v.sites<-st_transform(v.sites,27700)

map.vac.cen.car.60<-ggplot()+
  geom_sf(data=subset(acc,car.min60>0), aes(fill = car.min60),colour = "grey20", size = 0)+#colour = 'white'                                                            
  scale_fill_viridis(option="inferno",direction=-1,guide = guide_colorbar(
    direction = "horizontal",
    barheight = unit(2, units = "mm"),
    barwidth = unit(50, units = "mm"),
    draw.ulim = T,
    title.position = 'left',
    # some shifting around
    title.hjust = 0.5,
    label.hjust = 0.5
  ))+
  labs(fill = "Accessible COVID-19 vaccination centres within 60 min by car") + 
  theme_bw(base_size=11)+
  theme(legend.position = "bottom",legend.box="vertical",legend.margin=margin(),axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  #guides(fill=guide_legend(nrow=2,byrow=TRUE))+
    annotation_scale(location = "br", height = unit(0.3, "cm"))+
  annotation_north_arrow(location = "tl", 
                         style = north_arrow_nautical, 
                         height = unit(2, "cm"),
                         width = unit(2, "cm"))+
  geom_sf(data=v.sites,aes(colour = Type),color="grey20",size=0.5)+
  geom_sf(data=subset(acc,car.min60==0), fill="white",color="grey20",size=0)+
  geom_sf(data=eng.la,fill=NA,color="black",size=0.3,show.legend=FALSE)+
  geom_label_repel(data=la.coords, aes(X, Y, label = NAME),
                   colour = "Black",size = NA, 
                   alpha = 0.8,label.padding=0.1,family="Raleway",fontface="bold")

ggsave("map.vac.cen.car.60.png",map.vac.cen.car.60,width=200, height=200, units = "mm", dpi = 300, bg = "white")

map.vac.cen.pub.60<-ggplot()+
  geom_sf(data=subset(acc,pub.min60>0), aes(fill = pub.min60),colour = "grey20", size = 0)+#colour = 'white'                                                            
  scale_fill_viridis(option="inferno",direction=-1,guide = guide_colorbar(
    direction = "horizontal",
    barheight = unit(2, units = "mm"),
    barwidth = unit(50, units = "mm"),
    draw.ulim = T,
    title.position = 'left',
    # some shifting around
    title.hjust = 0.5,
    label.hjust = 0.5
  ))+
  labs(fill = "Accessible COVID-19 vaccination centres within 60 min by public transportation") + 
  theme_bw(base_size=11)+
  theme(legend.position = "bottom",legend.box="vertical",legend.margin=margin(), axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  #guides(fill=guide_legend(nrow=2,byrow=TRUE))+
  annotation_scale(location = "br", height = unit(0.3, "cm"))+
  annotation_north_arrow(location = "tl", 
                         style = north_arrow_nautical, 
                         height = unit(2, "cm"),
                         width = unit(2, "cm"))+
  geom_sf(data=v.sites,aes(colour = Type),color="grey20",size=0.5)+
  geom_sf(data=subset(acc,pub.min60==0), fill="white",color="grey20",size=0)+
  geom_sf(data=eng.la,fill=NA,color="black",size=0.3,show.legend=FALSE)+
  geom_label_repel(data=la.coords, aes(X, Y, label = NAME),
                   colour = "Black",size = NA, 
                   alpha = 0.8,label.padding=0.1,family="Raleway",fontface="bold")

ggsave("map.vac.cen.pub.60.png",map.vac.cen.pub.60,width=200, height=200, units = "mm", dpi = 300, bg = "white")

##Only london
census.lookup<-read.csv("Census_bodies_skywalk_table_England_DEC_2020.csv")[c(5,6,9,10)]
msoa.to.la<-distinct(census.lookup);eng.msoa.to.la<-msoa.to.la[grepl("E0",msoa.to.la$MSOA11CD),]
#msoa.in.london<-subset(eng.msoa.to.la,RGN20NM=="London")
acc<-left_join(acc[c(2,4,5,6)],eng.msoa.to.la,by="MSOA11CD")

map.london.vac.cen.car.60<-ggplot()+
  geom_sf(data=subset(acc,RGN20NM=="London" & car.min60>0), aes(fill = car.min60),colour = "grey20", size = 0)+#colour = 'white'  
  scale_fill_viridis(option="inferno",direction=-1,guide = guide_colorbar(
    direction = "horizontal",
    barheight = unit(2, units = "mm"),
    barwidth = unit(50, units = "mm"),
    draw.ulim = T,
    title.position = 'left',
    # some shifting around
    title.hjust = 0.5,
    label.hjust = 0.5
  ))+
  labs(fill = "Accessible COVID-19 vaccination centres within 60 min by car") + 
  theme_bw(base_size=11)+
  theme(legend.position = "bottom",legend.box="vertical",legend.margin=margin(),axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  #guides(fill=guide_legend(nrow=2,byrow=TRUE))+
  annotation_scale(location = "br", height = unit(0.3, "cm"))+
  annotation_north_arrow(location = "tl", 
                         style = north_arrow_nautical, 
                         height = unit(2, "cm"),
                         width = unit(2, "cm"))+
  geom_sf(data=subset(acc,RGN20NM=="London" & car.min60==0), fill="white",color="grey20",size=0)+
  geom_sf(data=subset(v.sites,RGN11NM=="London"),aes(colour = Type),color="grey50",size=2)
  #geom_sf(data=eng.la,fill=NA,color="black",size=0.3,show.legend=FALSE)+
  #geom_label_repel(data=la.coords, aes(X, Y, label = NAME),
                   #colour = "Black",size = NA, 
                   #alpha = 0.8,label.padding=0.1,family="Raleway",fontface="bold")
  
ggsave("map.london.vac.cen.car.60.png",map.london.vac.cen.car.60,width=200, height=200, units = "mm", dpi = 300, bg = "white")

map.london.vac.cen.pub.60<-ggplot()+
  geom_sf(data=subset(acc,RGN20NM=="London" & pub.min60>0), aes(fill = pub.min60),colour = "grey20", size = 0)+#colour = 'white'                                                            
  scale_fill_viridis(option="inferno",direction=-1,guide = guide_colorbar(
    direction = "horizontal",
    barheight = unit(2, units = "mm"),
    barwidth = unit(50, units = "mm"),
    draw.ulim = T,
    title.position = 'left',
    # some shifting around
    title.hjust = 0.5,
    label.hjust = 0.5
  ))+
  labs(fill = "Accessible COVID-19 vaccination centres within 60 min by public transportation") + 
  theme_bw(base_size=11)+
  theme(legend.position = "bottom",legend.box="vertical",legend.margin=margin(), axis.title.x = element_blank(),
        axis.title.y = element_blank())+
    #guides(fill=guide_legend(nrow=2,byrow=TRUE))+
  annotation_scale(location = "br", height = unit(0.3, "cm"))+
  annotation_north_arrow(location = "tl", 
                         style = north_arrow_nautical, 
                         height = unit(2, "cm"),
                         width = unit(2, "cm"))+
  geom_sf(data=subset(acc,RGN20NM=="London" & pub.min60==0), fill="white",color="grey20",size=0)+
  geom_sf(data=subset(v.sites,RGN11NM=="London"),aes(colour = Type),color="grey50",size=2)
  #geom_sf(data=eng.la,fill=NA,color="black",size=0.3,show.legend=FALSE)+
  #geom_label_repel(data=la.coords, aes(X, Y, label = NAME),
                  #colour = "Black",size = NA, 
                  #alpha = 0.8,label.padding=0.1,family="Raleway",fontface="bold")

ggsave("map.london.vac.cen.pub.60.png",map.london.vac.cen.pub.60,width=200, height=200, units = "mm", dpi = 300, bg = "white")

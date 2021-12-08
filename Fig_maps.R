library(sf)
library(tidyverse)


load("Objects and Data/4_dp_outs_towns.rda")
load("Objects and Data/0_MA_town_shapefiles.rda")
ma_towns<-ma_towns[!duplicated(ma_towns$city),]
load("Objects and Data/0_species_list.rda")
load("Objects and Data/5_DP_PCA.rda")


#Maps
#-----------------------------------------------------------------------------------------
df<-dp_outs_towns%>%
  group_by(actual_city_town)%>%
  summarise(across(c("saidi", toupper(name_list$sp_file)), mean,na.rm=T))%>%
  ungroup()

towns_birds<-left_join(ma_towns,df,by=c("city"="actual_city_town"))%>%
  pivot_longer(toupper(name_list$sp_file),names_to="Species",values_to="DP")

#plot species grouped by PC2 (Assumed to be grouping by spatial patterns)
s1<-row.names(vectors[vectors$PC2>=0.2,])
s2<-row.names(vectors[vectors$PC2>-0.2&vectors$PC2<0.2,])
s3<-row.names(vectors[vectors$PC2<=-0.2,])

#Group 1
p1<-ggplot() +
  #specify Bird Species
  geom_sf(data = towns_birds%>%filter(Species%in%s1), aes(geometry=geometry,fill = DP)) +
  scale_fill_viridis_c(option = "plasma",
                       na.value = "grey60")+
  facet_wrap(~Species)+
  theme_bw()+
  theme(legend.title = element_text(size = 10), 
        legend.text = element_text(size = 10),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        strip.text = element_text(size=10),
        strip.background = element_rect(color="black", fill="white", size=1.5,
                                        linetype="blank"))+
  guides(fill=guide_colourbar(barwidth=0.7))+
  labs(title="A")

#Group2
p2<-ggplot() +
  #specify Bird Species
  geom_sf(data = towns_birds%>%filter(Species%in%s2), aes(geometry=geometry,fill = DP)) +
  scale_fill_viridis_c(option = "plasma",
                       na.value = "grey60")+
  facet_wrap(~Species,ncol = 2)+
  theme_bw()+
  theme(legend.title = element_text(size = 10), 
        legend.text = element_text(size = 10),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        strip.text = element_text(size=10),
        strip.background = element_rect(color="black", fill="white", size=1.5,
                                        linetype="blank"))+
  guides(fill=guide_colourbar(barwidth=0.7))+
  labs(title="B")

#Group3
p3<-ggplot() +
  #specify Bird Species
  geom_sf(data = towns_birds%>%filter(Species%in%s3), aes(geometry=geometry,fill = DP)) +
  scale_fill_viridis_c(option = "plasma",
                       na.value = "grey60")+
  facet_wrap(~Species,ncol = 2)+
  theme_bw()+
  theme(legend.title = element_text(size = 10), 
        legend.text = element_text(size = 10),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        strip.text = element_text(size=10),
        strip.background = element_rect(color="black", fill="white", size=1.5,
                                        linetype="blank"))+
  guides(fill=guide_colourbar(barwidth=0.7))+
  labs(title="C")


p_map<-ggplot() +
  #specify Bird Species
  geom_sf(data = towns_birds%>%filter(Species%in%c("TUVU","EUST")), aes(geometry=geometry,fill = DP)) +
  scale_fill_viridis_c(option = "plasma",
                       na.value = "grey60")+
  facet_wrap(~Species,ncol = 2)+
  theme_bw()+
  theme(legend.title = element_text(size = 10), 
        legend.text = element_text(size = 10),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        strip.text = element_text(size=10),
        strip.background = element_rect(color="black", fill="white", size=1.5,
                                        linetype="blank"))+
  guides(fill=guide_colourbar(barwidth=0.7))
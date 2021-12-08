library(rgdal)
library(lubridate)
library(sf)
library(tidyverse)
library(ggpubr)

dp<-read.csv("Outputs/dp_out_towns.csv")
ma_towns<-st_read("Raw_Data/MA_Shapefiles/ma_towns_namescorrected.shp")%>%
  st_set_crs(st_crs(4326))


#Maps
df<-dp%>%
  group_by(actual_city_town)%>%
  summarise(EUST = mean(EUST,na.rm=T),
            TUVU = mean(TUVU,na.rm=T),
            SAIDI=mean(saidi,na.rm = T))%>%
  ungroup()

towns_birds<-left_join(ma_towns,df,by=c("city"="actual_city_town"))

m1<-ggplot() +
  #specify Bird Species
  geom_sf(data = towns_birds, aes(fill = TUVU)) +
  scale_fill_viridis_c(option = "plasma", alpha = 0.8,
                       guide = guide_legend(title.position = "top",
                                            label.position = "bottom",
                                            title="TUVU"))+
  theme_classic(base_size = 12)+
  theme(legend.position = "top",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank())

m2<-ggplot() +
  #specify Bird Species
  geom_sf(data = towns_birds, aes(fill = EUST)) +
  scale_fill_viridis_c(option = "plasma", alpha = 0.8,
                       guide = guide_legend(title.position = "top",
                                            label.position = "bottom",
                                            title="EUST"))+
  theme_classic(base_size = 12)+
  theme(legend.position = "top",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank())

m3<-ggplot() +
  #specify Bird Species
  geom_sf(data = towns_birds, aes(fill = SAIDI)) +
  scale_fill_viridis_c(option = "plasma", alpha = 0.8,
                       guide = guide_legend(title.position = "top",
                                            label.position = "bottom",
                                            title="SAIDI"))+
  theme_classic(base_size = 12)+
  theme(legend.position = "top",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank())

ggarrange(m1,m2,ncol = 2,nrow=1)

ggsave("Figures/Paper Figures/species_townmap.jpeg",
       width=8,
       height = 5,
       dpi = 300,
       units = "in")



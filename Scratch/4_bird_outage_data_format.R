library(lubridate)
library(sf)
library(tidyverse)



load("Objects and Data/3_DP_predictions.rda")
load("Objects and Data/0_MA_town_shapefiles.rda")
load("Objects and Data/0_species_list.rda")
outs<-read.csv("Objects and Data/MA_SAIDI.csv")


#Additional Data cleaning and Prep
#----------------------------------------------------------------------------------------
#1. Filter outage data to animal-caused outages
outs2<- outs%>%
  filter((Reason.For.Outage %in% c("Animal","Bird","Animal - Other","Squirrel")))%>%
  #remove any records with missing Reason data
  filter(!is.na(Reason.For.Outage))%>%
  
#2. summarize outages by week
  #a) add weekly time variables
  mutate(date=mdy(Date.Out),
         week=week(date),
         year=year(date))%>%
  select(-Date.Out)%>%
  
  #b) Calculate SAIDI, Customer Outage Minutes per incident
  #NCOH = (# customers affected * outage duration)/ total customers served (households)
  mutate(saidi=(Original.Number.Customers.Affected*Actual.Duration*60)/hh_total,
         
  #c) Flag records with more customers impacted than total households
         hh_cust_flag=ifelse(hh_total<Original.Number.Customers.Affected,1,0))%>%
  #remove flagged records
    filter(hh_cust_flag!=1)%>%
  select(-hh_cust_flag)%>%
  group_by(actual_city_town,week,year)%>%
  
  #d) aggregate SAIDI per town/week
  summarize(saidi=sum(saidi,na.rm=T),
            nouts=n())%>%
  ungroup()
  
#3. Remove any outliers of SAIDI
  #outliers defined as greater than 3 SDs from the mean
  outs3<-outs2%>%
   mutate(zscore=(saidi - mean(saidi))/sd(saidi))%>%
   filter(zscore<3)
    
  #how many records are removed as outliers
  length(outs2$saidi)-length(outs3$saidi)
  #check distribution of ncoh after outliers are removed
  hist(outs3$saidi)

#4. Create one bird dataset with each species as a variable
  #a) First create a "season" field 
  #   based on eBird status and trends breeding seasons of our study species 
  #(Breeding = Summer, Post-Breeding= Fall, Non-Breeding= Winter, Pre-Breeding=Spring)
  season<-function(x){x%>%
     mutate(month=month(date),
            winter=ifelse(month%in%c(12,1,2),1,0), #Dec-Feb
            spring=ifelse(month%in%c(3,4,5),1,0), #March-May
            summer=ifelse(month%in%c(6,7,8),1,0), #June-Aug
            fall=ifelse(month%in%c(9,10,11),1,0),#Sept-Nov
            season=case_when(
              winter==1~ "winter",
              spring==1 ~ "spring",
              summer==1 ~ "summer",
              fall==1 ~ "fall"))%>%
      dplyr::select(-c("time_observations_started",
                       "day_of_week", "protocol_type",
                       "LLA","TOWN_ID","Area_km2",
                       "eBird.DP.RF.SE",
                       "winter","spring","summer","fall"))
  }

  dp_towns<-lapply(ebirdRF_results_town,season)

  #b) Format bird data so each species file gets added as a species variable
    #list species 4 letter codes
  colnames<-toupper(name_list$sp_file)
    #grab non-species time-town variables
  dp_towns2<-select(dp_towns[[1]],-eBird.DP.RF)
    #Grab all the species detection probabilities
  dp_towns<-lapply(dp_towns,function(x){select(x,"eBird.DP.RF")})
    #column bind them
  dp_towns<-bind_cols(dp_towns)
    #Name them as their species codes
  colnames(dp_towns)<-colnames
    #column bind species DPs with time-town variables
  dp_towns3<-bind_cols(dp_towns2,dp_towns)



#Merge bird and outage data into one dataset
#------------------------------------------------------------------------------------------
dp_outs_towns<-left_join(outs3,dp_towns2, by=c("actual_city_town"="city",
                                               "year"="Year",
                                               "week"="week"))%>%
  dplyr::select(-c("elevation_median","elevation_sd","zscore"))


#write final dataset to files
write.csv(dp_outs_towns,"Outputs/dp_out_towns.csv",row.names = F)

save(dp_outs_towns, file = "Objects and Data/4_dp_outs_towns.rda")

#Turn outages into binary presence/absence
#retain only towns with at least 1 outage record. 
#use all weeks for those towns, filling missing weeks with 0
dp_towns4<-dp_towns3%>%filter(city%in%outs2$actual_city_town)
dp_outs_towns<-left_join(dp_towns4,outs2, by=c("city"="actual_city_town",
                                               "Year"="year",
                                               "week"="week"))%>%
  dplyr::select(-c("elevation_median","elevation_sd"))%>%
  mutate(outage=case_when(!is.na(nouts)~1,
                          is.na(nouts)~0))

#-----------------------------  
#Data exploration figures 
#-----------------------------

#Look at time series of outage frequency and bird abundance statewide
#Outage frequency peaks around June and November, supporting that animal outage activity increases in these months
#Northern flicker, osprey, turkey vulture, common grackle, and brown-headed cowbird seasonal trends 
#most align with outage frequency trends

timeseries<- dp_outs_towns%>%
  group_by(date)%>%
  summarise(a.TUVU = mean(TUVU,na.rm = TRUE),#specify bird species
            b.OSPR = mean(OSPR,na.rm = TRUE),
            c.DOWO = mean(DOWO,na.rm = TRUE),
            d.HOSP = mean(HOSP,na.rm = TRUE),
            e.SAIDI = mean(saidi,na.rm = TRUE))%>%
  gather(measure, value, a.TUVU:e.SAIDI)%>%     ## stack metrics into 1 column for plotting
  mutate(value=replace_na(value,0),
         date=as.Date(date))

ggplot(data = timeseries, aes(x=date, y=value,group=1)) +
  geom_line() +
  theme_bw(base_size = 12)+
  scale_x_date(date_breaks="6 month", minor_breaks=NULL, date_labels="%b %Y")+
  xlab("") +
  ylab("State Average") + 
  facet_grid(measure ~ ., scales='free')

ggsave("../Outputs/ts_out_dp.jpeg",
       width=11,
       height = 7,
       dpi = 300,
       units = "in")


#Spatial correlations across town spatial anomalies through time
#remove dates with less than 3 town records
date_count<-group_by(dp_outs_towns,date)%>%summarise(n=n())%>%
  filter(n>2)
out_space_week<-dp_outs_towns[complete.cases(dp_outs_towns) &
                           dp_outs_towns$date%in%date_count$date, ]

out_space_season<-dp_outs_towns[complete.cases(dp_outs_towns), ]%>%
  mutate(date=as.Date(date),
         month=month(date))

  

df<-group_by(out_space_week,date)%>%
  summarise(TUVU = cor(TUVU, saidi,method = "spearman"),
            OSPR = cor(OSPR, saidi,method = "spearman"),
            DOWO = cor(DOWO, saidi,method = "spearman"),
            HOSP = cor(HOSP, saidi,method = "spearman"),
            date=as.Date(date))%>%
  gather(cor, value,2:5)

df_season<-group_by(out_space_season,year,season)%>%
  summarise(TUVU = cor(TUVU, saidi,method = "spearman"),
            OSPR = cor(OSPR, saidi,method = "spearman"),
            DOWO = cor(DOWO, saidi,method = "spearman"),
            HOSP = cor(HOSP, saidi,method = "spearman"),
            month=max(month))%>%
  mutate(date_ref=my(paste0(month,"-",year)),
         date=paste0(year," ",toupper(season)))%>%
  gather(cor, value,3:6)%>%
  arrange(date_ref)

ggplot(data=df_season,aes(x=date, y=value,group=cor)) +
  geom_line(aes(color=cor))+
  theme_classic(base_size = 12)+
  #scale_x_date(date_breaks="6 month", minor_breaks=NULL, date_labels="%b %Y")+
  scale_color_viridis_d(option = "turbo",begin=0.1)+
  theme(axis.text.x = element_text(angle = 45,vjust = 1,hjust = 1))+
  labs(y="Spearman's r",
       x="Date",
       color="Bird Species")

ggsave("../Outputs/out_bird_spatialcorr_seasonal_ts.jpeg",
       width=11,
       height = 7,
       dpi = 300,
       units = "in")



#Map outages and bird abundance
#Do one map with all bird abundance and compare to individual species abundance maps

#The town polygons will represent outages and centroid shapes will represent bird abundance

#individual spp
df2<-dp_outs_towns%>%
  group_by(actual_city_town)%>%
  summarise(DOWO = mean(DOWO,na.rm=T),
            OSPR = mean(OSPR,na.rm=T),
            PIWO = mean(PIWO,na.rm=T),
            NOFL = mean(NOFL,na.rm=T),
            SAIDI=mean(saidi,na.rm = T))%>%
  ungroup()

towns_birds2<-left_join(ma_towns,df2,by=c("city"="actual_city_town"))

#merge to town centroids
#change bird abundance to a categorical variable for plotting
towns_birds_nogeom2<-st_drop_geometry(towns_birds2)

cent_var2<-right_join(cent,towns_birds_nogeom2,by="city")%>%
  mutate(SAIDI_DOWO=case_when(
            SAIDI>quantile(dp_outs_towns$saidi,0.9,na.rm = T)&
              DOWO>quantile(dp_outs_towns$DOWO,0.9,na.rm = T)~ "Extreme SAIDI and Bird DP",
            SAIDI>quantile(dp_outs_towns$saidi,0.9,na.rm = T)&
              DOWO<quantile(dp_outs_towns$DOWO,0.9,na.rm = T)~ "Extreme SAIDI"),
         SAIDI_OSPR=case_when(
           SAIDI>quantile(dp_outs_towns$saidi,0.9,na.rm = T)&
             OSPR>quantile(dp_outs_towns$OSPR,0.9,na.rm = T)~ "Extreme SAIDI and Bird DP",
           SAIDI>quantile(dp_outs_towns$saidi,0.9,na.rm = T)&
             OSPR<quantile(dp_outs_towns$OSPR,0.9,na.rm = T)~ "Extreme SAIDI"),
         SAIDI_PIWO=case_when(
           SAIDI>quantile(dp_outs_towns$saidi,0.9,na.rm = T)&
             PIWO>quantile(dp_outs_towns$PIWO,0.9,na.rm = T)~ "Extreme SAIDI and Bird DP",
           SAIDI>quantile(dp_outs_towns$saidi,0.9,na.rm = T)&
             PIWO<quantile(dp_outs_towns$PIWO,0.9,na.rm = T)~ "Extreme SAIDI"),
         SAIDI_NOFL=case_when(
           SAIDI>quantile(dp_outs_towns$saidi,0.9,na.rm = T)&
             NOFL>quantile(dp_outs_towns$NOFL,0.9,na.rm = T)~ "Extreme SAIDI and Bird DP",
           SAIDI>quantile(dp_outs_towns$saidi,0.9,na.rm = T)&
             NOFL<quantile(dp_outs_towns$NOFL,0.9,na.rm = T)~ "Extreme SAIDI"))
cent_var2<-cent_var2%>%filter(city!="HULL")

#map

m1<-ggplot() +
  #specify Bird Species
  geom_sf(data = towns_birds2, aes(fill = OSPR)) +
  scale_fill_viridis_c(option = "plasma", alpha = 0.8,
                       guide = guide_legend(title.position = "top",
                                            label.position = "bottom",
                                            title="OSPR"))+
  #outage measure
  geom_sf(data=cent_var2[!is.na(cent_var2$SAIDI_OSPR),],aes(color = SAIDI_OSPR))+
  scale_shape_manual(values=19)+
  scale_color_manual(values = c("white","red"),
                     guide = guide_legend(title = element_blank(),
                                          nrow = 2))+
  theme_classic(base_size = 12)+
  theme(legend.position = "top",
        legend.key = element_rect(fill = "lightgrey", color = NA),
        strip.background = element_rect(color=NA),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank())

m2<-ggplot() +
  #specify Bird Species
  geom_sf(data = towns_birds2, aes(fill = DOWO)) +
  scale_fill_viridis_c(option = "plasma",alpha = 0.8,
                       guide = guide_legend(title.position = "top",
                                            label.position = "bottom",
                                            title="DOWO"))+
  #outage measure
  geom_sf(data=cent_var2[!is.na(cent_var2$SAIDI_DOWO),],aes(color = SAIDI_DOWO))+
  scale_shape_manual(values=19)+
  scale_color_manual(values = c("white","red"),
                     guide = guide_legend(title = element_blank(),
                                          nrow = 2))+
  theme_classic(base_size = 12)+
  theme(legend.position = "top",
        legend.key = element_rect(fill = "lightgrey", color = NA),
        strip.background = element_rect(color=NA),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank())

m3<-ggplot() +
  #specify Bird Species
  geom_sf(data = towns_birds2, aes(fill = PIWO)) +
  scale_fill_viridis_c(option = "plasma",alpha = 0.8,
                       guide = guide_legend(title.position = "top",
                                            label.position = "bottom",
                                            title="PIWO"))+
  #outage measure
  geom_sf(data=cent_var2[!is.na(cent_var2$SAIDI_PIWO),],aes(color = SAIDI_PIWO))+
  scale_shape_manual(values=19)+
  scale_color_manual(values = c("white","red"),
                     guide = guide_legend(title = element_blank(),
                                          nrow = 2))+
  theme_classic(base_size = 12)+
  theme(legend.position = "top",
        legend.key = element_rect(fill = "lightgrey", color = NA),
        strip.background = element_rect(color=NA),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank())

m4<-ggplot() +
  #specify Bird Species
  geom_sf(data = towns_birds2, aes(fill = NOFL)) +
  scale_fill_viridis_c(option = "plasma",alpha = 0.8,
                       guide = guide_legend(title.position = "top",
                                            label.position = "bottom",
                                            title="NOFL"))+
  #outage measure
  geom_sf(data=cent_var2[!is.na(cent_var2$SAIDI_NOFL),],aes(color = SAIDI_NOFL))+
  scale_shape_manual(values=19)+
  scale_color_manual(values = c("white","red"),
                     guide = guide_legend(title = element_blank(),
                                          nrow = 2))+
  theme_classic(base_size = 12)+
  theme(legend.position = "top",
        legend.key = element_rect(fill = "lightgrey", color = NA),
        strip.background = element_rect(color=NA),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank())

ggarrange(m1,m2,m3,m4,ncol = 2,nrow=2,legend = "bottom")

ggsave("Scratch/Outputs/Figures/species_extremeoutages_alltime_townmap.jpeg",
       width=8,
       height = 9,
       dpi = 300,
       units = "in")




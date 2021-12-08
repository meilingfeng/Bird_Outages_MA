library(tidyverse)
library(lubridate)
library(stringr)
library(DescTools)
library(sf)


out_abund_var<-read.csv("Old Outputs/out_abund_var.csv")%>%
  mutate(Date=ymd(date))

out_abund_space_var<-read.csv("Old Outputs/out_abund_space_var.csv")%>%
  mutate(Date=ymd(date))

out_abund_time_var<-read.csv("Old Outputs/out_abund_time_var.csv")%>%
  mutate(Date=ymd(date))

#MA towns
crs <- st_crs(4326)
towns<-st_read("../Raw_Data/MA_Shapefiles/ma_towns_namescorrected.shp")%>% 
  dplyr::select(city,TOWN_ID,Area_km2,county)%>%
  st_transform(crs = crs)%>%
  mutate(TOWN_ID=as.integer(TOWN_ID))

hh<-read.csv("../Raw_Data/MA_SAIDI.csv")

#Lag outages by5 week intervals

change_all_5lag <- out_abund_space_var%>%
  group_by(city)%>%
  arrange(date)%>%
  mutate_at(c("st_COH_avg","coh_PCT","countpercap","count_PCT"),lag, n=5)
change_all_10lag <- out_abund_space_var%>%
  group_by(city)%>%
  arrange(date)%>%
  mutate_at(c("st_COH_avg","coh_PCT","countpercap","count_PCT"),lag, n=10)
change_all_15lag <- out_abund_space_var%>%
  group_by(city)%>%
  arrange(date)%>%
  mutate_at(c("st_COH_avg","coh_PCT","countpercap","count_PCT"),lag, n=15)
change_all_20lag <- change_all%>%
  group_by(city)%>%
  arrange(date)%>%
  mutate_at(c("st_COH_avg","coh_PCT","countpercap","count_PCT"),lag, n=20)
change_all_25lag <- change_all%>%
  group_by(city)%>%
  arrange(date)%>%
  mutate_at(c("st_COH_avg","coh_PCT","countpercap","count_PCT"),lag, n=25)
change_all_30lag <- change_all%>%
  group_by(city)%>%
  arrange(date)%>%
  mutate_at(c("st_COH_avg","coh_PCT","countpercap","count_PCT"),lag, n=30)
change_all_35lag <- change_all%>%
  group_by(city)%>%
  arrange(date)%>%
  mutate_at(c("st_COH_avg","coh_PCT","countpercap","count_PCT"),lag, n=35)
change_all_40lag <- change_all%>%
  group_by(city)%>%
  arrange(date)%>%
  mutate_at(c("st_COH_avg","coh_PCT","countpercap","count_PCT"),lag, n=40)


#can classify data into seasons and quartiles. Split data into separate datasets for each and scatterplot.
#quartiles
#st_avg_coh vs change in ER 
#(urban sp, pileated/flicker,redtailed and vulture neg in 4th, hairy/downy/redbellied/cowbird positive in 4th)
#for raw ER: star,spar,pige,vult,osprey,bbirds pos in 4th, hairy,downy,pileated,flicker neg in 4th
par(mar=c(5,6,4,1)+.1)
ggplot(filter(change_all_5lag,coh_PCT>0),
           aes(x=redbellied, y=st_COH_avg)) + geom_point(color='blue',alpha =1/10)+
  geom_smooth(method='lm',color='red')+
  labs(x = "Encounter Rate", y = "Standardized Average COH")+
  facet_wrap(~coh_PCT)


#seasons
#st_avg_coh vs change in ER 
#(summer/fall positive for redwinged)
#raw ER: summer/fall positive for star/spar/pige/flic
par(mar=c(5,6,4,1)+.1)
ggplot(out_abund_space_var,
       aes(x=ospr, y=coh_space_anom)) + geom_point(color='blue',alpha =1/10)+
  geom_smooth(method='lm',color='red')+
  labs(x = "Encounter Rate", y = "Standardized Average COH")+
  facet_wrap(~season)
#weighted st_avg coh vs change in ER 
#(vulture more positive in summer/fall,cowbird more positive in spring hairy/downy slightly more in fall/spring, redtailed slightly more in winter)
#raw ER:redtail more pos in summer/fall


#Conclusions, outage frequency and weighted variables are less correlated than smoothed avg COH. Use st_avg_coh moving forward.
#COH is more correlated with urban, bbird, vulture and osprey ER; more correlated with changes in woodpecker ER

#Re run analyses separating by town quartiles of outages
town_quant<-out_abund_space_var%>%
  group_by(city,season)%>%
  summarize(COH=mean(coh_space_anom,na.rm=TRUE))
town_quant_75<-town_quant%>%
  group_by(season)%>%
  mutate( COHq=CutQ(COH))

change_quants<-left_join(out_abund_space_var,town_quant_75, by=c("city","season"))

#correaltions between ER and stavgCOH separated by quartiles of the town total seasonal coh
par(mar=c(5,6,4,1)+.1)
ggplot(filter(change_quants),
       aes(x=ospr, y=coh_space_anom)) + geom_point(color='blue',alpha =1/10)+
  geom_smooth(method='lm',color='red')+
  labs(x = "Encounter Rate", y = "Smoothed Average COH")+
  facet_wrap(~COHq)

#Check correlations in the lag of change
#Do a lag length of 2 (weeks), ~25 lags will equal a year's lag.
#lag the ER data

#separate town quartiles
town_quant<-out_abund_space_var%>%
  group_by(city,season)%>%
  summarize(COH=sum(coh_space_anom,na.rm=TRUE),
            rbwo=mean(rbwo,na.rm = TRUE),
            hosp=mean(hosp,na.rm = TRUE),
            ospr=mean(ospr,na.rm = TRUE),
            rtha=mean(rtha,na.rm = TRUE))
town_quant_75<-town_quant%>%
  group_by(season)%>%
  mutate( COHq=CutQ(COH),
          rbwoq=CutQ(rbwo),
          hospq=CutQ(hosp),
          osprq=CutQ(ospr),
          rthaq=CutQ(rtha))%>%
  filter(COHq=="Q4"&osprq=="Q1")

sub_towns<-out_abund_space_var%>%filter(city%in% town_quant_75$city)
out<-sub_towns %>%
  group_by(city)%>%
  arrange(date, by_group=TRUE)%>%
  filter(!is.na(coh_space_anom))%>%
  summarise((ccfout=list(ccf(ospr,coh_space_anom,50))))
out[1,2]
#lag the pcs
#separate town quartiles
#fall ,pc 4 with countpercap
#spring/summer/winter, pc2 wiht count, pc3 for change
town_quant<-read.csv("C:/Users/emfeng/OneDrive - Lincoln Park Zoo/MA Power Data/outage_species_spatial_join/PCA/all space time/ER change/ER_change_zscores_outages_weeks.csv")%>%
  filter(!(year<2013))

town_quant_75<-town_quant%>%
  group_by(season)%>%
  mutate(downyq=CutQ(downy),
         hairyq=CutQ(hairy),
         redbelliedq=CutQ(redbellied),
         pileatedq=CutQ(pileated))%>%
  filter(countpercap_PCT>=4&downyq=="Q4")

wood_towns<-change_all%>%filter(city%in% town_quant_75$city)
out<-town_quant_75 %>%
  group_by(city)%>%
  arrange(date, by_group=TRUE)%>%
  summarise((ccfout=list(ccf(downy,st_COH_avg,50))))


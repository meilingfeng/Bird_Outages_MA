library(tidyverse)
library(lubridate)
library(stringr)
library(DescTools)
library(sf)


#MA towns
crs <- st_crs(4326)
towns<- st_read("../Raw_Data/MA_Shapefiles/MA_Towns_Shape_Final.shp")%>% 
  select(city,cty_twn,POP2010,Area_km2)%>%
  st_transform(crs = crs)


#times and towns with outage events
change<-read.csv('Old Outputs/weekly_COH_avg_st_wt_join_animal_abundance.csv')%>%
  mutate(date=mdy(`date_time_out`),
         city=actual_city_town)%>%
  select(-c("actual_city_town","date_time_out","blackbirds","raptors","woodpeckers","urban"))


#all ER through time (zero fill missing dates for outages)
ERchange_all<-read.csv('Old Outputs/allspecies_ER.csv')%>%
  mutate(date=mdy(`date`))%>%
  filter(year>=2013)
# Merge the two datasets: the full dates and outage events
change_all <- merge(ERchange_all, dplyr::select(change, c("count","st_COH_avg","date","city")), by=c("date","city"), all=T)%>%
  mutate(month=month(date),
         season=ifelse(month%in% c(12,1:5), ifelse(month%in% c(12,1,2),"Winter", "Spring"),
                       ifelse(month%in% c(6:8), "Summer", "Fall")))%>%
  select(-c("month"))

change_all<-as.data.frame(merge(change_all,towns[,-5],by="city", all.x = TRUE))%>%
  mutate(coh_PCT=ntile(st_COH_avg,4),
         countpercap=count/POP2010,
         count_PCT=ntile(countpercap, 4))
change_all[is.na(change_all)] <- 0

#Lag outages by5 week intervals

change_all_5lag <- change_all%>%
  group_by(city)%>%
  arrange(date)%>%
  mutate_at(c("st_COH_avg","coh_PCT","countpercap","count_PCT"),lag, n=5)
change_all_10lag <- change_all%>%
  group_by(city)%>%
  arrange(date)%>%
  mutate_at(c("st_COH_avg","coh_PCT","countpercap","count_PCT"),lag, n=10)
change_all_15lag <- change_all%>%
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
ggplot(change_all,
       aes(x=osprey, y=st_COH_avg)) + geom_point(color='blue',alpha =1/10)+
  geom_smooth(method='lm',color='red')+
  labs(x = "Encounter Rate", y = "Standardized Average COH")+
  facet_wrap(~season)
#weighted st_avg coh vs change in ER 
#(vulture more positive in summer/fall,cowbird more positive in spring hairy/downy slightly more in fall/spring, redtailed slightly more in winter)
#raw ER:redtail more pos in summer/fall


#Conclusions, outage frequency and weighted variables are less correlated than smoothed avg COH. Use st_avg_coh moving forward.
#COH is more correlated with urban, bbird, vulture and osprey ER; more correlated with changes in woodpecker ER

#Re run analyses separating by town quartiles of outages
town_quant<-change_all%>%
  group_by(city,season)%>%
  summarize(COH=sum(st_COH_avg,na.rm=TRUE))
town_quant_75<-town_quant%>%
  group_by(season)%>%
  mutate( COHq=CutQ(COH))

change_quants<-left_join(change_all,town_quant_75, by=c("city","season"))

#correaltions between ER and stavgCOH separated by quartiles of the town total seasonal coh
par(mar=c(5,6,4,1)+.1)
ggplot(filter(change_quants),
       aes(x=starling, y=st_COH_avg)) + geom_point(color='blue',alpha =1/10)+
  geom_smooth(method='lm',color='red')+
  labs(x = "Encounter Rate", y = "Smoothed Average COH")+
  facet_wrap(~COHq)

#Check correlations in the lag of change
#Do a lag length of 2 (weeks), ~25 lags will equal a year's lag.
#lag the ER data

#separate town quartiles
town_quant<-change_all%>%
  group_by(city,season)%>%
  summarize(COH=sum(st_COH_avg,na.rm=TRUE),
            redbellied=mean(redbellied,na.rm = TRUE),
            downy=mean(downy,na.rm = TRUE),
            hairy=mean(hairy,na.rm = TRUE),
            pileated=mean(pileated,na.rm = TRUE))
town_quant_75<-town_quant%>%
  group_by(season)%>%
  mutate( COHq=CutQ(COH),
          redbelliedq=CutQ(redbellied),
          downyq=CutQ(downy),
          hairyq=CutQ(hairy),
          pileatedq=CutQ(pileated))%>%
  filter(COHq=="Q4"&pileatedq=="Q4")

wood_towns<-change_all%>%filter(city%in% town_quant_75$city)
out<-wood_towns %>%select(-osprey)%>%
  group_by(city)%>%
  arrange(date, by_group=TRUE)%>%
  summarise((ccfout=list(ccf(pileated,st_COH_avg,50))))

#lag the pcs
#separate town quartiles
#fall ,pc 4 with countpercap
#spring/summer/winter, pc2 wiht count, pc3 for change
town_quant<-read.csv("Old Outputs/Species_PCA_old/ER_change_zscores_outages_weeks.csv")%>%
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


library(lubridate)
library(sf)
library(tidyverse)


load("Objects and Data/3_DP_predictions.rda")
load("Objects and Data/0_MA_town_shapefiles.rda")
ma_towns<-ma_towns[!duplicated(ma_towns$city),]
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
  dplyr::select(-Date.Out)%>%
  
  #b) Calculate SAIDI, Customer Outage Minutes per incident
  #NCOH = (# customers affected * outage duration)/ total customers served (households)
  mutate(saidi=(Original.Number.Customers.Affected*Actual.Duration*60)/hh_total,
         
  #c) Flag records with more customers impacted than total households
         hh_cust_flag=ifelse(hh_total<Original.Number.Customers.Affected,1,0))%>%
  #remove flagged records
    filter(hh_cust_flag!=1)%>%
  dplyr::select(-hh_cust_flag)%>%
  group_by(actual_city_town,week,year)%>%
  
  #d) aggregate SAIDI per town/week
  summarize(saidi=sum(saidi,na.rm=T),
            log_saidi=log(saidi))%>%
  ungroup()
  
#3. Remove any outliers of log(SAIDI)
  
  #outliers defined as outside 1.5(iqr)
  Q <- quantile(outs2$log_saidi, probs=c(.25, .75), na.rm = FALSE)
  iqr<-IQR(outs2$log_saidi)
  
  outs3<- subset(outs2, outs2$log_saidi > (Q[1] - 1.5*iqr) & outs2$log_saidi < (Q[2]+1.5*iqr))  
  
  #how many records are removed as outliers
  length(outs2$saidi)-length(outs3$saidi)
  #check distribution of SAIDI after log(SAIDI) outliers are removed
  hist(outs2$saidi)

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
  dp_towns2<-dplyr::select(dp_towns[[1]],-eBird.DP.RF)
    #Grab all the species detection probabilities
  dp_towns<-lapply(dp_towns,function(x){dplyr::select(x,"eBird.DP.RF")})
    #column bind them
  dp_towns<-bind_cols(dp_towns)
    #Name them as their species codes
  colnames(dp_towns)<-colnames
    #column bind species DPs with time-town variables
  dp_towns3<-bind_cols(dp_towns2,dp_towns)%>%
    filter(Year>=2013)



#Merge bird and outage data into one dataset
#------------------------------------------------------------------------------------------
dp_outs_towns<-left_join(outs3,dp_towns3, by=c("actual_city_town"="city",
                                               "year"="Year",
                                               "week"="week"))%>%
  dplyr::select(-c("elevation_median","elevation_sd"))


#write final dataset to files
write.csv(dp_outs_towns,"Outputs/dp_out_towns.csv",row.names = F)

save(dp_outs_towns, file = "Objects and Data/4_dp_outs_towns.rda")
save(dp_towns3, file = "Objects and Data/4_dp_towns.rda")


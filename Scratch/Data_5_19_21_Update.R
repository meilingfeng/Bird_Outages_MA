library(plyr)
library(tidyverse)
library(lubridate)
library(sf)
library(psych)
library(lme4)


#MA town shapefile (from Mass.gov)
  #set coordinate reference system
  crs <- st_crs(4326)
  towns<-st_read("../Raw_Data/MA_Shapefiles/ma_towns_namescorrected.shp")%>% 
    dplyr::select(city,TOWN_ID,Area_km2,county)%>%
    st_transform(crs = crs)%>%
    mutate(TOWN_ID=as.integer(TOWN_ID))
  towns_nogeom<- towns%>%
    st_drop_geometry()
  
  cent<- st_read("../Raw_Data/MA_Shapefiles/town_cent_namescorrected.shp")%>% 
    dplyr::select(city,TOWN_ID)%>%
    st_transform(crs = crs)
  


#---------------------------------------------------------------------------------  
#MA bird abundance data. "Encounter rates" are our proxies for relative abundance.
#---------------------------------------------------------------------------------  
  #15 study species
  name_list <- list(
    sp_name = c("Red-winged Blackbird","Common Grackle","Brown-headed Cowbird","Red-tailed Hawk","Osprey", "Red-bellied Woodpecker",
                "Northern Flicker","Hairy Woodpecker","Downy Woodpecker",
                "American Crow","Rock Pigeon","House Sparrow","European Starling","Turkey Vulture","Pileated Woodpecker"),
    #Four-letter species codes
    sp_alpha = c("rwbl","cogr","bhco","rtha","ospr","rbwo","nofl","hawo","dowo",
                "amcr","ropi","hosp","eust","tuvu","piwo"),
    #Scientific names
    sp_sci = c("Agelaius phoeniceus","Quiscalus quiscula","Molothrus ater", "Buteo jamaicensis","Pandion haliaetus", "Melanerpes carolinus",
               "Colaptes auratus","Dryobates villosus","Dryobates pubescens", "Corvus brachyrhynchos", "Columba livia","Passer domesticus", "Sturnus vulgaris",
               "Cathartes aura", "Dryocopus pileatus"))

  

       # Read in all the bird abundance files ####
    bird_files <- pmap(name_list, ~ paste0("Old Outputs/Species_DP_Old/",..2,"_weekly_ER_05_18.csv"))
    
    bird_dat <- map(bird_files, read_csv, 
                    col_types = cols(X = col_skip(),
                                     observation_date = col_date(format = "%m/%d/%Y")))
    names(bird_dat) <- name_list$sp_alpha
    
    # Calculate the max ER for the previous 4 weeks (week 0, -1, -2 ,-3) to create a "monthly ER"####
    # reduces weekly noise (likely without biological meaning)
    bird_dat <- map(bird_dat, ~ .x %>% 
                      filter(!is.na(year)&year>=2010)%>%
                      mutate(date=observation_date,
                             month=month(date),
                             week=week(date))%>%
                      select(-c("observation_date","day_of_year"))%>%
                      group_by(city) %>% 
                      mutate(lag_01 = lag(avg_encount_rate, n = 1),
                             lag_02 = lag(avg_encount_rate, n = 2),
                             lag_03 = lag(avg_encount_rate, n = 3),
                             max_er_4wk = pmax(avg_encount_rate, lag_01, lag_02, lag_03)) %>% 
                      select(-starts_with("lag")) %>% 
                      ungroup()%>%
                      filter(year>=2013)%>%
     #Calculate ER anomalies to remove seasonal trends and show relativity across: 
     #time (months from year to year) and space (towns)
                      group_by(city,month)%>%
                      #Deviation of each month's ER from the average ER for that month across years, divided by the stdev of the month across years
                      #How atypical is each month in a town compared to other years for the town?
                      mutate(anomaly= max_er_4wk-mean(max_er_4wk,na.rm=TRUE),
                             monthly_anomaly= anomaly/sd(max_er_4wk,na.rm=TRUE))%>%
                      select(-anomaly)%>%
                      ungroup()%>%
                      group_by(month,year)%>%
                      #Deviation of each town's ER from the average ER across the state for that date, divided by the stdev of the month across years
                      #How atypical is each town compared to other towns at a particular month in time?
                      mutate(anomaly= max_er_4wk-mean(max_er_4wk,na.rm=TRUE),
                             town_anomaly= anomaly/sd(max_er_4wk,na.rm=TRUE))%>%
                      select(-anomaly)%>%
                      ungroup())
    
    #Arrange data into 4 week max, temporal anomaly, and spatial anomaly bird ER datasets
    names<-read.csv("../Raw_Data/MA_town_county_names.csv")%>%select(-c("town_ID","POP2010"))
    abundance <- bind_rows(bird_dat, .id = "sp") %>% 
         select(-c("avg_encount_rate","monthly_anomaly","town_anomaly")) %>% 
         pivot_wider(names_from = sp,
                     values_from = max_er_4wk)%>%
      mutate(week=week(date))%>%
      #change city names to match outage data
      left_join(names,by=c("city"="city.bird"),all.x=T)%>%
      select(-c("city","TOWN_ID"))%>%
      rename(city=city.outage)%>%
      unique()
    

  

  
  #create a "season" field based on eBird status and trends breeding seasons of our study species 
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
             fall==1 ~ "fall"))
  }
  abund_var<-season(abundance)

 
  
  #write datasets to file
  #raw outage and encounter rate measures
  #write.csv(abund_var, "Old Outputs/weekly_bird_abundance_MA.csv",row.names = F)
  
  
  
  
#-----------------------------  
#Data exploration figures 
#-----------------------------
  #raw outage and encounter rate measures
out_abund_var<-read.csv("Old Outputs/out_abund_var.csv")%>%
    mutate(Date=ymd(date))
logit<-out_abund_var%>%
  mutate_at(c(7:21),function(x){log(x/(1-x))})

  #spatial (town) anomalies for outages and encounter rates at each weekly time step
out_abund_space_var<-read.csv("out_abund_space_var.csv")%>%
    mutate(Date=ymd(date))
  
  #temporal (monthly for ERs and weekly for outages) anomalies for outage and encounter rates for each town
out_abund_time_var<-read.csv("out_abund_time_var.csv")%>%
    mutate(Date=ymd(date))
  
#Look at time series of outage frequency and bird abundance statewide
  #Outage frequency peaks around June and November, supporting that animal outage activity increases in these months
  #Northern flicker, osprey, turkey vulture, common grackle, and brown-headed cowbird seasonal trends 
  #most align with outage frequency trends
timeseries<- out_abund_time_var%>%
    group_by(Date)%>%
    summarise(RBWO = max(rbwo,na.rm = TRUE),#specify bird species
              OSPR = max(ospr,na.rm = TRUE),
              BHCO = max(bhco,na.rm = TRUE),
              HOSP = max(hosp,na.rm = TRUE),
              RTHA = max(rtha,na.rm = TRUE),
              SAIDI=mean(saidi_time_anom,na.rm = TRUE),
              Number.Outages = sum(out_time_anom,na.rm = TRUE))%>%
    gather(measure, value, RBWO:Number.Outages)%>%     ## stack metrics into 1 column for plotting
  mutate(value=replace_na(value,0))

  ggplot(data = timeseries, aes(x=Date, y=SMA(value,n=3),group=1)) +
    geom_line() +
    scale_x_date(date_breaks="6 month", minor_breaks=NULL, date_labels="%b %Y")+
    xlab("") +
    ylab("State Average") + 
    facet_grid(measure ~ ., scales='free')
  
  ggsave("state_avg_timeseries_woodpecker_timeanom.jpeg",
         width=12.5,
         height = 9,
         dpi = 96,
         units = "in")
  
  #By individual towns
  timeseries<- out_abund_time_var%>%
    filter(city=="LOWELL")%>%
    transmute(OSPR = ospr,#specify bird species
              HOSP = hosp,
              BHCO = bhco,
              RBWO = rbwo,
              RTHA = rtha,
              Number.Outages = saidi_time_anom,
              Date=Date)%>%
    gather(measure, value, OSPR:Number.Outages)%>%     ## stack metrics into 1 column for plotting
    mutate(value=replace_na(value,0))
  
  ggplot(data = timeseries, aes(x=Date, y=value,group=1)) +
    geom_line() +
    scale_x_date(date_breaks="6 month", minor_breaks=NULL, date_labels="%b %Y")+
    xlab("") +
    ylab("State Average") + 
    facet_grid(measure ~ ., scales='free')

m<-glmer(num_outs~rbwo+ospr+bhco+hosp+rtha+(1|city),data=logit[complete.cases(logit),],family="poisson")
  summary(m)
  

timeseries<- out_abund_var%>%  
  group_by(Date)%>%
    summarise(RBWO = max(rbwo,na.rm = TRUE),#specify bird species
              OSPR = max(ospr,na.rm = TRUE),
              BHCO = max(bhco,na.rm = TRUE),
              HOSP = max(hosp,na.rm = TRUE),
              RTHA = max(rtha,na.rm = TRUE),
              DOWO = max(dowo,na.rm = TRUE),
              COGR = max(cogr,na.rm = TRUE),
              EUST = max(eust,na.rm = TRUE),
              AMCR = max(amcr,na.rm = TRUE),
              SAIDI=mean(saidi,na.rm = TRUE),
              Number.Outages = sum(num_outs,na.rm = TRUE))%>%
    mutate(SAIDI=replace_na(SAIDI,0))


rbwo_ts<- decompose(ts(timeseries$RBWO,frequency=52,start=c(2013,1)))
ospr_ts<- plot(decompose(ts(timeseries$OSPR,frequency=52,start=c(2013,1))))
bhco_ts<- plot(decompose(ts(timeseries$BHCO,frequency=52,start=c(2013,1))))
rtha_ts<- plot(decompose(ts(timeseries$RTHA,frequency=52,start=c(2013,1))))
hosp_ts<- plot(decompose(ts(timeseries$HOSP,frequency=52,start=c(2013,1))))
dowo_ts<- plot(decompose(ts(timeseries$DOWO,frequency=52,start=c(2013,1))))
cogr_ts<- plot(decompose(ts(timeseries$COGR,frequency=52,start=c(2013,1))))
eust_ts<- plot(decompose(ts(timeseries$EUST,frequency=52,start=c(2013,1))))
amcr_ts<- plot(decompose(ts(timeseries$AMCR,frequency=52,start=c(2013,1))))
saidi_ts<- plot(decompose(ts(timeseries$SAIDI,frequency=52,start=c(2013,1))))
numouts_ts<- plot(decompose(ts(timeseries$Number.Outages,frequency=52,start=c(2013,1))))
plot(ospr_ts$trend)

  ggplot(data = timeseries, aes(x=Date, y=SMA(value,n=3),group=1)) +
    geom_line() +
    scale_x_date(date_breaks="6 month", minor_breaks=NULL, date_labels="%b %Y")+
    xlab("") +
    ylab("State Average") + 
    facet_grid(measure ~ ., scales='free')
  
  ggsave("state_avg_timeseries_woodpecker_timeanom.jpeg",
         width=12.5,
         height = 9,
         dpi = 96,
         units = "in")
  
  
  
  
  
  #Temporal correlations between temporal anomalies in towns with high vs low bird abundance
  
  #Classify towns in the 90th and 10th percentile of mean seasonal bird abundance
  town_perc<-out_abund_var%>%
    select(city,season,rwbl:piwo)%>%
    #take the mean seasonal bird ERs for each town
    group_by(city,season)%>%
    summarize_all(mean,na.rm = T)
  #rank towns by percentiles of average ER for each season
  town_perc<-town_perc%>% 
    group_by(season)%>%
    mutate(across(rwbl:piwo, function(x) {as.integer(cut(x, quantile(x, probs=0:15/15), include.lowest=TRUE))}))
  
  percents<-left_join(out_abund_time_var,town_perc, by=c("city","season"))
  
  ggplot(filter(percents,eust.y%in%c(1,15)&season=="fall"),
         aes(x=eust.x, y=out_time_anom)) + geom_point(color='black',alpha =1/10)+
    geom_smooth(method='lm',color='red')+
    labs(x = "Encounter Rate", y = "Outage Frequency")+
    facet_wrap(~eust.y)
  
  ggsave("out_nofl_timecorr_summer.jpeg",
         width=14.2,
         height = 6.9,
         dpi = 300,
         units = "in")
  
  
  
  #Spatial correlations across town spatial anomalies through time
  out_space<-out_abund_space_var%>%
    filter(!is.na(out_space_anom))
  
  cor1 <- function(df)
  {
    return(data.frame(RBWO = cor(df$rbwo, df$out_space_anom,method = "spearman")))
  }
  cor2 <- function(df)
  {
    return(data.frame(DOWO = cor(df$dowo, df$out_space_anom,method = "spearman")))
  }
  cor3 <- function(df)
  {
    return(data.frame(HAWO = cor(df$hawo, df$out_space_anom,method = "spearman")))
  }
  cor4 <- function(df)
  {
    return(data.frame(PIWO = cor(df$piwo, df$out_space_anom,method = "spearman")))
  }
  seasons_1_count<-ddply(out_space, .(Date), cor1)
  seasons_2_count<-ddply(out_space, .(Date), cor2)
  seasons_3_count<-ddply(out_space, .(Date), cor3)
  seasons_4_count<-ddply(out_space, .(Date), cor4)
  seasons_cors<-merge(seasons_1_count,seasons_2_count,by="Date")%>%
    merge(seasons_3_count,by="Date")%>%merge(seasons_4_count,by="Date")%>%
    gather(cor, value,2:5)
  
  ggplot(data=seasons_cors,aes(x=Date, y=value,group=cor)) +
    geom_line(aes(color=cor))+
    labs(y="Spearman's Cor. Coeff.",
         x="Date",
         title="Outages-Species Correlation Through Time",
         color="Correlation")
  
  ggsave("out_woodpecker_spatialcorr_throughtime.jpeg",
         width=14.2,
         height = 6.9,
         dpi = 300,
         units = "in")
  
  
  
#Map outages and bird abundance
  
  #The town polygons will represent outages and centroid shapes will represent bird abundance
  
  #merge to town shapefile
  towns_var<-left_join(towns,out_abund_var,by="city")
  
  #merge to town centroids
  #change bird abundance to a categorical variable for plotting
  cent_var<-left_join(cent,out_abund_var,by="city")%>%
    mutate(tuvu=ifelse(tuvu>-1&tuvu<1, "Average", ifelse(tuvu<=-1, "Below Average","Above Average")),
           rtha=ifelse(rtha>-1&rtha<1, "Average", ifelse(rtha<=-1, "Below Average","Above Average")),
           ospr=ifelse(ospr>-1&ospr<1, "Average", ifelse(ospr<=-1, "Below Average","Above Average")),
           eust=ifelse(eust>-1&eust<1, "Average", ifelse(eust<=-1, "Below Average","Above Average")),
           hosp=ifelse(hosp>-1&hosp<1, "Average", ifelse(hosp<=-1, "Below Average","Above Average")),
           ropi=ifelse(ropi>-1&ropi<1, "Average", ifelse(ropi<=-1, "Below Average","Above Average")),
           piwo=ifelse(piwo>-1&piwo<1, "Average", ifelse(piwo<=-1, "Below Average","Above Average")),
           rbwo=ifelse(rbwo>-1&rbwo<1, "Average", ifelse(rbwo<=-1, "Below Average","Above Average")))
  cent_var<-cent_var%>%filter(city!="HULL")
  
  #specify the season and year to map
  week<-25
  year<-2017
  
  #map
  ggplot() +
    #specify outage metric
    geom_sf(data = towns_var[towns_var$week==week&towns_var$year==year,], aes(fill = saidi)) +
    scale_fill_viridis_c(option = "plasma")+
    #specify bird species
    geom_sf(data=cent_var[cent_var$week==week&cent_var$year==year,],aes(color = ospr))+
    scale_shape_manual(values=19)+
    scale_color_manual(values=c("yellow","white","blue"))+
    labs(fill="Number of Animal-Outages",
         color="Abundance Anomalies",
         shape="Abundance Anomalies",
         title="MA Towns, Summer 2018")+
    theme(plot.title = element_text(size = 12, face = "bold"),
          legend.title=element_text(size=13, face="bold"), 
          legend.text=element_text(size=12))
  #save to map to file
  ggsave("Summer13_outs_ROPI_townmap.jpeg",
         width=14.2,
         height = 6.9,
         dpi = 500,
         units = "in")
  
  
  #outages modeled by birds
 hist(subset$saidi)
 subset<-out_abund_time_var%>%
   filter(!is.na(saidi_time_anom)&
            city=="BRIGHTON")%>%
   mutate_at(c(7:23), scale)
 
summary(lm(saidi_time_anom~ospr+rtha+hosp+bhco+nofl+dowo+hawo+
             tuvu+amcr+ropi,data = subset))

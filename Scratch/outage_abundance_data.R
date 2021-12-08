library(plyr)
library(tidyverse)
library(lubridate)
library(sf)
library(psych)

#set location of Data folder
  folder <- "C:/Users/emfeng/OneDrive - Lincoln Park Zoo/Papers/energy-ecology paper/Data"
  setwd(folder)

#MA town shapefile (from Mass.gov)
  #set coordinate reference system
  town<-crs <- st_crs(4326)
  towns_nogeom<- st_read("MA_Towns_Shape_Final.shp")%>% 
    dplyr::select(city,cty_twn,TOWN_ID,POP2010,Area_km2)%>%
    st_transform(crs = crs)%>%
    st_drop_geometry()%>%
    mutate(pop_dens=POP2010/Area_km2)
  towns<- st_read("MA_Towns_Shape_Final.shp")%>% 
    dplyr::select(city,cty_twn,TOWN_ID,POP2010,Area_km2)%>%
    st_transform(crs = crs)
  cent<- st_read("town_centroids.shp")%>% 
    dplyr::select(city,cty_twn,TOWN_ID,POP2010,Area_km2)%>%
    st_transform(crs = crs)
  
#Temperature data (from PRISM)  
  
  #location of Temp Data folder within the Data folder
  temp_folder<-"C:/Users/emfeng/OneDrive - Lincoln Park Zoo/Papers/energy-ecology paper/Data/Temp Data"
  setwd(temp_folder)
  
  #calculate minimum weekly temperatures for each town
  tbl <-
    list.files(pattern = "*.csv") %>% 
    map_df(~read_csv(., col_types = cols(.default = "c")))
  
  temp<-tbl%>%
    mutate(Date=mdy(Date),
           month=month(Date),
           week=week(Date),
           year=year(Date))%>%
    group_by(year,week,TOWN_ID)%>%
    summarise(tmin=min(tmin_F,na.rm = TRUE))
    
  
setwd(folder)
#----------------
#MA outage data
#----------------
  #outages
  outs<-read.csv("MA_SAIDI.csv")
  #cooling/warming degree days. Daily mean temp minus 65F.
  #Often used to normalize the effect of weather on energy consumption and reliability of the grid.
  dd<- read.csv("MA_disparity_data.csv")%>%  
    mutate(date=ymd(date_out),
           time=hms(time_out))
  
  #align town names with bird abundance data by joining to town names in the town shapefile ("cty_twn")
  outs$actual_city_town<-gsub("New Marlborough","New Marlboro",outs$actual_city_town)
  outs$actual_city_town<-gsub("East Bridgewater","E. Bridgewater",outs$actual_city_town)
  outs$actual_city_town<-gsub("Manchester-by-the-Sea","Manchester",outs$actual_city_town)
  outs$actual_city_town<-gsub("Mount Washington","Mt. Washington",outs$actual_city_town)
  outs<-merge(outs,towns_nogeom[,c("city","cty_twn")], by.x="actual_city_town",by.y="cty_twn", all.x=TRUE)
  
  #select outages with non-squirrel, animal related cause
  #These include animal, animal-other, and bird
  outs<- outs%>%
    filter((Reason.For.Outage %in% c("Animal","Bird","Animal - Other")))%>%
    filter(!is.na(Reason.For.Outage))%>%
    #add weekly time variables
    mutate(date=mdy(Date.Out),
           month=month(date),
           week=week(date),
           year=year(date),
           time=hms(Time.Out))%>%
    #add cooling/warming degree days
    merge(dd[,c("date","time","actual_city_town", "number_of_customers_affected","deg_cool","deg_heat")],
          by.x = c("date","time","actual_city_town","Original.Number.Customers.Affected"),
          by.y = c("date","time","actual_city_town","number_of_customers_affected"), 
          all.x = T)
  
  #summarize outages by week
  #We can calculate 2 different outage metrics, the number of outages and customer outage hours (coh)
  #**several recorded outages have 0's for number of customers affected or outage duration
  #**we could substitute in small values for 0? What impact does a coh of 0 have on the grid?
  #*previous studies have focused on the number of outages, since it's a more direct reflection of
  #*animal-equipment interactions. Check distribution of outage frequency (GLM?).
  
  outs<-outs%>%
    group_by(city,week,year)%>%
      #COH is calculated by multiplying customers affected by outage duration and dividing by the number of households per town
    summarise(coh=(sum(Original.Number.Customers.Affected,na.rm = T)*sum(Actual.Duration,na.rm = T))/mean(hh_total),
      #number of outages (outage frequency) is the total outages that occur in a town per week       
               num_outs=n(),
      #heating and cooling degree days are summarized as the highest daily degree days experienced in each week for each town
              deg_cool=max(deg_cool),
              deg_heat=max(deg_heat))

#---------------------------------------------------------------------------------  
#MA bird abundance data. "Encounter rates" are our proxies for relative abundance.
#---------------------------------------------------------------------------------  
  #Species
  name_list <- list(
    sp_name = c("Red-winged Blackbird","Common Grackle","Brown-headed Cowbird","Red-tailed Hawk","Osprey", "Red-bellied Woodpecker",
                "Northern Flicker","Hairy Woodpecker","Downy Woodpecker",
                "Mourning Dove","Rock Pigeon","House Sparrow","European Starling","Turkey Vulture","Pileated Woodpecker"),
    #Four-letter species codes
    sp_alpha = c("rwbl","cogr","bhco","rtha","ospr","rbwo","nofl","hawo","dowo",
                "modo","ropi","hosp","eust","tuvu","piwo"),
    #Scientific names
    sp_sci = c("Agelaius phoeniceus","Quiscalus quiscula","Molothrus ater", "Buteo jamaicensis","Pandion haliaetus", "Melanerpes carolinus",
               "Colaptes auratus","Dryobates villosus","Dryobates pubescens", "Zenaida macroura", "Columba livia","Passer domesticus", "Sturnus vulgaris",
               "Cathartes aura", "Dryocopus pileatus"))

  #Species grouped by clades
  group_list<-list(
    g_file=c("blackbird", "raptor","woodpecker","dove","urban"),
    g_name=c("Blackbird", "Raptor","Woodpecker","Dove","Urban"),
    g_clade=c("Icteridae", "Accipitriformes","Picidae","Columbiformes","Passeriformes"))


  
  #Calculate abundance change metric
  
    #Monthly standardized anomalies-
    #First average weekly ERs into monthly averages, reduces weekly noise (likely without biological meaning)
    #deviation of each month's ER from the average ER for that month across years, divided by the stdev of the month across years
    #How atypical is each month compared to other years. 
  anomaly<-function(x) {x%>%group_by(city,month,year)%>%
      summarise(avg_encount_rate=mean(avg_encount_rate,na.rm=TRUE))%>%
      ungroup()%>%
      group_by(city,month)%>%
        mutate(anomaly= avg_encount_rate-mean(avg_encount_rate,na.rm=TRUE),
               st_anomaly= anomaly/sd(avg_encount_rate,na.rm=TRUE))%>%
        select(-anomaly)%>%
       ungroup()}
   

   
    #create a matrix to contain all species abundance anomalies
    abundance_change<-matrix(data=NA,nrow = 25200, ncol = 15)
    colnames(abundance_change)<-c("rwbl","cogr","bhco","rtha","ospr","rbwo",
                                  "nofl","hawo", "dowo","modo","ropi","hosp",
                                  "eust","tuvu", "piwo")
    
  
    
    
    #read in species abundance files
  for (j in 1:length(name_list$sp_name)) {
    species<-read.csv(paste0(name_list$sp_name[1],"/",name_list$sp_alpha[1],"_weekly_ER_05_18.csv"))%>%
      
      #create a "season" field based on eBird status and trends breeding seasons of our study species 
      #(Breeding = Summer, Post-Breeding= Fall, Non-Breeding= Winter, Pre-Breeding=Spring)
      mutate(date=mdy(observation_date),
             month=month(date),
             year=year(date))%>%
      filter(!is.na(year)&year>=2013)%>%
      select(-c("X","observation_date","day_of_year"))
    
    
    #--------------------------------------------------------------------------
    test<-species%>%group_by(city,month,year)%>%
      summarise(avg_encount_rate=mean(avg_encount_rate,na.rm=TRUE))%>%
      ungroup()%>%
      group_by(city,month)%>%
      mutate(mean=mean(avg_encount_rate,na.rm=TRUE),
             anomaly= avg_encount_rate-mean,
             st_anomaly= anomaly/sd(avg_encount_rate,na.rm=TRUE),
             Date=as.Date(dmy(paste0("1-",month,"-",year))))%>%
      select(-anomaly)%>%
      ungroup()
    
    mar<-test[test$city=="UXBRIDGE" & test$month==1,]
    sep<-test[test$city=="UXBRIDGE" & test$month==7,]
    library(ggpubr)
    mar<- ggplot(data = mar, aes(x=Date, y=avg_encount_rate)) +
      geom_line()+ 
      geom_hline(yintercept=mar$mean, linetype="dashed", color = "red")+
      labs(y="January ERs")
    
    sep<- ggplot(data = sep, aes(x=Date, y=avg_encount_rate)) +
      geom_line()+ 
      geom_hline(yintercept=sep$mean, linetype="dashed", color = "red")+
      labs(y="July ERs")
    ggarrange(mar,sep, ncol=, nrow=2)
    ggsave("RWBL_anomalies.jpeg",
           width=12.5,
           height = 9,
           dpi = 96,
           units = "in")
    
    #----------------------------------------------------------------------------
      #apply change metric calculations
     species_anomaly<-anomaly(species)

    
    #Fill in matrix with weekly ER change metrics for each species
    abundance_change[,j]<-species_anomaly$st_anomaly
    
  }

  #fill in variables for city and time in the weekly species abundance matrix   
    abundance_change<-as.data.frame(abundance_change)
    abundance_change$city <- species_anomaly$city
    abundance_change$year <- species_anomaly$year
    abundance_change$month <- species_anomaly$month
    
    #Fill monthly bird ER anomalies to weekly values
    abundance_change_filled<- abundance_change%>%
      mutate(Date=as.Date(dmy(paste0("1-",month,"-",year))))%>%
      complete(Date = seq.Date(min(Date), ymd("2018-12-31"), by="day"),city) %>%
      group_by(city)%>%
      arrange(Date, .by_group=TRUE)%>%
      fill(c(3:19))%>%
      ungroup()%>%
      mutate(week=week(Date))%>%
      group_by(week, city, year)%>%
      summarise_all(mean,na.rm=TRUE)%>%
      select(-month)
  

#--------------------------------------------  
#Merge the outage and bird abundance datasets
#--------------------------------------------
  #select 2013-2018 from the abundance data and merge with outages
  out_abund <- merge(abundance_change_filled,outs, by=c("year","week","city"), all=T)
    
  #fill in NA with 0, seasons and years in towns that have no recorded outages from 2013-2018
  out_abund[is.na(out_abund)] <- 0
  
  #add variables for human population density and seasonal average weekly minimum air temperature
  out_abund_var<-as.data.frame(merge(out_abund,towns_nogeom[,c("city","POP2010","TOWN_ID","pop_dens")],by="city", all.x = TRUE))%>%
    merge(temp,by=c("TOWN_ID","year","week"),all.x = TRUE)
  
  ## We now have a weekly dataset containing:
  
  ## 1. 2 weekly measures of outages (coh and number of outages)
  ## 2. Monthly anomalies of relative bird abundance for 15 species known to cause outages
  ## 3. weekly air temperature minimum
  ## 4. Human population density from 2010 census
  ## 5. Weekly Cooling and Warming degree day maximums
  ## 6. Season
  
  ## for each town in MA, each week from 2013-2018
  
  #write to file
  write.csv(out_abund_var, "out_abund_var.csv",row.names = F)

#-----------------------------  
#Data exploration figures 
#-----------------------------
  
out_abund_var<-read.csv("out_abund_var.csv")%>%
    mutate(Date=ymd(Date))
  
#Look at time series of outage frequency statewide and bird abundance
  timeseries<- out_abund_var%>%group_by(Date)%>%
    summarise(HOSP.Anomaly = mean(hosp,na.rm = TRUE),#specify bird species
              EUST.Anomaly = mean(eust,na.rm = TRUE),
              ROPI.Anomaly = mean(ropi,na.rm = TRUE),
              MODO.Anomaly = mean(modo,na.rm = TRUE),
              Number.Outages = mean(num_outs,na.rm = TRUE),
              COH=mean(coh,na.rm = TRUE))%>% 
    gather(measure, value, HOSP.Anomaly:COH)     ## stack metrics into 1 column for plotting
  
  ggplot(data = timeseries, aes(x=Date, y=value,group=1)) +
    geom_line() +
    scale_x_date(date_breaks="6 month", minor_breaks=NULL, date_labels="%b %Y")+
    xlab("") +
    ylab("State Average") + 
    facet_grid(measure ~ ., scales='free')
  
  ggsave("state_avg_timeseries_urban.jpeg",
         width=12.5,
         height = 10,
         dpi = 96,
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
  year<-2018
  
  #map
  ggplot() +
    #specify outage metric
    geom_sf(data = towns_var[towns_var$week==week&towns_var$year==year,], aes(fill = num_outs)) +
    scale_fill_viridis_c(option = "plasma")+
    #specify bird species
    geom_sf(data=cent_var[cent_var$week==week&cent_var$year==year,],aes(color = rtha))+
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
  ggsave("Summer18_outs_RBWO_townmap.jpeg",
         width=14.2,
         height = 6.9,
         dpi = 500,
         units = "in")
  
  
#correlations between all space/time points
  #results:1. House Sparrow (hosp),Northern Flicker (nofl), Hairy Woodpecker (hawo), Red-tailed Hawk (rtha),
          #Osprey (ospr), Brown-headed Cowbird (bhco), and Common Grackle (cogr) had the highest r (>=.18) 
  
          #2. European starling (eust),Red-winged Blackbird (rwbl), Turkey Vulture (tuvu), Downy Woodpecker (dowo),
          #Rock Pigeon (ropi), Mourning Dove (modo), Pileated Woodpecker (piwo), Red-bellied Woodpecker (rbwo)
          #did not have strong correlations across all space time points
  
          #3. ER st. changes were more often correlated with outage frequency than st. anomalies
  
  corr<-out_abund_var%>%
    dplyr::select(-c("year","city","week"))
  jpeg("out_birds_corrmatrix4.jpeg", units = "in", width = 7,height=6, res=300)
  pairs.panels(corr[,c(11:18,20)], #specify which species to correlate with outage metric
               method = "spearman",
               hist.col = "#00AFBB",
               density=TRUE,
               ellipses = FALSE,
               lm=TRUE,
               stars=TRUE)
  dev.off()
  
 
#Correlations across towns through time
  #Results:1. correlations between specific species abundance and outages changes over time
          #2. some species groups (eg. blackbirds) cycle through years with strong correlations and years with little correlation
          #   Could be reflecting years with larger than normal flocks of birds with shared behaviors.
          #   Influenced by weather/migration timing, food availability....

  cor1 <- function(df)
  {
    return(data.frame(BHCO = cor(df$bhco_stchange, df$num_outs,method = "spearman")))
  }
  cor2 <- function(df)
  {
    return(data.frame(RWBL = cor(df$rwbl_stchange, df$num_outs,method = "spearman")))
  }
  cor3 <- function(df)
  {
    return(data.frame(COGR = cor(df$cogr_stchange, df$num_outs,method = "spearman")))
  }
  cor4 <- function(df)
  {
    return(data.frame(RBWO = cor(df$rbwo_stchange, df$num_outs,method = "spearman")))
  }
  seasons_1_count<-ddply(out_abund_season, .(season,year), cor1)
  seasons_2_count<-ddply(out_abund_season, .(season,year), cor2)
  seasons_3_count<-ddply(out_abund_season, .(season,year), cor3)
  seasons_4_count<-ddply(out_abund_season, .(season,year), cor4)
  seasons_cors<-merge(seasons_1_count,seasons_2_count,by=c("season","year"))%>%
    merge(seasons_3_count,by=c("season","year"))%>%merge(seasons_4_count,by=c("season","year"))%>%
    gather(cor, value,3:6)%>%
    mutate(month=case_when(
                          season=="winter"~ 1,
                          season=="spring" ~ 4,
                          season=="summer" ~ 8,
                          season=="fall" ~ 10),
           date=dmy(paste0("1-",month,"-",year)))
  
  ggplot(data=seasons_cors,aes(x=date, y=value,group=cor)) +
    geom_line(aes(color=cor))+
    labs(y="Spearman's Cor. Coeff.",
         x="Date",
         title="Outages-Species Correlation Through Time",
         color="Correlation")
  
  ggsave("out_blackbirds_corr_throughtime.jpeg",
         width=14.2,
         height = 6.9,
         dpi = 300,
         units = "in")
  
  
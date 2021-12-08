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
    st_drop_geometry()
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
  
  temp_week<-tbl%>%
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
  outs<-read.csv("MA_SAIDI.csv")
  
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
    mutate(date=ymd(Date.Out),
           month=month(date),
           week=week(date),
           year=year(date))
  
  #summarize outages by week
  #We can calculate 2 different outage metrics, the number of outages and customer outage hours (coh)
  #**several recorded outages have 0's for number of customers affected or outage duration
  #**we could substitute in small values for 0? What impact does a coh of 0 have on the grid?
  #*previous studies have focused on the number of outages, since it's a more direct reflection of
  #*animal-equipment interactions. Check distribution of outage frequency (GLM?).
  
  outs_week<-outs%>%group_by(city,week,year)%>%
    summarise(coh=sum(Original.Number.Customers.Affected,na.rm = T)*sum(Actual.Duration,na.rm = T),
              num_outs=n())

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


  
  #Calculate abundance change metrics
  
    #1. Weekly standardized anomalies-
    #deviation of each week's ER from the average ER for that week across years, divided by the stdev of the week across years
    #How atypical is each week compared to other years. 
    #seasonally would be more biologically realistic 
  anomaly<-function(x) {x%>%group_by(city,season,year)%>%
      summarise(avg_encount_rate=mean(avg_encount_rate,na.rm=TRUE))%>%
      ungroup()%>%
      group_by(city,season)%>%
        mutate(anomaly= avg_encount_rate-mean(avg_encount_rate,na.rm=TRUE),
               st_anomaly= anomaly/sd(avg_encount_rate,na.rm=TRUE))%>%
        select(-anomaly)%>%
       ungroup()}

   
    #create a matrix to contain all species abundance anomalies
    abundance_change<-matrix(data=NA,nrow = 19600, ncol = 15)
    colnames(abundance_change)<-c("rwbl","cogr",
                                  "bhco","rtha",
                                  "ospr","rbwo",
                                  "nofl","hawo",
                                  "dowo","modo",
                                  "ropi","hosp",
                                  "eust","tuvu",
                                  "piwo")
    
    
    #read in species abundance files
  for (j in 1:length(name_list$sp_name)) {
    species<-read.csv(paste0(name_list$sp_name[j],"/",name_list$sp_alpha[j],"_weekly_ER_05_18.csv"))%>%
      
      #create a "season" field based on eBird status and trends breeding seasons of our study species 
      #(Breeding = Summer, Post-Breeding= Fall, Non-Breeding= Winter, Pre-Breeding=Spring)
      mutate(date=mdy(observation_date),
             month=month(date),
             week=week(date),
             year=year(date),
             winter=ifelse(month%in%c(12,1,2),1,0), #Dec-Feb
             spring=ifelse(month%in%c(3,4,5),1,0), #March-May
             summer=ifelse(month%in%c(6,7,8),1,0), #June-Aug
             fall=ifelse(month%in%c(9,10,11),1,0),#Sept-Nov
             season=case_when(
               winter==1~ "winter",
               spring==1 ~ "spring",
               summer==1 ~ "summer",
               fall==1 ~ "fall"))%>% 
      filter(!is.na(year))%>%
      select(-c("X","month","observation_date","day_of_year"))
    
      #replace 0 valued encounter rates with a very small number to avoid errors (diving by 0)
     species$avg_encount_rate[species$avg_encount_rate==0]<-0.0000001
     
     #apply change metric calculations
     species_change<-anomaly(species)
 
    #clean up unnecessary columns
    #species_week<- species_change%>%
     # select(-c("TOWN_ID","date","avg_encount_rate", "st_ER"))
    
    #Fill in matrix with weekly ER change metrics for each species
    abundance_change[,j]<-species_change$st_anomaly

    
  }

  #fill in variables for city and time in the weekly species abundance matrix   
    abundance_change<-as.data.frame(abundance_change)
    abundance_change$winter <- species_change$winter
    abundance_change$spring <- species_change$spring
    abundance_change$summer <- species_change$summer
    abundance_change$season <- species_change$season
    abundance_change$city <- species_change$city
    abundance_change$year <- species_change$year

    
  #replace above 1 change values for osprey
   # abundance_change$ospr_stchange[abundance_change$ospr_stchange>1]<-1

#--------------------------------------------  
#Merge the outage and bird abundance datasets
#--------------------------------------------
  #select 2013-2018 from the abundance data and merge with outages
  out_abund_week <- merge(filter(abundance_change,year>=2013),outs_week, by=c("year","week","city"), all=T)
    
  #fill in NA with 0, seasons and years in towns that have no recorded outages from 2013-2018
  out_abund_week[is.na(out_abund_week)] <- 0
  
  #add variables for human population density and seasonal average weekly minimum air temperature
  out_abund_week<-as.data.frame(merge(out_abund_week,towns_nogeom[,c("city","POP2010","TOWN_ID")],by="city", all.x = TRUE))%>%
    merge(temp_week,by=c("TOWN_ID","year","week"),all.x = TRUE)
  
  ## We now have a dataset(out_abund_week) containing:
  
  ## 1. 2 measures of outages (coh and number of outages)
  ## 2. anomalies of relative bird abundance for 15 species known to cause outages
  ## 3. weekly air temperature minimum
  ## 4. Human population density from 2010 census
  ## 5. Season
  
  ## for each town in MA, each week from 2013-2018

#-----------------------------  
#Data exploration figures 
#-----------------------------
  
#Use seasonally summarized data to make figures easier to read
  out_abund_season<-out_abund_week%>%
    mutate(season=case_when(
      winter==1~ "winter",
      spring==1 ~ "spring",
      summer==1 ~ "summer",
      fall==1 ~ "fall"))%>%
    group_by(year,season,city)%>%
    summarise(num_outs=sum(num_outs,na.rm = T),
              coh=sum(coh,na.rm = T),
              rwbl_stchange=mean(rwbl_stchange,na.rm = T),rwbl_stanom=mean(rwbl_stanom,na.rm = T),
              cogr_stchange=mean(cogr_stchange,na.rm = T),cogr_stanom=mean(cogr_stanom,na.rm = T),
              bhco_stchange=mean(bhco_stchange,na.rm = T),bhco_stanom=mean(bhco_stanom,na.rm = T),
              rtha_stchange=mean(rtha_stchange,na.rm = T),rtha_stanom=mean(rtha_stanom,na.rm = T),
              ospr_stchange=mean(ospr_stchange,na.rm = T),ospr_stanom=mean(ospr_stanom,na.rm = T),
              rbwo_stchange=mean(rbwo_stchange,na.rm = T),rbwo_stanom=mean(rbwo_stanom,na.rm = T),
              nofl_stchange=mean(nofl_stchange,na.rm = T),nofl_stanom=mean(nofl_stanom,na.rm = T),
              hawo_stchange=mean(hawo_stchange,na.rm = T),hawo_stanom=mean(hawo_stanom,na.rm = T),
              dowo_stchange=mean(dowo_stchange,na.rm = T),dowo_stanom=mean(dowo_stanom,na.rm = T),
              modo_stchange=mean(modo_stchange,na.rm = T),modo_stanom=mean(modo_stanom,na.rm = T),
              ropi_stchange=mean(ropi_stchange,na.rm = T),ropi_stanom=mean(ropi_stanom,na.rm = T),
              hosp_stchange=mean(hosp_stchange,na.rm = T),hosp_stanom=mean(hosp_stanom,na.rm = T),
              eust_stchange=mean(eust_stchange,na.rm = T),eust_stanom=mean(eust_stanom,na.rm = T),
              tuvu_stchange=mean(tuvu_stchange,na.rm = T),tuvu_stanom=mean(tuvu_stanom,na.rm = T),
              piw_stchangeo=mean(piwo_stchange,na.rm = T),piwo_stanom=mean(piwo_stanom,na.rm = T))%>%
    ungroup()
  
#Look at time series of outage frequency statewide and bird abundance
  #time series by season, ER change and anomalies still have seasonality...
  #declines in winter are continuously greater, while summer is constant through time
  #
  timeseries<- abundance_change%>%group_by(season,year)%>%
    summarise(ER.Anomaly = mean(rtha,na.rm = TRUE))%>%#,#specify bird species
              #ER.Change = mean(ospr_stchange,na.rm = TRUE),#specify bird species
              #Number.Outages = sum(num_outs,na.rm = TRUE))%>% 
    #gather(measure, value, ER.Anomaly:Number.Outages)%>%     ## stack metrics into 1 column for plotting
    mutate(month=case_when(
      season=="winter"~ 1,
      season=="spring" ~ 4,
      season=="summer" ~ 8,
      season=="fall" ~ 10),
      date=dmy(paste0("1-",month,"-",year)))

  
  ggplot(data = timeseries, aes(x=date, y=ER.Anomaly)) +
    geom_line() +
    xlab("") +
    ylab("State Average") #+ 
   # facet_grid(measure ~ ., scales='free')
  
  ggsave(paste0("C:/Users/emfeng/OneDrive - Lincoln Park Zoo/PRISM DL Data/ebird raw data/ebird_data_encounter_rates/", name_list$sp_name[i]," Encounter Rate Model Figures/",name_list$sp_file[i],"_ERchange_outages_avgplots.jpeg"),
         width=12.5,
         height = 9.375,
         dpi = 96,
         units = "in")
  
  
#Map outages and bird abundance
  
  #The town polygons will represent outages and centroid shapes will represent bird abundance
  
  #merge to town shapefile
  towns_var<-left_join(towns,out_abund_season,by="city")
  
  #merge to town centroids
  #change bird abundance to a categorical variable for plotting
  cent_var<-left_join(cent,out_abund_season,by="city")%>%
    mutate(tuvu=ifelse(tuvu_stanom>-1&tuvu_stanom<1, "Average", ifelse(tuvu_stanom<=-1, "Below Average","Above Average")),
           rtha=ifelse(rtha_stanom>-1&rtha_stanom<1, "Average", ifelse(rtha_stanom<=-1, "Below Average","Above Average")),
           ospr=ifelse(ospr_stanom>-1&ospr_stanom<1, "Average", ifelse(ospr_stanom<=-1, "Below Average","Above Average")),
           eust=ifelse(eust_stanom>-1&eust_stanom<1, "Average", ifelse(eust_stanom<=-1, "Below Average","Above Average")),
           hosp=ifelse(hosp_stanom>-1&hosp_stanom<1, "Average", ifelse(hosp_stanom<=-1, "Below Average","Above Average")),
           ropi=ifelse(ropi_stanom>-1&ropi_stanom<1, "Average", ifelse(ropi_stanom<=-1, "Below Average","Above Average")),
           piwo=ifelse(piwo_stanom>-1&piwo_stanom<1, "Average", ifelse(piwo_stanom<=-1, "Below Average","Above Average")),
           rbwo=ifelse(rbwo_stanom>-1&rbwo_stanom<1, "Average", ifelse(rbwo_stanom<=-1, "Below Average","Above Average")))
  cent_var<-cent_var%>%filter(city!="HULL")
  
  #specify the season and year to map
  season<-"summer"
  year<-2018
  
  #map
  ggplot() +
    #specify outage metric
    geom_sf(data = towns_var[towns_var$season==season&towns_var$year==year,], aes(fill = num_outs)) +
    scale_fill_viridis_c(option = "plasma")+
    #specify bird species
    geom_sf(data=cent_var[cent_var$season==season&cent_var$year==year,],aes(color = rtha))+
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
  
  corr<-out_abund_season%>%
    filter(!(num_outs==0))%>% #Filter out instances with no recorded outages
    dplyr::select(-c("year","city","season"))
  jpeg("out_birds_corrmatrix5.jpeg", units = "in", width = 7,height=6, res=300)
  pairs.panels(corr[,c(3:8,1)], #specify which species to correlate with outage metric
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
  
  
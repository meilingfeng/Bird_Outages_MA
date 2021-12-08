#This code provides pre-downloaded and filtered data from the eBird Basic Dataset (EBD May 2019)
#https://ebird.org/data/download/ebd
library(auk)
library(lubridate)
library(sf)
library(gridExtra)
library(tidyverse)
library(dplyr)
library(boot)
library(ggpubr)
library(arm)
library(caret)
library(zoo)
library(ggpmisc)
library(exactextractr)
library(verification)
library(ebirdst)
#dggridR no longer on CRAN, install separately below or download here https://cran.r-project.org/src/contrib/Archive/dggridR/ 
library(devtools) #Use `install.packages('devtools')` if need be
install_github('r-barnes/dggridR', vignette=TRUE)


#-----------------
#DATA PREPARATION
#-----------------
memory.limit(100000)

#Load Massachusetts ebird data files
load("Scratch/Outputs/1_mass_bird_data_2018.rda")
load("Objects and Data/0_MA_town_shapefiles.rda")


name_list <- list(
  sp_name = c("Red-winged Blackbird","Common Grackle","Brown-headed Cowbird","Red-tailed Hawk","Osprey", "Red-bellied Woodpecker",
              "Northern Flicker","Hairy Woodpecker","Downy Woodpecker",
              "Mourning Dove","House Sparrow","European Starling","Turkey Vulture","Pileated Woodpecker"),
  #Four-letter species codes
  sp_file = c("rwbl","cogr","bhco","rtha","ospr","rbwo","nofl","hawo","dowo",
              "modo","hosp","eust","tuvu","piwo"), 
  #Scientific names
  sp_sci = c("Agelaius phoeniceus","Quiscalus quiscula","Molothrus ater", "Buteo jamaicensis","Pandion haliaetus", "Melanerpes carolinus",
             "Colaptes auratus","Dryobates villosus","Dryobates pubescens", "Zenaida macroura", "Passer domesticus", "Sturnus vulgaris",
             "Cathartes aura", "Dryocopus pileatus"),
  #day of the year with peak breeding season detection frequency for each species
  peak = c(121,145,121,289,205,121,109,43,121,181,193,157,103,115)) 


sp_ebird<-list()



#1. a) Data filtering functions for standardization

#Zero-fill observation data to create presence/absence data using auk_zerofill()
#auk_zerofill automatically applies auk_rollup() which rolls up sub-species observations to species level (drops hybids and family level observations)
#and auk_unique() which gets rid of duplicate checklists that are part of a group suvery and keeps one idependent copy.
#using collapse = TRUE condenses the observation data and checklist data into one dataframe. 

#Also reclassify "random" protocol as either stationary or traveling, and add List Length (LLA) variable

zf_reclass <- function(x) {x%>%
    mutate(protocol=ifelse(protocol_type == "Random" & effort_distance_km == 0, "Stationary", 
                           ifelse(protocol_type == "Random" & effort_distance_km > 0, "Traveling", protocol_type)))%>%
    dplyr::select(-protocol_type)%>%
    rename(protocol_type=protocol)%>%
    left_join(LLA, by = "checklist_id")}

#convert time of observation to hours since midnight

time_to_decimal <- function(x) {
  x <- hms(x, quiet = TRUE)
  hour(x) + minute(x) / 60 + second(x) / 3600
}

#clean up variables

clean_vars <- function(data) {
  data %>% 
    mutate(
      # convert individual counts of 'X' to NA
      observation_count = if_else(observation_count == "X", 
                                  NA_character_, observation_count),
      observation_count = as.integer(observation_count),
      # effort_distance_km to 0 for non-travelling/stationary counts
      effort_distance_km = if_else(protocol_type != "Traveling", 
                                   0, effort_distance_km),
      # convert time to decimal hours since midnight
      time_observations_started = time_to_decimal(time_observations_started),
      # split date into year, month, and day of year
      month = month(observation_date),
      year = year(observation_date),
      day_of_year = yday(observation_date),
      Week_of_year = week(observation_date),
      day_of_week=wday(observation_date)
    )}

#additional filtering (Strimas-Mackey et al. 2020) to reduce checklist detection variability:
#Survey duration less than 5hr long, 5km in length, and 10 or fewer observers.
#remove checklists with less than 5 species reported (likely a false reporting of completeness)

extra_filters <- function(data){
  data %>% 
    filter(
      # 10 or fewer observers
      number_observers <= 10,
      #year range, ebird data availability greatly increases past 2005
      year >= 2005 & year <=2018,
      #List length >=5 species
      LLA >= 5)}
#specify time range
time<-c(2005:2018)



#1. b) Filter eBird species files with filtering functions
species_zf<-purrr::map(species_zf,zf_reclass)
species_zf_local<- purrr::map2(species_zf,species_filt,function (x,y) {filter(x,locality_id %in% y$locality_id)})
ebird_latlong<-purrr::map(species_zf_local,function(x) {clean_vars(x)%>%
    extra_filters()})


#The raw ebird data is now filtered to the desired parameters
#Then join to MA town spatial features
map_proj <- st_crs(4326)

sp_ebird <- purrr::map(ebird_latlong, function(x) {st_as_sf(x, 
                                                     coords = c("longitude","latitude"), 
                                                     crs=map_proj)%>%
    st_join(ma_towns, join = st_intersects, left = FALSE)%>%
    st_drop_geometry()})

ebird_towns<-sp_ebird 
ebird_towns2<-purrr::map(ebird_towns,function(x){dplyr::select(x,checklist_id,city,TOWN_ID)})
##Data with towns and lat long coordinates
ebird_all<-purrr::map2(ebird_latlong, ebird_towns2,right_join, by= "checklist_id")



#Spatiotemporal subsampling to reduce spatial and temporal bias
ebird_ss<-list()
# set random number seed to insure fully repeatable results
set.seed(1)


for (j in 1:length(name_list$sp_file)) {
  ebird<-as.data.frame(ebird_all[[j]])%>%mutate(observation_date=ymd(observation_date))
  
  # Reduce spatial and temporal bias as well as class imbalance (detection to non-detection ratio)
  # Sample one detection and one non-detection checklist from each grid cell for each week
  # generate hexagonal grid with ~ 5 km between cells
  dggs <- dggridR::dgconstruct(spacing = 5)
  # get hexagonal cell id and week number for each checklist
  checklist_cell <- ebird%>% 
    mutate(cell = dggridR::dgGEO_to_SEQNUM(dggs, longitude, latitude)$seqnum,
           Year = year(observation_date),
           month = month(observation_date),
           week= week(observation_date))
  # sample one checklist per grid cell per week
  # sample detection/non-detection independently 
  ebird_ss[[j]] <- checklist_cell%>% 
    group_by(species_observed, Year,week, cell) %>% 
    sample_n(size = 1) %>% 
    ungroup()
}




#NOTE: detection prevalence increased with subsampling which increases occurrence rate of model predictions




#3. Check data distribution
#-----------------------------
# convert checklists to spatial features
e_latlong_all<-bind_rows(ebird_ss, .id = "column_label")
ebird_pts <- as.data.frame(e_latlong_all) %>%  
  distinct(latitude,longitude)%>%
  st_as_sf(coords = c("longitude","latitude"), crs = 4326) 



#4. Data balancing for ebird, final touches
#-----------------------------------
ebird_ss_balanced<-list()
survey_count_list<-list()

  #For each species...
  for(j in 1:length(ebird_ss)){
    
    sp_comp<-ebird_ss[[j]]#i,j
    
    set.seed(1)
    survey_count<-sp_comp%>%group_by(Year,city)%>%
      dplyr::summarise(count=n())%>%
      ungroup()%>%
      group_by(city)%>%
      mutate(average=floor(mean(count)),
             adjustment=average-count)
    survey_count_list[[j]]<-unique(survey_count[,c('city','average')])
    
    sp_comp<-left_join(sp_comp,survey_count,by=c("Year","city"))
    
    #Balance sample sizes across years
    # years with below average checklists
    low_sample_years <- sp_comp %>%
      group_by(Year,city)%>% 
      filter( n() < mean(average))%>%
      sample_n(size = mean(average), replace = T)
    
    # years with above average checklists
    high_sample_years <- sp_comp %>%
      group_by(Year,city)%>% 
      filter( n() > mean(average))%>%
      sample_n(size = mean(average), replace = F)
    
    # years with average checklists
    mid_sample_years <- sp_comp %>%
    group_by(Year,city)%>% 
      filter( n() == mean(average))
 
    
    ebird_ss_balanced[[j]] <- rbind(low_sample_years,high_sample_years,mid_sample_years)
    
  }

save(ebird_ss_balanced, file="Objects and Data/2_filtered_ebird_data.rda")
save(name_list, file="Objects and Data/0_species_list.rda")
save(ma_towns,cent, file="Objects and Data/0_MA_town_shapefiles.rda")
#-------------------------------------
#PREPARING ENVIRONMENTAL COVARIATES
#-------------------------------------

#Summarise within each town

#NLCD Land cover
#---------------------------

grids <- list.files("Scratch/Raw_Data/Environmental Data/NLCD" , pattern = "*.tiff$")

#create a raster stack from the input raster files 
landcover <- raster::stack(paste0("Scratch/Raw_Data/Environmental Data/NLCD/", grids[-8]))

save(landcover, file="Objects and Data/1_landcover_rasters.rda")

# label layers with year
landcover <- names(landcover) %>% 
  str_extract("\\d{4}") %>% 
  paste0("y", .) %>% 
  setNames(landcover, .)
landcover


town_year <- ma_towns %>% 
  dplyr::select(city)%>%
  slice(rep(1:n(), each = length(time)))%>%
  arrange(city)%>%
  mutate(year=rep(time,times=length(ma_towns$city)),
  # for 2019 use 2018 landcover data
         year_lc = case_when(
          as.integer(year) > 2004 & as.integer(year)<2008~as.character(2006),
          as.integer(year) >= 2008 & as.integer(year)<2011~as.character(2008),
          as.integer(year) >= 2011 & as.integer(year)<2013~as.character(2011),
          as.integer(year) >= 2013 & as.integer(year)<2016 ~ as.character(2013),
          as.integer(year) >= 2016 ~ as.character(2016)),
        year_lc = paste0("y", year_lc)) %>% 
  # transform to modis projection
  st_transform(crs = raster::projection(landcover)) %>% 
  # nest by year
  nest(data = c(year, city, geometry))

# function to summarize landcover data for all checklists in a given year
calculate_pland <- function(yr, regions, lc) {
  locs <- st_set_geometry(regions, NULL)
  exact_extract(lc[[yr]], regions, progress = FALSE) %>% 
    map(~ count(., landcover = value)) %>% 
    tibble(locs, data = .) %>% 
    unnest(data)
}
# iterate over all years extracting landcover for all checklists in each
lc_extract <- town_year %>% 
  mutate(pland = map2(year_lc, data, calculate_pland, lc = landcover)) %>% 
  dplyr::select(pland) %>% 
  unnest(cols = pland)

#calculate pland, proportion of neighborhood within each landcover class
pland <- lc_extract %>% 
  # calculate proportion
  group_by(city, year) %>% 
  mutate(pland = n / sum(n)) %>% 
  ungroup() %>% 
  dplyr::select(-n) %>% 
  # remove NAs after tallying so pland is relative to total number of cells
  filter(!is.na(landcover))

# convert names to be more descriptive
lc_names <- tibble(landcover = c(0,11,12,21,22,23,24,31,41,42,43,52,71,81,82,90,95),
                   lc_name = c("Unclassified", 
                               "Open_Water", 
                               "Perennial_Snow_Ice", 
                               "Developed_Open", 
                               "Developed_Low", 
                               "Developed_Medium",
                               "Developed_High", 
                               "Barren_Land", 
                               "Deciduous_Forest", 
                               "Evergreen_Forest", 
                               "Mixed_Forest", 
                               "Shrub_Scrub", 
                               "Herbaceuous", 
                               "Hay_Pasture", 
                               "Cultivated_Crops", 
                               "Woody_Wetlands",
                               "Emergent_Herbaceuous_Wetlands"))

#Classify into broader land cover categories
pland2 <- pland %>% 
  inner_join(lc_names, by = "landcover") %>% 
  arrange(landcover) %>% 
  dplyr::select(-landcover)%>%
  dplyr::filter(!(lc_name %in% c("Unclassified","Perennial_Snow_Ice")))%>%
  group_by(year, city)%>%
  #make new column with aggregated land cover names
  dplyr::mutate(lc_name=case_when(
    lc_name%in%c("Developed_Open","Developed_Low", 
                 "Developed_Medium","Developed_High")~ "Developed",
    lc_name%in%c("Woody_Wetlands","Emergent_Herbaceuous_Wetlands")~"Wetland",
    lc_name%in%c("Deciduous_Forest","Evergreen_Forest","Mixed_Forest")~"Forest",
    lc_name%in%c("Herbaceuous","Hay_Pasture")~"Grassland",
    lc_name%in%c("Cultivated_Crops","Barren_Land","Open_Water","Shrub_Scrub")~lc_name),
    pland=pland)%>%
  ungroup()%>%
  #sum the proportion of land in each aggregated lc type
  group_by(year,city,lc_name)%>%
  summarise(pland=sum(pland))%>%
  ungroup()


# tranform to wide format, filling in missing values with 0%>% 
pland3 <- pland2 %>% 
  pivot_wider(names_from = lc_name, 
              values_from = pland, 
              values_fill = list(pland = 0))

# save
write.csv(pland3, "Scratch/Raw_Data/Environmental Data/NLCD/NLCD_pland_year_MAtowns.csv",row.names = F)



#Elevation
#-------------------------

elev <- raster::raster("Raw_Data/Environmental Data/Elevation/elevation_1KMmd_GMTEDmd.tif")

# crop raster to town shapefile
elev <- ma_towns %>% 
  st_transform(crs = raster::projection(elev)) %>% 
  raster::crop(elev, .) %>% 
  raster::projectRaster(crs = raster::projection(landcover))
#save(elev,file="Objects and Data/1_elevation_raster.rda")

# extract elevation values and calculate median and sd
ma_towns_elev<-ma_towns
elev_towns <- exact_extract(elev, ma_towns_elev, progress = FALSE) %>% 
  map_dfr(~ tibble(elevation_median = mean(.$value, na.rm = TRUE),
                   elevation_sd = sd(.$value, na.rm = TRUE))) %>% 
  # join to lookup table to get locality_id
  bind_cols(ma_towns_elev, .)%>%
  st_drop_geometry()


#Combine all checklist evironmental covariates
pland_elev_towns <- inner_join(pland3, elev_towns, by = "city")
write.csv(pland_elev_towns, "Scratch/Raw_Data/Environmental Data/pland_elev_year_MAtowns.csv",row.names = F)


save(ebird_ss_balanced,name_list,ma_towns,pland_elev_towns,
file="Scratch/Outputs/2_filtered_bird_data.rda")


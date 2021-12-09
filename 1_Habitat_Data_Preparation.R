library(lubridate)
library(sf)
library(tidyverse)
library(exactextractr)


#NLCD Land cover data
load("Objects and Data/0_MA_town_shapefiles.rda")
ma_towns<-ma_towns[!duplicated(ma_towns$city),]

#-------------------------------------
#PREPARING ENVIRONMENTAL COVARIATES
#-------------------------------------

#specify time range
time<-c(2005:2018)

#Land cover/habitat
#--------------------------------------------------------------------------------------------
#Summarize the proportion of habitat/land cover within each town

#get land cover data from NLCD, CONUS 2006-2016
#Go to NLCD and download land cover layers for 2006-2016 (select all years),
#https://www.mrlc.gov/viewer/
#Add downloaded .tiff raster files to a subfolder in the Bird_Outages_MA directory
#.>Scratch>Raw_Data>Environmental Data/NLCD

grids <- list.files("Scratch/Raw_Data/Environmental Data/NLCD" , pattern = "*.tiff$")

#create a raster stack from the input raster files 
landcover <- raster::stack(paste0("Scratch/Raw_Data/Environmental Data/NLCD/", grids[-8]))


# label land cover layers with their year
landcover <- names(landcover) %>% 
  str_extract("\\d{4}") %>% 
  paste0("y", .) %>% 
  setNames(landcover, .)

#make an annual observation for each town
town_year <- ma_towns %>% 
  dplyr::select(city)%>%
  slice(rep(1:n(), each = length(time)))%>%
  arrange(city)%>%
  mutate(year=rep(time,times=length(ma_towns$city)),
         year_lc = case_when(
           # for 2004- 2007 use 2006 landcover data
           as.integer(year) > 2004 & as.integer(year)<2008~as.character(2006),
           # for 2008- 2010 use 2008 landcover data
           as.integer(year) >= 2008 & as.integer(year)<2011~as.character(2008),
           # for 2011- 2012 use 2011 landcover data
           as.integer(year) >= 2011 & as.integer(year)<2013~as.character(2011),
           # for 2013- 2015 use 2013 landcover data
           as.integer(year) >= 2013 & as.integer(year)<2016 ~ as.character(2013),
           # for 2016- 2018 use 2016 landcover data
           as.integer(year) >= 2016 ~ as.character(2016)),
         year_lc = paste0("y", year_lc)) %>% 
  # transform to modis projection
  st_transform(crs = raster::projection(landcover)) %>% 
  # nest by year
  nest(data = c(year, city, geometry))

# function to summarize land cover data for all checklists in a given year
calculate_pland <- function(yr, regions, lc) {
  locs <- st_set_geometry(regions, NULL)
  exact_extract(lc[[yr]], regions, progress = FALSE) %>% 
    map(~ count(., landcover = value)) %>% 
    tibble(locs, data = .) %>% 
    unnest(data)
}

# iterate over years, extracting area of each land cover type in each town
lc_extract <- town_year %>% 
  dplyr::mutate(pland = map2(year_lc, data, calculate_pland, lc = landcover)) %>% 
  dplyr::select(pland) %>% 
  unnest(cols = pland)

#calculate pland, proportion of neighborhood within each land cover class
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


# tranform to wide format, filling in missing values with 0
pland3 <- pland2 %>% 
  pivot_wider(names_from = lc_name, 
              values_from = pland, 
              values_fill = list(pland = 0))



#Elevation
#--------------------------------------------------------------------------------------------
#Download 1km elevation data from Amatulli et al 2018
#http://www.earthenv.org/topography 
#Add downloaded .tif raster files to a subfolder in the Bird_Outages_MA directory
#.>Scratch>Raw_Data>Environmental Data/Elevation
elev <- raster::raster("Raw_Data/Environmental Data/Elevation/elevation_1KMmd_GMTEDmd.tif")

# crop raster to town shapefile
elev <- ma_towns %>% 
  st_transform(crs = raster::projection(elev)) %>% 
  raster::crop(elev, .) %>% 
  raster::projectRaster(crs = raster::projection(landcover))

# extract elevation values and calculate median and sd
ma_towns_elev<-ma_towns
elev_towns <- exact_extract(elev, ma_towns_elev, progress = FALSE) %>% 
  map_dfr(~ tibble(elevation_median = mean(.$value, na.rm = TRUE),
                   elevation_sd = sd(.$value, na.rm = TRUE))) %>% 
  # join to lookup table to get locality_id
  bind_cols(ma_towns_elev, .)%>%
  st_drop_geometry()


#Combine all environmental covariates
pland_elev_towns <- inner_join(pland3, elev_towns, by = "city")

save(pland_elev_towns, file="Objects and Data/2_habitat_elev_year_MAtowns.rda")

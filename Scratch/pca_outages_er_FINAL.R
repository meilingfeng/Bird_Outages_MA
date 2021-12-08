
library(tidyverse)
library(lubridate)
library(stringr)
library(factoextra)
library(ggfortify)
library(ggpubr)
library(sf)
library(psych)
library(DescTools)
library(wildlifeR)


#species alpha codes
alpha<-AOU_species_codes


#min temp data
setwd("../Raw_Data/Temperature Data")
tbl <-
   list.files(pattern = "*.csv") %>% 
   map_df(~read_csv(., col_types = cols(.default = "c")))
temp<-tbl%>%
   mutate(Date=mdy(Date),
          month=month(Date),
          week=week(Date),
          year=year(Date),
          season=ifelse(month%in% c(12,1:5), ifelse(month%in% c(12,1,2),"Winter", "Spring"),
                        ifelse(month%in% c(6:8), "Summer", "Fall")))%>%
   group_by(year,week,TOWN_ID)%>%
   summarise(tmin=min(tmin_F))
rm(tbl)

setwd('../../Scratch')

#MA towns
crs <- st_crs(4326)
towns<-st_read("../Raw_Data/MA_Shapefiles/ma_towns_namescorrected.shp")%>% 
   dplyr::select(city,TOWN_ID,Area_km2,county)%>%
   st_transform(crs = crs)%>%
   mutate(TOWN_ID=as.integer(TOWN_ID))
towns_nogeom<- towns%>%
   st_drop_geometry()

#------------
#####PCAs####
#------------

#PCA 1. For all space time (weeks and towns)
#---------------------------------------------
#times and towns with outage events
outages<-read.csv('Old Outputs/weekly_COH_avg_st_wt_join_animal_abundance_change.csv')%>%
   mutate(date=mdy(`date_time_out`),
          city=actual_city_town)%>%
   select(-c("actual_city_town","date_time_out","blackbirds","raptors","woodpeckers","urban"))

outs<-merge(outages,select(change_all,c("date","city","osprey")),by=c("city","date"), all.x = TRUE)
outs$osprey[outs$osprey>1]<-1
outs<-outs%>%mutate(raptors_osprey=osprey)%>%
   select(-osprey)
#write.csv(outs, "Old Outputs/weekly_COH_avg_st_wt_join_animal_abundance_change.csv")

#all ER through time (weekly 2005-2018)
change_all<-read.csv('Old Outputs/allspecies_change_ER.csv')%>%
   mutate(date=mdy(`date`))%>%
   mutate(month=month(date),
          week=week(date),
          season=ifelse(month%in% c(12,1:5), ifelse(month%in% c(12,1,2),"Winter", "Spring"),
                        ifelse(month%in% c(6:8), "Summer", "Fall")))
# Merge the two datasets: the weekly ER from 2005-2018 and outage events (zero fill missing dates for outages)
change_all <- merge(change_all, dplyr::select(outages, c(city,date,count,st_COH_avg)), by=c("date","city"), all=T)
change_all[is.na(change_all)] <- 0
#Add temperature and town data
change_all<-as.data.frame(merge(change_all,towns_nogeom[,-5],by="city", all.x = TRUE))%>%
   merge(temp,by=c("TOWN_ID","year","week"),all.x = TRUE)
#For the proportional yearly change values, adjust osprey change values to cap at 1 (100%)
change_all$osprey[change_all$osprey>1]<-1

#Add the per capita outage frequency measure
all<-change_all%>%
   mutate(countpercap=count/POP2010)
#Add columns with the outage measure percentiles for better plotting later on
all$countpercap[all$countpercap==0]<-NA
all$st_COH_avg[all$st_COH_avg==0]<-NA
all<-all%>%mutate(countpercap_PCT=ntile(countpercap, 5),
             coh_PCT=ntile(st_COH_avg,5))%>%
   unite(city_week_year,c("city","week","year"),sep="_",remove=FALSE)
all[is.na(all)] <- 0
#Transform ER change values to zscores for better interpretation
zscore <- function(x){(x - mean(x))/sd(x)}
all[,c(7:21)]<-apply(all[,c(7:21)],2,zscore)
all<-select(all,-c("TOWN_ID","count","cty_twn"))
#Don't need 2005 for change values
all<-  filter(all,year>2005)%>%
   filter(!duplicated(city_week_year))

#add alpha codes
spp<-data.frame(oldname=colnames(all[,6:20]),common=c("European Starling","House Sparrow","Mourning Dove",
                                                      "Rock Dove","Turkey Vulture","Hairy Woodpecker","Downy Woodpecker",
                                                      "Pileated Woodpecker","Red-bellied Woodpecker", "Northern Flicker",
                                                      "Red-tailed Hawk","Osprey","Red-winged Blackbird", "Brown-headed Cowbird",
                                                      "Common Grackle"))
alpha_spp<-alpha%>%
   filter(name%in%c("European Starling","House Sparrow","Mourning Dove",
                    "Rock Dove","Turkey Vulture","Hairy Woodpecker","Downy Woodpecker",
                    "Pileated Woodpecker","Red-bellied Woodpecker", "Northern Flicker",
                    "Red-tailed Hawk","Osprey","Red-winged Blackbird", "Brown-headed Cowbird",
                    "Common Grackle"))
join<-merge(spp,alpha_spp,by.x = "common",by.y="name",all.x = TRUE)
join$alpha.code<-as.character(join$alpha.code)
join[join$oldname=="flicker","alpha.code"]<-"NOFL"
join[join$oldname=="pigeon","alpha.code"]<-"ROPI"
all<-all %>% rename_(.dots=with(join, setNames(as.list(as.character(oldname)), alpha.code)))

#matrix for all seasons of all years and all towns
#write.csv(filter(all,year>=2013), "ER_change_zscores_outages_weeks_ALPHACODES.csv", row.names = FALSE)


#PCA 2. For all space time within specific seasons (All weeks and towns in a season across years)
#-------------------------------------------------------------------------------------------------
#Look for patterns within seasons.
#seasons defined by overlapping non-breeding, pre-breeding, breeding, post-breeding dates for species
#winter= Dec-Feb
#Spring= Mar-May
#summer= Jun-Aug
#fall= Sept-Nov

 #subset all space-time points into each season
 spring<-all%>%filter(season=="Spring")%>%dplyr::select(-season)
 summer<-all%>%filter(season=="Summer")%>%dplyr::select(-season)
 fall<-all%>%filter(season=="Fall")%>%dplyr::select(-season)
 winter<-all%>%filter(season=="Winter")%>%dplyr::select(-season)

 #-----------------------------------------------------------------
 #Run PCAs
 
 #Label observations
pr.all<-all %>%
    remove_rownames %>% 
    column_to_rownames(var = "city_week_year")
 
pr.winter<-winter %>% 
   unite(city_week_year,c("city","week","year"),sep="_",remove = FALSE)%>%
   remove_rownames %>% 
   column_to_rownames(var = "city_week_year")
 pr.spring<-spring %>%
    unite(city_week_year,c("city","week","year"),sep="_",remove = FALSE)%>%
    remove_rownames %>% 
    column_to_rownames(var = "city_week_year")
 pr.summer<-summer %>% 
    unite(city_week_year,c("city","week","year"),sep="_",remove = FALSE)%>%
    remove_rownames %>% 
    column_to_rownames(var = "city_week_year")
 pr.fall<-fall %>% 
    unite(city_week_year,c("city","week","year"),sep="_",remove = FALSE)%>%
    remove_rownames %>% 
    column_to_rownames(var = "city_week_year")
 
 #PCA outputs, create arranged outputs for plotting purposes
 
#All space time
pr.out.all<-prcomp(pr.all[,c(5:19)] , scale=TRUE)
   #arranged by seasons
   pr.all.seasons<-arrange(pr.all,season)
   pr.out.all.seasons=prcomp(pr.all.seasons[,c(5:19)] , scale=TRUE)
   #arranged by outage percentile
   pr.all.outages<-arrange(pr.all,st_COH_avg)
   pr.out.all.outages=prcomp(pr.all.outages[,c(5:19)] , scale=TRUE)
   #arranged by temperature
   pr.all.temp<-arrange(pr.all,tmin)
   pr.out.all.temp=prcomp(pr.all.temp[,c(5:19)] , scale=TRUE)
   #arranged by year
   pr.all.year<-arrange(pr.all,desc(year))
   pr.out.all.year=prcomp(pr.all.year[,c(5:19)] , scale=TRUE)

#Winter space time
pr.out.winter=prcomp(pr.winter[,c(5:19)] , scale=TRUE)
 #arranged by outage percentile
 pr.winter.outages<-arrange(pr.winter,st_COH_avg)
 pr.out.winter.outages=prcomp(pr.winter.outages[,c(5:19)] , scale=TRUE)
 #arranged by temperature
 pr.winter.temp<-arrange(pr.winter,tmin)
 pr.out.winter.temp=prcomp(pr.winter.temp[,c(5:19)] , scale=TRUE)
 
#Spring Space time
pr.out.spring=prcomp(pr.spring[,c(5:19)] , scale=TRUE)
 #arranged by outage percentile
 pr.spring.outages<-arrange(pr.spring,st_COH_avg)
 pr.out.spring.outages=prcomp(pr.spring.outages[,c(5:19)] , scale=TRUE)
 #arranged by temperature
 pr.spring.temp<-arrange(pr.spring,tmin)
 pr.out.spring.temp=prcomp(pr.spring.temp[,c(5:19)] , scale=TRUE)
 
#Summer space time
pr.out.summer=prcomp(pr.summer[,c(5:19)] , scale=TRUE)
 #arranged by outage percentile
 pr.summer.outages<-arrange(pr.summer,st_COH_avg)
 pr.out.summer.outages=prcomp(pr.summer.outages[,c(5:19)] , scale=TRUE)
 #arranged by temperature
 pr.summer.temp<-arrange(pr.summer,tmin)
 pr.out.summer.temp=prcomp(pr.summer.temp[,c(5:19)] , scale=TRUE)

#Fall space time 
pr.out.fall=prcomp(pr.fall[,c(5:19)] , scale=TRUE)
 #arranged by outage percentile
 pr.fall.outages<-arrange(pr.fall,st_COH_avg)
 pr.out.fall.outages=prcomp(pr.fall.outages[,c(5:19)] , scale=TRUE)
 #arranged by temperature
 pr.fall.temp<-arrange(pr.fall,tmin)
 pr.out.fall.temp=prcomp(pr.fall.temp[,c(5:19)] , scale=TRUE)

 #for annual space pcas, plot town coordinates (PC1&2) grouped by COH and season
 #shows where towns with these values fall along the PCs. Can interpret what these groups mean by
 #looking at correlation with PC scores and original species ER variables.

p1<-fviz_pca_biplot(pr.out.all.year,
                    axes = c(1,2), 
                    label = "var", col.var="black", arrowsize=0.6, labelsize=4, repel = TRUE,
                    col.ind=pr.all.year$year, pointshape=16,pointsize=1,
                    legend.title="Year",title="PC 1&2")
p2<-fviz_pca_biplot(pr.out.all.year,
                    axes = c(3,4), 
                    label = "var", col.var="black", arrowsize=0.6, labelsize=4, repel = TRUE,
                    col.ind =pr.all.year$year, pointshape=16,pointsize=1,
                    legend.title="Year",title="PC 3&4")
p3<-fviz_pca_biplot(pr.out.all.outages,
                    axes = c(5,7), 
                    label = c("var"), col.var="red", arrowsize=0.6, labelsize=6, repel = TRUE,
                    col.ind = as.factor(pr.all.outages$coh_PCT), pointshape=16,pointsize=1,palette = "Blues",
                    legend.title="COH\nPercentile",title="")+
   theme_dark()
p3<-ggpar(p3, font.x = c(14,"bold"),font.y=c(14,"bold"),font.legend = c(14,"bold"))

p4<-fviz_pca_biplot(pr.out.all.outages,
                    axes = c(7,8), 
                    label = c("var"), col.var="red", arrowsize=0.6, labelsize=6, repel = TRUE,
                    col.ind = as.factor(pr.all.outages$coh_PCT), pointshape=16,pointsize=1,palette = "Blues",
                    legend.title="COH\nPercentile",title="")
   theme_dark()+
p4<-ggpar(p4, font.x = c(14,"bold"),font.y=c(14,"bold"),font.legend = c(14,"bold"))

p5<-fviz_pca_biplot(pr.out.spring.temp,
                    axes = c(1,2), 
                    label = "var", col.var="red", arrowsize=0.6, labelsize=4, repel = TRUE,
                    col.ind=as.numeric(pr.spring.temp$tmin), pointshape=16,pointsize=1,
                    legend.title="Min Temperature (F)",title="PC 1&2")
p6<-fviz_pca_biplot(pr.out.spring.temp,
                    axes = c(3,4), 
                    label = "var", col.var="red", arrowsize=0.6, labelsize=4, repel = TRUE,
                    col.ind =as.numeric(pr.spring.temp$tmin), pointshape=16,pointsize=1,
                    legend.title="Min Temperature (F)",title="PC 3&4")
 
 p_season<-ggarrange(p1,p2, ncol=2, nrow=1,legend="right")
 
 ggsave("allspacetime_ERchanges_yeardesc_ALPHACODE.jpeg",
        width=16,
        height = 7,
        dpi = 300,
        units = "in")
 p_out<-ggarrange(p3,p4, ncol=2, nrow=1,legend="right")
 ggsave("allspacetime_ERchanges_coh_ALPHACODE_pc57.jpeg",
        width=16,
        height = 7,
        dpi = 300,
        units = "in")
 p_temp<-ggarrange(p5,p6, ncol=2, nrow=1,legend="right")
 ggsave("springspacetime_ERchanges_temp_ALPHACODE.jpeg",
        width=16,
        height = 7,
        dpi = 300,
        units = "in")
 
# p2 <- autoplot(pr.out.14, mutate(space_14,rescale_count= -scale(count_dens)), colour = 'rescale_count',
#                shape = FALSE, label.size = 3, #labels points with town names
#                loadings = TRUE, loadings.colour = 'black',
#                loadings.label = TRUE, loadings.label.size = 3)
# ggsave("space14_changesaifi_townlabels.jpeg",
#        width=14.2,
#        height = 6.9,
#        dpi = 300,
#        units = "in")


 
#***PC loading vectors***
pr.out.all$rotation%>%write.csv("allspacetime_ERchangepcloadings.csv")
pr.out.winter$rotation%>%write.csv("winter_ERchangepcloadings.csv")
pr.out.spring$rotation%>%write.csv("spring_ERchangepcloadings.csv")
pr.out.summer$rotation%>%write.csv("summer_ERchangepcloadings.csv")
pr.out.fall$rotation%>%write.csv("fall_ERchangepcloadings.csv")
summary(pr.out.all)

#plot first 2 pcs (variables). Positive corr variables point to same side, negative corr variables point to opposite sides
p1<-fviz_pca_var(pr.out.all,
             axes = c(5,7),
             labelsize=7,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE    # Avoid text overlapping
)
p1<-ggpar(p1, font.x = c(16,"bold"),font.y=c(16,"bold"),font.legend = c(16,"bold"))

p2<-fviz_pca_var(pr.out.all,
                 axes = c(7,8),
                 labelsize=7,
                 col.var = "contrib", # Color by contributions to the PC
                 gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                 repel = TRUE     # Avoid text overlapping
)
p2<-ggpar(p2, font.x = c(16,"bold"),font.y=c(16,"bold"),font.legend = c(16,"bold"))

p<-ggarrange(p1,p2, ncol=2, nrow=1,legend="bottom")
ggsave("allsppspacetime_change_pca_var_plot_ALPHACODE_pc5678.jpeg",
       width=14.2,
       height = 6.9,
       dpi = 300,
       units = "in")

#Percent contribution of variance of PCAs for each season
perc.winter <- (pr.out.winter$sdev^2/sum(pr.out.winter$sdev^2))
perc.spring <- (pr.out.spring$sdev^2/sum(pr.out.spring$sdev^2))
perc.summer <- (pr.out.summer$sdev^2/sum(pr.out.summer$sdev^2))
perc.fall <- (pr.out.fall$sdev^2/sum(pr.out.fall$sdev^2))

df.winter <- data.frame(pc = c('1','2','3','4','5-15'), 
                 perc = c(perc.winter[1:4],
                          sum(perc.winter[-c(1:4)])))  
df.spring <- data.frame(pc = c('1','2','3','4','5-15'), 
                        perc = c(perc.spring[1:4],
                                 sum(perc.spring[-c(1:4)]))) 
df.summer <- data.frame(pc = c('1','2','3','4','5-15'), 
                        perc = c(perc.summer[1:4],
                                 sum(perc.summer[-c(1:4)]))) 
df.fall <- data.frame(pc = c('1','2','3','4','5-15'), 
                        perc = c(perc.fall[1:4],
                                 sum(perc.fall[-c(1:4)]))) 
df2 <- bind_rows(df.winter, df.spring, df.summer, df.fall, .id = "season")

df2 <- df2 %>% 
   mutate(pc = factor(pc, levels = rev(c('1','2','3','4','5-15'))))
df2$season[df2$season==1]<-"winter"
df2$season[df2$season==2]<-"spring"
df2$season[df2$season==3]<-"summer"
df2$season[df2$season==4]<-"fall"

ggplot(df2)+
   geom_bar(aes(x = season, y = perc, fill = pc),
            stat = "identity")+
   labs(y="Proportion of Variance",
        x="Season",
        fill="PC")

#Access PCA results
#Results for individual space-time moments
pr.ind <- get_pca_ind(pr.out.all)
coord<-merge(pr.ind$coord,pr.all[,c("city","year","season","week","st_COH_avg","countpercap","tmin","date")],by=0,all.x=TRUE)%>% # Coordinates
   select(-c("Row.names"))%>%
   filter(year>=2013)
write.csv(coord, "pcvalues_ERchange_allspacetime.csv",row.names = FALSE)
pr.ind <- get_pca_ind(pr.out.fall)
coord<-merge(pr.ind$coord,pr.fall[,c("city","year","week","st_COH_avg","countpercap","tmin","date")],by=0,fall.x=TRUE)%>% # Coordinates
   select(-c("Row.names"))%>%
   filter(year>=2013)
write.csv(coord, "pcvalues_ERchange_fallspacetime.csv",row.names = FALSE)

#map PC values on MA towns

#All spacetime: High correlations between change PC3 and countpercap in 2017-2018. Map PC3 in Summer 2018.

#PC results for towns
pr.ind <- get_pca_ind(pr.out.all)
coord<-pr.ind$coord # Coordinates
coord<- as.data.frame(coord)%>%
   tibble::rownames_to_column(var="city_year")%>%
   mutate(city=sub("_.*", "",city_year),
          year=sub(".*_", "",city_year))%>%
   select(-city_year)

#merge PC values for each year to town shapefile
coord<-read.csv("Old Outputs/Species_PCA_old/pcvalues_ERchange_allspacetime.csv")
towns_pc<-left_join(towns,coord,by="city")

ggplot() +
   geom_sf(data = towns_pc[towns_pc$season=="Winter"&towns_pc$year==2017,], aes(fill = Dim.3)) +
   scale_fill_viridis_c(option = "plasma")

ggsave("allspacetime_Summer18_changePC3_townmap.jpeg",
       width=14.2,
       height = 6.9,
       dpi = 300,
       units = "in")
#Map COH for reference
towns_coh<-left_join(towns,space_18,by="city")
ggplot() +
   geom_sf(data = towns_coh[towns_coh$season=="Winter",], aes(fill = countpercap)) +
   scale_fill_viridis_c(option = "plasma")

ggsave("Winter18_countpercap_townmap.jpeg",
       width=14.2,
       height = 6.9,
       dpi = 300,
       units = "in")
#Map all outages in a season through time
change<-read.csv('"Old Outputs/weekly_COH_avg_st_wt_join_animal_abundance.csv')%>%
   mutate(date=mdy(`date_time_out`),
          city=actual_city_town)%>%
   select(-c("actual_city_town","date_time_out","blackbirds","raptors","woodpeckers","urban"))

change_all<-read.csv('allspecies_ER.csv')%>%
   mutate(date=mdy(`date`))%>%
   filter(year>=2013)%>%
   mutate(month=month(date),
          season=ifelse(month%in% c(12,1:5), ifelse(month%in% c(12,1,2),"Winter", "Spring"),
                        ifelse(month%in% c(6:8), "Summer", "Fall")))
# Merge the two datasets: the full dates and outage events
change_all <- merge(change_all, dplyr::select(change, (count:city)), by=c("date","city"), all=T)
change_all[is.na(change_all)] <- 0
change_all<-as.data.frame(merge(change_all,towns[,-5],by="city", all.x = TRUE))

df<-change_all
#Each town for each season 
#group by town and season and summarize
sum<-dplyr::select(df,-c("month","date"))%>% 
   group_by(city,season)%>%
   summarize(st_COH_avg=sum(st_COH_avg,na.rm = TRUE),
             saidi=sum(st_COH_avg,na.rm = TRUE)/mean(POP2010,na.rm = TRUE),
             countpercap=sum(count,na.rm=TRUE)/mean(POP2010,na.rm = TRUE))%>%
   ungroup()


towns_coh<-left_join(towns,sum,by="city")
ggplot() +
   geom_sf(data = towns_coh[towns_coh$season=="Fall",], aes(fill = st_COH_avg)) +
   scale_fill_viridis_c(option = "plasma")

ggsave("Fall13_18_coh_townmap.jpeg",
       width=14.2,
       height = 6.9,
       dpi = 300,
       units = "in")

towns<- st_read("../Raw_Data/MA_Shapefiles/MA_Towns_Shape_Final.shp")%>% 
   dplyr::select(city,cty_twn,TOWN_ID,POP2010,Area_km2)%>%
   st_transform(crs = crs)
cent<- st_read("../Raw_Data/MA_Shapefiles/town_centroids.shp")%>% 
   st_transform(crs = crs)

er<-read.csv("Old Outputs/ER_change_zscores_outages_weeks_ALPHACODES.csv")
er_season<-er%>%group_by(city,year,season)%>%
   summarise(TUVU=mean(TUVU,na.rm=TRUE),
             RTHA=mean(RTHA,na.rm=TRUE),
             OSPR=mean(OSPR,na.rm=TRUE),
             EUST=mean(EUST,na.rm=TRUE),
             HOSP=mean(HOSP,na.rm=TRUE),
             ROPI=mean(ROPI,na.rm=TRUE),
             PIWO=mean(PIWO,na.rm = TRUE),
             RBWO=mean(RBWO,na.rm = TRUE),
             countpercap=mean(countpercap,na.rm=TRUE),
             COH=mean(st_COH_avg,na.rm = TRUE))

towns_coh<-left_join(towns,er_season,by="city")
cent_coh<-left_join(cent,er_season,by="city")%>%
   mutate(TUVU=ifelse(TUVU>-1&TUVU<1, "Stable", ifelse(TUVU<=-1, "Decrease","Increase")),
          RTHA=ifelse(RTHA>-1&RTHA<1, "Stable", ifelse(RTHA<=-1, "Decrease","Increase")),
          OSPR=ifelse(OSPR>-1&OSPR<1, "Stable", ifelse(OSPR<=-1, "Decrease","Increase")),
          EUST=ifelse(EUST>-1&EUST<1, "Stable", ifelse(EUST<=-1, "Decrease","Increase")),
          HOSP=ifelse(HOSP>-1&HOSP<1, "Stable", ifelse(HOSP<=-1, "Decrease","Increase")),
          ROPI=ifelse(ROPI>-1&ROPI<1, "Stable", ifelse(ROPI<=-1, "Decrease","Increase")),
          PIWO=ifelse(PIWO>-1&PIWO<1, "Stable", ifelse(PIWO<=-1, "Decrease","Increase")),
          RBWO=ifelse(RBWO>-1&RBWO<1, "Stable", ifelse(RBWO<=-1, "Decrease","Increase")))
cent_coh<-cent_coh%>%filter(city!="HULL")

ggplot() +
   geom_sf(data = towns_coh[towns_coh$season=="Summer"&towns_coh$year==2018,], aes(fill = COH)) +
   scale_fill_viridis_c(option = "plasma")+
   geom_sf(data=cent_coh[cent_coh$season=="Summer"&cent_coh$year==2018,],aes(color = RBWO, shape=RBWO))+
   scale_shape_manual(values=c(6,17,19))+
   scale_color_manual(values=c("yellow","#66CCFF","white"))+
   labs(fill="Customer Outage\n Hours (COH)",
        color="Red-bellied Woodpecker \nYearly Population Trend",
        shape="Red-bellied Woodpecker \nYearly Population Trend",
        title="MA Towns, Summer 2018")+
   theme(plot.title = element_text(size = 12, face = "bold"),
         legend.title=element_text(size=13, face="bold"), 
         legend.text=element_text(size=12))

ggsave("Summer18_coh_RBWOchange_townmap.jpeg",
       width=14.2,
       height = 6.9,
       dpi = 500,
       units = "in")

#correlations
fall<-read.csv("Old Outputs/Species_PCA_old/pcvalues_ERchange_allspacetime.csv")%>%
   #filter(year==2018)%>%
   select(c(1:4,20:21))%>%
   filter(!(st_COH_avg==0))
jpeg("out_ERchangePC_corr_2018Spring.jpeg", units = "in", width = 7,height=6, res=300)
pairs.panels(fall,
             method = "spearman",
             hist.col = "#00AFBB",
             density=TRUE,
             ellipses = FALSE,
             lm=TRUE)
dev.off()

#Re run correlation separating by town quartiles of outages
season<-read.csv("pcvalues_raw_summer.csv")%>%
   filter(year>=2013)%>%
   select(-c(8:19))
town_quant<-season%>%
   group_by(city)%>%
   summarize(countpercap=100*mean(countpercap,na.rm=TRUE))
town_quant_75<-town_quant%>%
   mutate( countq=CutQ(countpercap, breaks=10, labels = FALSE))%>%
   select(-countpercap)

change_quants<-left_join(season,town_quant_75, by=c("city"))

#correaltions between ER and stavgCOH separated by quartiles of the town total seasonal coh
par(mar=c(5,6,4,1)+.1)
ggplot(filter(change_quants),
       aes(x=Dim.2, y=countpercap)) + geom_point(color='blue',alpha =1/10)+
   geom_smooth(method='lm',color='red')+
   labs(x = "Encounter Rate", y = "Outage Frequency Per Capita")+
   facet_wrap(~countq)

#Grouped correlation
# All correlations by for each cycle level
winter<-read.csv("Old Outputs/Species_PCA_old/Winter/pcvalues_ERchange_winterspacetime.csv")%>%
   filter(year>=2013)%>%
   select(c(1:4,16:22))%>%
   filter(!(st_COH_avg==0))
spring<-read.csv("Old Outputs/Species_PCA_old/Spring/pcvalues_ERchange_springspacetime.csv")%>%
   filter(year>=2013)%>%
   select(c(1:4,16:22))%>%
   filter(!(st_COH_avg==0))
summer<-read.csv("Old Outputs/Species_PCA_old/Summer/pcvalues_ERchange_Summerspacetime.csv")%>%
   filter(year>=2013)%>%
   select(c(1:4,16:22))%>%
   filter(!(st_COH_avg==0))
fall<-read.csv("Old Outputs/Species_PCA_old/Fall/pcvalues_ERchange_fallspacetime.csv")%>%
   filter(year>=2013)%>%
   select(c(1:4,16:22))%>%
   filter(!(st_COH_avg==0))


weeks<-read.csv("Old Outputs/Species_PCA_old/pcvalues_ERchange_allspacetime.csv")%>%
   mutate(date=ymd(date))%>%
   filter(year>=2013)%>%
   select(c(1:12,16:23))%>%
   filter(!(st_COH_avg==0))

require(plyr)
cor1 <- function(df)
{
   return(data.frame(PC.9 = cor(df$Dim.9, df$st_COH_avg,method = "spearman")))
}
cor2 <- function(df)
{
   return(data.frame(PC.10 = cor(df$Dim.10, df$st_COH_avg,method = "spearman")))
}
cor3 <- function(df)
{
   return(data.frame(PC.11 = cor(df$Dim.11, df$st_COH_avg,method = "spearman")))
}
cor4 <- function(df)
{
   return(data.frame(PC.12 = cor(df$Dim.12, df$st_COH_avg,method = "spearman")))
}
seasons_1_count<-ddply(weeks, .(season,year), cor1)#use both (season,year) for all space time points
seasons_2_count<-ddply(weeks, .(season,year), cor2)
seasons_3_count<-ddply(weeks, .(season,year), cor3)
seasons_4_count<-ddply(weeks, .(season,year), cor4)
seasons_cors<-merge(seasons_1_count,seasons_2_count,by=c("year","season"))%>%
   merge(seasons_3_count,by=c("year","season"))%>%merge(seasons_4_count,by=c("year","season"))%>%
   gather(cor, value,3:6)%>%#3:6 for all space time points, 2:5 for seasons
   mutate(month=ifelse(season=="Winter",1,
                       ifelse(season=="Spring",4,
                              ifelse(season=="Summer",8,
                                     ifelse(season=="Fall",10,season)))),
          date=dmy(paste0("1-",month,"-",year)))

ggplot(data=seasons_cors,aes(x=date, y=value,group=cor)) + #date instead of year for all space time
   geom_line(aes(color=cor))+
   labs(y="Spearman's Cor. Coeff.",
        x="Year",
        color="Annual Bird Trend\nPrincipal \nComponents (PCs)")+
   theme(text = element_text(size=18,face = "bold"))
   
ggsave("weekly_ER_change_pc9_12_coh_cor13_18.jpeg",
       width=14.2,
       height = 6.9,
       dpi = 300,
       units = "in")

#What's happening with raptors and outages in 2017-2018?
#Raptor change over time
change<-read.csv("Old Outputs/ER_change_zscores_outages_weeks_ALPHACODES.csv")
change_raptors<-change%>%
   dplyr::select(DOWO,HAWO,PIWO,RBWO,date,city)%>%
   gather(species,change,c("DOWO","HAWO","PIWO","RBWO"))%>%
   group_by(date,species)%>%
   summarise(change=mean(change,na.rm=TRUE))
tmin<-change%>%
   dplyr::select(tmin,date,city)%>%
   group_by(date)%>%
   summarise(tmin=mean(tmin,na.rm=TRUE))%>%
   mutate(week=week(ymd(date)),
          year=year(ymd(date)),
          month=month(ymd(date)))
tmin_anom<-tmin%>%group_by(month)%>%
      mutate(anomaly= tmin-mean(tmin,na.rm=TRUE),
             st_anomaly= anomaly/sd(tmin,na.rm=TRUE))
change_out<-change%>%
   dplyr::select(st_COH_avg,countpercap,date,city)%>%
   gather(outage_metric,value,c("st_COH_avg","countpercap"))%>%
   group_by(date,outage_metric)%>%
   summarise(value=mean(value,na.rm=TRUE))

ggplot(change_raptors, aes(x=ymd(date),y=change,group=species))+
   geom_line(aes(color=species))+
   labs(x="Date",y="Yearly Change Z-score")


ggsave("change_woodpeckers.jpeg",
       width=16,
       height = 7,
       dpi = 300,
       units = "in")

ggplot(filter(change_out,outage_metric=="st_COH_avg"), aes(x=ymd(date),y=value))+
   geom_line()+
   labs(x="Date",y="Customer Outage Hours")

ggsave("change_stCOHavg.jpeg",
       width=16,
       height = 7,
       dpi = 300,
       units = "in")

ggplot(filter(change_out,outage_metric=="countpercap"), aes(x=ymd(date),y=value))+
   geom_line()+
   labs(x="Date",y="Outage Frequency Per Capita")


ggsave("change_countpercap.jpeg",
       width=16,
       height = 7,
       dpi = 300,
       units = "in")

library(ggplot2)
library(rgdal)
library(lubridate)
library(sf)
library(gridExtra)
library(tidyverse)
library(dplyr)
library(ggpubr)
library(stringr)
library(factoextra)
library(ggfortify)
library(psych)
library(DescTools)

memory.limit(100000)

#Load data
load("../Objects and Data/5_PCA.rda")


#------------
#####PCAs####
#------------
#bird species dimension reduction
#----------------------------------
#Maybe week misalignment? Bird data missing week 53 in some years. Remove dates with NAs
df<-dp_outs_towns[complete.cases(dp_outs_towns), ]  

#Classify towns by quartiles of total (summed across years) outage frequency
  #Sum outage frequency across time for each town
  perc<-df%>%
    select(actual_city_town,ncom,week,year)%>%
  #rank towns by percentiles of outage freq
    mutate(ncom_rank =as.integer(cut(ncom, breaks=quantile(ncom, probs=0:6/6),
                                     include.lowest = T))
           )%>%
    select(-ncom)

#add town outage ranks to dataset
m<-left_join(df,perc, by=c("actual_city_town","week","year"))


#Look at individual seasons, as well as all time points together.
#Does the relationship between outage and bird distributions change with seasonal bird activity?
pr.all<-m
pr.winter<-filter(m, season== "winter")
pr.spring<-filter(m, season== "spring")
pr.summer<-filter(m, season== "summer")
pr.fall<-filter(m, season== "fall")


#Run PCAs
#select just species DPs
#PCA outputs, create arranged outputs for plotting categorizing variables

#All space time
#arranged by total seasonal outages per town
pr.all.outages<-arrange(pr.all,ncom)
pr.out.all.outages=prcomp(pr.all.outages[,c(16:29)] , scale=TRUE)


#Winter 
#arranged by outage percentile
pr.winter.outages<-arrange(pr.winter,ncom)
pr.out.winter.outages=prcomp(pr.winter.outages[,c(16:29)] , scale=TRUE)


#Spring 
#arranged by outage percentile
pr.spring.outages<-arrange(pr.spring,ncom)
pr.out.spring.outages=prcomp(pr.spring.outages[,c(16:29)] , scale=TRUE)


#Summer 
#arranged by outage percentile
pr.summer.outages<-arrange(pr.summer,ncom)
pr.out.summer.outages=prcomp(pr.summer.outages[,c(16:29)] , scale=TRUE)


#Fall 
#arranged by outage percentile
pr.fall.outages<-arrange(pr.fall,ncom)
pr.out.fall.outages=prcomp(pr.fall.outages[,c(16:29)] , scale=TRUE)


#Plot week/town coordinates (PC1&2 and PC3&4).
#Show coordinates categorized by outages and time to examine 
#where outages are occurring in relation to changes in bird distributions.

#Show where town-week points,ranked by NCOH percentiles,fall within bird distributions.

p1<-fviz_pca_biplot(pr.out.all.outages,
                    #Biplot of PC1 vs PC2
                    axes = c(1,2), 
                    label = c("var"),
                    col.var = "black",
                    fill.var = "black",
                    arrowsize=0.8, labelsize=4, repel = TRUE,
                    #col.ind = pr.all.outages$ncom,
                    col.ind=as.factor(pr.all.outages$ncom_rank),
                    pointshape=16,pointsize=1,
                    #palette = "viridis",
                    palette =c("#e8fa5bff","#f7cb44ff","#eb8055ff","#a65c85ff","#7e4e90ff","#0c2a50ff"),#"orang/purp#a65c85ff","darkorg#eb8055ff","darkyllw#f7cb44ff",navy"#13306dff","purple#7e4e90ff","light orange#f9b641ff")
                    legend.title="SAIDI Quantile",title="",
                    ggtheme = theme_minimal())

p1.var<-fviz_pca_var(pr.out.all.outages,
                     axes = c(1,2),
                     labelsize=5,
                     col.var = "contrib", # Color by contributions to the PC
                     gradient.cols = c("#13306dff","#7e4e90ff","#f9b641ff"),
                     title= "",
                     repel = TRUE    # Avoid text overlapping
)



p2<-fviz_pca_biplot(pr.out.all.outages,
                    #Biplot of PC3 vs PC4
                    axes = c(3,4), 
                    label = c("var"),
                    col.var = "black",
                    fill.var = "black",
                    arrowsize=0.8, labelsize=4, repel = TRUE,
                    col.ind = as.factor(pr.all.outages$ncom_rank), 
                    pointshape=16,pointsize=1,
                    palette =c("#e8fa5bff","#f7cb44ff","#eb8055ff","#a65c85ff","#7e4e90ff","#0c2a50ff"),
                    legend.title="Weekly Town \nNCOH Quantiles",title="",
                    ggtheme = theme_minimal())


#arrange the two biplots side by side
ggarrange(p1,p1.var, ncol=2, nrow=1,legend="top")
#write plots to file
ggsave("../Outputs/PCA/PCA_all_NCOH_quantiles.jpeg",
       width=11,
       height = 7,
       dpi = 300,
       units = "in")



#Make same plots just using seasonal data

#Replace "winter" with season of choice
p1<-fviz_pca_biplot(pr.out.winter.outages,
                    #Biplot of PC1 vs PC2
                    axes = c(1,2), 
                    label = c("var"),
                    col.var = "black",
                    fill.var = "black",
                    arrowsize=0.8, labelsize=4, repel = TRUE,
                    col.ind = as.factor(pr.winter.outages$ncom_rank), 
                    pointshape=16,pointsize=1,
                    palette = c("#e8fa5bff","#f7cb44ff","#eb8055ff","#a65c85ff","#7e4e90ff","#0c2a50ff"),
                    legend.title="Winter Weekly Town\nNCOM Quantiles",title="",
                    ggtheme = theme_minimal())


p2<-fviz_pca_biplot(pr.out.winter.outages,
                    #Biplot of PC3 vs PC4
                    axes = c(3,4), 
                    label = c("var"),
                    col.var = "black",
                    fill.var = "black",
                    arrowsize=0.8, labelsize=4, repel = TRUE,
                    col.ind = as.factor(pr.winter.outages$ncom_rank), 
                    pointshape=16,pointsize=1,
                    palette = c("#e8fa5bff","#f7cb44ff","#eb8055ff","#a65c85ff","#7e4e90ff","#0c2a50ff"),
                    legend.title="Winter Weekly Town\nRNCOM Quantiles",title="",
                    ggtheme = theme_minimal())


#arrange the two biplots side by side
ggarrange(p1,p2, ncol=2, nrow=1,legend="top",common.legend = T)
#write plots to file
ggsave("../Outputs/PCA/PCA_winter_NCOH_quartiles.jpeg",
       width=11,
       height = 7,
       dpi = 300,
       units = "in")


#***PC loading vectors***
pr.out.all.outages$rotation%>%write.csv("../Outputs/PCA/allspacetime_pcloadings.csv")
pr.out.winter.outages$rotation%>%write.csv("../Outputs/PCA/winter_pcloadings.csv")
pr.out.spring.outages$rotation%>%write.csv("../Outputs/PCA/spring_pcloadings.csv")
pr.out.summer.outages$rotation%>%write.csv("../Outputs/PCA/summer_pcloadings.csv")
pr.out.fall.outages$rotation%>%write.csv("../Outputs/PCA/fall_pcloadings.csv")


#plot first 2 pcs (variables). Positive corr variables point to same side, negative corr variables point to opposite sides
p1.var<-fviz_pca_var(pr.out.spring.outages,
                 axes = c(1,2),
                 labelsize=5,
                 col.var = "contrib", # Color by contributions to the PC
                 gradient.cols = c("#13306dff","#7e4e90ff","#f9b641ff"),
                 repel = TRUE    # Avoid text overlapping
)


p2<-fviz_pca_var(pr.out.spring.outages,
                 axes = c(3,4),
                 labelsize=5,
                 col.var = "contrib", # Color by contributions to the PC
                 gradient.cols = c("#13306dff","#7e4e90ff","#f9b641ff"),
                 repel = TRUE     # Avoid text overlapping
)

#visual of PC loadings
ggarrange(p1,p2, ncol=2, nrow=1,legend="bottom")
ggsave("../Outputs/PCA/spring_var_plot_pc1_4.jpeg",
       width=11,
       height = 7,
       dpi = 300,
       units = "in")



#Percent contribution of variance of PCAs for each season
perc.winter <- (pr.out.winter.outages$sdev^2/sum(pr.out.winter.outages$sdev^2))
perc.spring <- (pr.out.spring.outages$sdev^2/sum(pr.out.spring.outages$sdev^2))
perc.summer <- (pr.out.summer.outages$sdev^2/sum(pr.out.summer.outages$sdev^2))
perc.fall <- (pr.out.all.outages$sdev^2/sum(pr.out.all.outages$sdev^2))

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

#Visual of Cumulative Variance across PCs
ggplot(df2)+
  geom_bar(aes(x = season, y = perc, fill = pc),
           stat = "identity")+
  labs(y="Proportion of Variance",
       x="Season",
       fill="PC") +
  scale_fill_viridis_d(option = "plasma")




#Extract PC variables
pr.ind <- get_pca_ind(pr.out.all.outages)
coord<-merge(pr.all[,c("actual_city_town","year","season","week","ncom","date","ncom_rank")],pr.ind$coord,by=0,all.x=TRUE)%>% # Coordinates
  select(-c("Row.names"))
write.csv(coord, "../Outputs/PCA/pcvalues_all.csv",row.names = FALSE)




#map PC values on MA towns

#merge PC values for each year to town shapefile
cent<-st_read("../Objects and Data/Mass_towns/town_cent_namescorrected.shp")%>%
  st_set_crs(st_crs(ma_towns))


coord2<-coord%>%
  group_by(actual_city_town)%>%
  summarise(Dim.1=mean(Dim.1,na.rm=T),
            Dim.2=mean(Dim.2,na.rm = T),
            NCOH=mean(ncoh,na.rm = T))%>%
  ungroup()

towns_pc<-left_join(ma_towns,coord2,by=c("city"="actual_city_town"))

#merge to town centroids
#change bird abundance to a categorical variable for plotting
towns_pc_nogeom<-st_drop_geometry(towns_pc)
cent_var<-right_join(cent,towns_pc_nogeom,by="city")%>%
  mutate(NCOH=ifelse(NCOH>quantile(coord$ncoh,0.9), "Extreme Outage",NA))
cent_var<-cent_var%>%filter(city!="HULL")

#map
ggplot() +
  #specify Bird PC
  geom_sf(data = towns_pc, aes(fill = Dim.1)) +
  scale_fill_viridis_c(option = "plasma")+
  #outage measure
  geom_sf(data=cent_var[!is.na(cent_var$NCOH),],aes(color = NCOH))+
  scale_shape_manual(values=19)+
  scale_color_manual(values = "white")+
  labs(fill="Mean Bird PC1",
       color="Mean NCOH")+
  theme_classic(base_size = 12)


ggsave("../Outputs/PCA/PC1_extremeoutages_alltime_townmap.jpeg",
       width=8,
       height = 7,
       dpi = 300,
       units = "in")



#Not updated yet
#--------------------------------------------------------------------------------------

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
coord<-read.csv("all space time/ER change/pcvalues_ERchange_allspacetime.csv")
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
change<-read.csv('C:/Users/emfeng/OneDrive - Lincoln Park Zoo/MA Power Data/outage_species_spatial_join/weekly_COH_avg_st_wt_join_animal_abundance.csv')%>%
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

towns<- st_read("C:/Users/emfeng/OneDrive - Lincoln Park Zoo/PRISM DL Data/ebird raw data/MA_Towns_Shape_Final.shp")%>% 
  dplyr::select(city,cty_twn,TOWN_ID,POP2010,Area_km2)%>%
  st_transform(crs = crs)
cent<- st_read("C:/Users/emfeng/OneDrive - Lincoln Park Zoo/MA Power Data/outage_species_spatial_join/PCA/town_centroids.shp")%>% 
  st_transform(crs = crs)

er<-read.csv("C:/Users/emfeng/OneDrive - Lincoln Park Zoo/MA Power Data/outage_species_spatial_join/PCA/ER_change_zscores_outages_weeks_ALPHACODES.csv")
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
fall<-read.csv("all space time/ER change/pcvalues_ERchange_allspacetime.csv")%>%
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
winter<-read.csv("pcvalues_ERchange_winterspacetime.csv")%>%
  filter(year>=2013)%>%
  select(c(1:4,16:22))%>%
  filter(!(st_COH_avg==0))
spring<-read.csv("pcvalues_ERchange_springspacetime.csv")%>%
  filter(year>=2013)%>%
  select(c(1:4,16:22))%>%
  filter(!(st_COH_avg==0))
summer<-read.csv("pcvalues_ERchange_Summerspacetime.csv")%>%
  filter(year>=2013)%>%
  select(c(1:4,16:22))%>%
  filter(!(st_COH_avg==0))
fall<-read.csv("pcvalues_ERchange_fallspacetime.csv")%>%
  filter(year>=2013)%>%
  select(c(1:4,16:22))%>%
  filter(!(st_COH_avg==0))


weeks<-read.csv("all space time/ER change/pcvalues_ERchange_allspacetime.csv")%>%
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
setwd('C:/Users/emfeng/OneDrive - Lincoln Park Zoo/MA Power Data/outage_species_spatial_join/PCA')
#Raptor change over time
change<-read.csv("ER_change_zscores_outages_weeks_ALPHACODES.csv")
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


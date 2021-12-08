
#-----------------------------------------------
#### quantile regression
#-----------------------------------------------

library(quantreg)
library(tidyverse)
library(lubridate)
library(gridExtra)
library(ggpubr)
library(sf)
library(ggplot2)
library(DescTools)
select <- dplyr::select

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

#Load outage data
outages<-read.csv("../Raw_Data/MA_SAIDI.csv")
outages$geo_id<-as.factor(outages$geo_id)
outages$Date.Out<-ymd(as.Date(outages$Date.Out))

#Load towns
crs <- st_crs(4326)
towns<- st_read("../Raw_Data/MA_Shapefiles/MA_Towns_Shape_Final.shp")%>% 
  select(city, TOWN_ID,Area_km2,geo_id)%>%
  st_transform(crs = crs)

#pot_animal_outages<- outages[outages$Reason.For.Outage %in% c("Tree Contact","Animal","Unknown","Physical Interference/Other") | 
#                               (outages$Reason.For.Outage == "Failed Equipment"& outages$Failed.or.Damaged.Equipment=="Pole - wood"),]%>%
#  filter(!is.na(Reason.For.Outage))%>%
#  mutate(week=week(Date.Out),year=year(Date.Out))%>%
#  left_join(towns, by = "geo_id")
#outages with potential animal related causes=86,282

animal_outages<- outages[outages$Reason.For.Outage=="Animal",]%>%
  filter(!is.na(Reason.For.Outage))%>%
  mutate(week=week(Date.Out), year=year(Date.Out), month=month(Date.Out),
         season= ifelse(month%in% c(4:9), "Summer","Winter"))%>%
  left_join(towns, by = "geo_id")
#outages with documented animal causes= 14,563


#outage metrics
animal_outages2<-animal_outages%>%
  group_by(year,week)%>%
  mutate(weekly_outages=n(),
         weekly_coh=sum(Actual.Duration)*sum(Original.Number.Customers.Affected))%>%
  ungroup%>%
  group_by(city,year,week)%>%
  summarise(num_outages= n(),
            prop_outages=num_outages/(mean(weekly_outages)),
            outages_weight=num_outages*prop_outages,
            coh= sum(Actual.Duration)*sum(Original.Number.Customers.Affected),
            prop_coh=coh/(mean(weekly_coh)),
            coh_weight=coh*prop_coh
  )%>%
  select(-c("num_outages","prop_outages","coh","prop_coh"))



#Combine into species groups
#blackbirds (red-winged, cowbird, grackle)
sp1 <-read.csv(paste0("Old Outputs/Species_DP_Old/",name_list$sp_alpha[1],"_weekly_changes_05_18.csv"))%>%
  select(avg_encount_rate, city, observation_date)
sp2 <-read.csv(paste0("Old Outputs/Species_DP_Old/",name_list$sp_alpha[2],"_weekly_changes_05_18.csv"))%>%
  select(avg_encount_rate, city, observation_date)
sp3 <-read.csv(paste0("Old Outputs/Species_DP_Old/",name_list$sp_alpha[3],"_weekly_changes_05_18.csv"))%>%
  select(avg_encount_rate, city, observation_date)
m<-merge(sp1,sp2,by = c("city","observation_date"))%>%
  merge(sp3,by = c("city","observation_date"))%>%
  mutate(blackbird_er=(avg_encount_rate.x+avg_encount_rate.y+avg_encount_rate)-(avg_encount_rate.x*avg_encount_rate.y*avg_encount_rate))
#write.csv(m,""Old Outputs/Species_DP_Old/blackbird_weekly_ER_05_18.csv")

#raptors (red-tailed, osprey, vulture)
sp4 <-read.csv(paste0("Old Outputs/Species_DP_Old/",name_list$sp_alpha[4],"_weekly_changes_05_18.csv"))%>%
  select(avg_encount_rate, city, observation_date)
sp5 <-read.csv(paste0("Old Outputs/Species_DP_Old/",name_list$sp_alpha[5],"_weekly_changes_05_18.csv"))%>%
  select(avg_encount_rate, city, observation_date)
sp6 <-read.csv(paste0("Old Outputs/Species_DP_Old/",name_list$sp_alpha[6],"_weekly_changes_05_18.csv"))%>%
  select(avg_encount_rate, city, observation_date)
m<-merge(sp4,sp5,by = c("city","observation_date"))%>%
  merge(sp6,by = c("city","observation_date"))%>%
  mutate(raptor_er=(avg_encount_rate.x+avg_encount_rate.y+avg_encount_rate)-(avg_encount_rate.x*avg_encount_rate.y*avg_encount_rate))
#write.csv(m,"Old Outputs/Species_DP_Old/raptor_weekly_ER_05_18.csv")

##woodpeckers (red-bellied, flicker, downy, hairy, pileated)
sp7 <-read.csv(paste0("Old Outputs/Species_DP_Old/",name_list$sp_alpha[7],"_weekly_changes_05_18.csv"))%>%
  select(avg_encount_rate, city, observation_date)
sp8 <-read.csv(paste0("Old Outputs/Species_DP_Old/",name_list$sp_alpha[8],"_weekly_changes_05_18.csv"))%>%
  select(avg_encount_rate, city, observation_date)
sp9 <-read.csv(paste0("Old Outputs/Species_DP_Old/",name_list$sp_alpha[9],"_weekly_changes_05_18.csv"))%>%
  select(avg_encount_rate, city, observation_date)
sp10 <-read.csv(paste0("Old Outputs/Species_DP_Old/",name_list$sp_alpha[10],"_weekly_changes_05_18.csv"))%>%
  select(avg_encount_rate, city, observation_date)
m<-merge(sp7,sp8,by = c("city","observation_date"))%>%
  rename(avg_encount_rate.w=avg_encount_rate.x,avg_encount_rate.z=avg_encount_rate.y)%>%
  merge(sp9,by = c("city","observation_date"))%>%
  merge(sp10,by = c("city","observation_date"))%>%
  mutate(woodpecker_er=(avg_encount_rate.w+avg_encount_rate.x+avg_encount_rate.y+avg_encount_rate.z)-(avg_encount_rate.w*avg_encount_rate.x*avg_encount_rate.y*avg_encount_rate.z))
#write.csv(m,"Old Outputs/Species_DP_Old/woodpecker_weekly_ER_05_18.csv")

##urban (sparrow, starling, pigeon, mourning dove)
sp11 <-read.csv(paste0("Old Outputs/Species_DP_Old/",name_list$sp_alpha[11],"_weekly_changes_05_18.csv"))%>%
  select(avg_encount_rate, city, observation_date)
sp12 <-read.csv(paste0("Old Outputs/Species_DP_Old/",name_list$sp_alpha[12],"_weekly_changes_05_18.csv"))%>%
  select(avg_encount_rate, city, observation_date)
sp13 <-read.csv(paste0("Old Outputs/Species_DP_Old/",name_list$sp_alpha[13],"_weekly_changes_05_18.csv"))%>%
  select(avg_encount_rate, city, observation_date)
sp14 <-read.csv(paste0("Old Outputs/Species_DP_Old/",name_list$sp_alpha[14],"_weekly_changes_05_18.csv"))%>%
  select(avg_encount_rate, city, observation_date)
m<-merge(sp11,sp12,by = c("city","observation_date"))%>%
  rename(avg_encount_rate.w=avg_encount_rate.x,avg_encount_rate.z=avg_encount_rate.y)%>%
  merge(sp13,by = c("city","observation_date"))%>%
  merge(sp14,by = c("city","observation_date"))%>%
  mutate(urban_er=(avg_encount_rate.w+avg_encount_rate.x+avg_encount_rate.y+avg_encount_rate.z)-(avg_encount_rate.w*avg_encount_rate.x*avg_encount_rate.y*avg_encount_rate.z))
#write.csv(m,"Old Outputs/Species_DP_Old/urban_weekly_ER_05_18.csv")

group_list<-list(
  g_file=c("blackbird", "raptor","woodpecker","urban"),
  g_name=c("Blackbird", "Raptor","Woodpecker","Urban")
)

#for (i in 1:length(name_list$sp_name)) {
  species <-read.csv(paste0("Old Outputs/Species_DP_Old/",name_list$sp_alpha[1],"_weekly_changes_05_18.csv"))
 
  out_bird<-species%>%
    mutate(observation_date=ymd(observation_date),week=week(observation_date), month=month(observation_date),
           season= ifelse(month%in% c(4:9), "Summer","Winter"))
  out_bird<-merge(out_bird, animal_outages2, by = c("city", "year", "week"),all= TRUE)
  out_bird<-filter(out_bird,city %in% animal_outages2$city)%>%
    group_by(city)%>%
    arrange(observation_date, .by_group=TRUE)%>%
    mutate(out_weight_diff = outages_weight-lag(outages_weight, n=53) ,
           out_weight_prop_diff = out_weight_diff/lag(outages_weight, n=53),
           coh_weight_diff= coh_weight-lag(coh_weight, n=53),
           coh_weight_prop_diff=coh_weight_diff/lag(coh_weight, n=53))%>%
    filter(year>=2013)%>%
  ungroup()
  

COH <- out_bird$coh_weight    # log(y) or y
ER <- out_bird$avg_encount_rate     # center the years, x-mean(x)
Outages <- out_bird$outages_weight   
Change.COH <- out_bird$coh_weight_diff   
Change.ER <- out_bird$diff 
Change.Outages <- out_bird$out_weight_diff   


#jpeg("hist_ER.jpeg")  
hist(ER, main=paste0(name_list$sp_name[1]," Encounter Rate"), xlab= "Encounter Rate")
#dev.off()
#summary(lm(yvar ~ xvar))

#summary(rq(formula = yvar ~ xvar, tau = 0.5))

#plot(xvar, yvar, cex=.25, type="n", xlab="Encounter Rate", ylab="Proportion of Outages")
#  points(xvar, yvar, cex=.5, col="gray")
#  abline(rq(yvar ~ xvar, tau=.5), col="blue")
#  abline(lm(yvar ~ xvar), lty=2, col="red")  # the dreaded ols line

#taus <- c(.05,.1,.25,.75,.90,.95)
#for ( i in 1:length(taus)) {
#  abline(rq(yvar ~ xvar, tau=taus[i]), col="darkgray")
#}


#use multiple predictor variables, include season (yvar~xvar+zvar), days+days^2
#Look at towns with highest species abundance, break into quantiles
  #split out_bird into subsets of towns from above

## create graph of intercept and slopes by quantile
#Correlation of the proportion of COH/Outages/change in COH/Outages to ER/change in ER over ER/change ER quantiles
#jpeg("quantile_regression_rawcoh.jpeg")
fit1 <- summary(rq(COH ~ ER, tau=2:98/100)) 
p1<-plot(fit1, mfrow = c(1,2))
#dev.off()
#jpeg("quantile_regression_rawout.jpeg")
fit2 <- summary(rq(Outages ~ ER, tau=2:98/100))
p2<-plot(fit2, mfrow = c(1,2))
#dev.off()
#jpeg("quantile_regression_changecoh.jpeg")
fit3 <- summary(rq(Change.COH ~ Change.ER, tau=2:98/100))
p3<-plot(fit3, mfrow = c(1,2))
#dev.off()
#jpeg("quantile_regression_changeout.jpeg") #
fit4 <- summary(rq(Change.Outages ~ Change.ER, tau=2:98/100))
p4<-plot(fit4, mfrow = c(1,2))
#dev.off()


#-----------------------------------------------
## Plot Time Series of Outages and ER
#-----------------------------------------------
#first compare time series of outages against ER anomalies and proportional change

dat4<- out_bird%>%group_by(observation_date)%>%
  summarise(ER.Anomaly = mean(st_anomaly,na.rm = TRUE),
            ER.Change = mean(diff,na.rm = TRUE),
            Number.Outages = mean(outages_weight,na.rm = TRUE),
            COH = mean(coh_weight,na.rm = TRUE)) %>% 
  gather(measure, value, ER.Anomaly:COH)     ## stack quantile values into 1 column for plotting


ggplot(data = dat4, aes(x=observation_date, y=value)) +
  geom_line() +
  xlab("") +
  ylab("Town Averages") + 
  facet_grid(measure ~ ., scales='free')
#ggsave(ERchange_outages_avgplots.jpeg"),
#       width=12.5,
#       height = 9.375,
#       dpi = 96,
#       units = "in")


##State averaged (across towns) Time Series, how do they change based on monthly/yearly/weekly smooths?

#Time series of proportional annual change in ER by quantiles (biggest changes vs smallest changes)

#Time series of Average Annual ER by Winter vs Summer seasons
dat4 <- out_bird%>%group_by(year)%>%
  filter(week==3)%>%
  summarise(ER = mean(avg_encount_rate,na.rm = TRUE), 
            Outages = mean(outages_weight,na.rm = TRUE),
            COH = mean(coh_weight,na.rm = TRUE),
  ) %>% 
  gather(quant, value, ER:COH)     ## stack quantile values into 1 column for plotting


ggplot(data = dat4, aes(x=year, y=value)) +
  geom_line() +
  xlab("") + 
  ylab("January Town Average")+
  facet_grid(quant ~ ., scales='free')

#ggsave("_ER_out_Jan_avgplots.jpeg"),
#       width=12.5,
#       height = 9.375,
#       dpi = 96,
#       units = "in")

dat4 <- out_bird%>%group_by(year)%>%
  filter(week==26)%>%
  summarise(ER = mean(avg_encount_rate,na.rm = TRUE), 
            Outages = mean(outages_weight,na.rm = TRUE),
            COH = mean(coh_weight,na.rm = TRUE),
  ) %>% 
  gather(quant, value, ER:COH)     ## stack quantile values into 1 column for plotting


ggplot(data = dat4, aes(x=year, y=value)) +
  geom_line() +
  xlab("") + 
  ylab("June Town Average")+
  facet_grid(quant ~ ., scales='free')

#ggsave("_ER_out_June_avgplots.jpeg"),
#       width=12.5,
#       height = 9.375,
#       dpi = 96,
#       units = "in")


#Re run analyses only for towns within the highest quantile of outages and bird occurrence in the winter or summer
town_quant<-out_bird%>%
  group_by(city,season)%>%
  summarize(ER=mean(avg_encount_rate),
            COH=sum(coh_weight,na.rm=TRUE),
            out=sum(outages_weight,na.rm = TRUE))
town_quant_75<-town_quant%>%
  group_by(season)%>%
  mutate(ERquant=CutQ(ER),
         COHquant=CutQ(COH),
         Outquant=CutQ(out))%>%
  filter(ERquant=="Q4" & COHquant=="Q4" & Outquant=="Q4")
town_quant_25<-town_quant%>%
  group_by(season)%>%
  mutate(ERquant=CutQ(ER),
         COHquant=CutQ(COH),
         Outquant=CutQ(out))%>%
  filter(ERquant=="Q1" & COHquant=="Q1" & Outquant=="Q1")

out_bird_75<-out_bird%>%
  filter(city %in% town_quant_75$city)
out_bird_25<-out_bird%>%
  filter(city %in% town_quant_25$city)


#-----------------------------------------------
## Plot Time Series of Outages and ER
#-----------------------------------------------
#first compare time series of outages against ER anomalies and proportional change

dat4<- out_bird_25%>%group_by(observation_date)%>%
  summarise(ER.Anomaly = mean(st_anomaly,na.rm = TRUE),
            ER.Change = mean(diff,na.rm = TRUE),
            Number.Outages = mean(outages_weight,na.rm = TRUE),
            COH = mean(coh_weight,na.rm = TRUE)) %>% 
  gather(measure, value, ER.Anomaly:COH)     ## stack quantile values into 1 column for plotting


ggplot(data = dat4, aes(x=observation_date, y=value)) +
  geom_line() +
  xlab("") +
  ylab("Town Averages") + 
  facet_grid(measure ~ ., scales='free')
#ggsave("ERchange_outages_avgplots25.jpeg",
#       width=12.5,
#       height = 9.375,
#       dpi = 96,
#       units = "in")

dat4<- out_bird_75%>%group_by(observation_date)%>%
  summarise(ER.Anomaly = mean(st_anomaly,na.rm = TRUE),
            ER.Change = mean(diff,na.rm = TRUE),
            Number.Outages = mean(outages_weight,na.rm = TRUE),
            COH = mean(coh_weight,na.rm = TRUE)) %>% 
  gather(measure, value, ER.Anomaly:COH)     ## stack quantile values into 1 column for plotting


ggplot(data = dat4, aes(x=observation_date, y=value)) +
  geom_line() +
  xlab("") +
  ylab("Town Averages") + 
  facet_grid(measure ~ ., scales='free')
#ggsave("ERchange_outages_avgplots75.jpeg",
#       width=12.5,
#       height = 9.375,
#       dpi = 96,
#       units = "in")


##State averaged (across towns) Time Series, how do they change based on monthly/yearly/weekly smooths?

#Time series of proportional annual change in ER by quantiles (biggest changes vs smallest changes)

#Time series of Average Annual ER by Winter vs Summer seasons
dat4 <- out_bird_25%>%group_by(year)%>%
  filter(week==3)%>%
  summarise(ER = mean(avg_encount_rate,na.rm = TRUE), 
            Outages = mean(outages_weight,na.rm = TRUE),
            COH = mean(coh_weight,na.rm = TRUE),
  ) %>% 
  gather(quant, value, ER:COH)     ## stack quantile values into 1 column for plotting


ggplot(data = dat4, aes(x=year, y=value)) +
  geom_line() +
  xlab("") + 
  ylab("January Town Average")+
  facet_grid(quant ~ ., scales='free')

#ggsave("_ER_out_Jan_avgplots25.jpeg",
#       width=12.5,
#       height = 9.375,
#       dpi = 96,
#       units = "in")

dat4 <- out_bird_75%>%group_by(year)%>%
  filter(week==3)%>%
  summarise(ER = mean(avg_encount_rate,na.rm = TRUE), 
            Outages = mean(outages_weight,na.rm = TRUE),
            COH = mean(coh_weight,na.rm = TRUE),
  ) %>% 
  gather(quant, value, ER:COH)     ## stack quantile values into 1 column for plotting


ggplot(data = dat4, aes(x=year, y=value)) +
  geom_line() +
  xlab("") + 
  ylab("January Town Average")+
  facet_grid(quant ~ ., scales='free')

#ggsave("_ER_out_Jan_avgplots75.jpeg",
#       width=12.5,
#       height = 9.375,
#       dpi = 96,
#       units = "in")

dat4 <- out_bird_25%>%group_by(year)%>%
  filter(week==26)%>%
  summarise(ER = mean(avg_encount_rate,na.rm = TRUE), 
            Outages = mean(outages_weight,na.rm = TRUE),
            COH = mean(coh_weight,na.rm = TRUE),
  ) %>% 
  gather(quant, value, ER:COH)     ## stack quantile values into 1 column for plotting


ggplot(data = dat4, aes(x=year, y=value)) +
  geom_line() +
  xlab("") + 
  ylab("June Town Average")+
  facet_grid(quant ~ ., scales='free')

#ggsave("_ER_out_June_avgplots25.jpeg",
#       width=12.5,
#       height = 9.375,
#       dpi = 96,
#       units = "in")


dat4 <- out_bird_75%>%group_by(year)%>%
  filter(week==26)%>%
  summarise(ER = mean(avg_encount_rate,na.rm = TRUE), 
            Outages = mean(outages_weight,na.rm = TRUE),
            COH = mean(coh_weight,na.rm = TRUE),
  ) %>% 
  gather(quant, value, ER:COH)     ## stack quantile values into 1 column for plotting


ggplot(data = dat4, aes(x=year, y=value)) +
  geom_line() +
  xlab("") + 
  ylab("June Town Average")+
  facet_grid(quant ~ ., scales='free')

#ggsave("_ER_out_June_avgplots75.jpeg",
#       width=12.5,
#       height = 9.375,
#       dpi = 96,
#       units = "in")

#Identify outliers
encount_out <- boxplot(out_bird_75$avg_encount_rate, plot=FALSE)$out
prop_diff_out <- boxplot(out_bird_75$diff, plot=FALSE)$out
coh_out <- boxplot(out_bird_75$coh_weight, plot=FALSE)$out
outages_out <- boxplot(out_bird_75$outages_weight, plot=FALSE)$out
anom_out <- boxplot(out_bird_75$st_anomaly, plot=FALSE)$out


out_bird_75_clean<-out_bird_75[-which(out_bird_75$avg_encount_rate %in% encount_out|
                                  out_bird_75$coh_weight %in% coh_out|
                                  out_bird_75$diff %in% prop_diff_out|
                                  out_bird_75$outages_weight %in% outages_out|
                                  out_bird_75$st_anomaly %in% anom_out),]%>%
  ungroup()

encount_out <- boxplot(out_bird_25$avg_encount_rate, plot=FALSE)$out
diff_out <- boxplot(out_bird_25$diff, plot=FALSE)$out
coh_weight_out <- boxplot(out_bird_25$coh_weight, plot=FALSE)$out
outages_out <- boxplot(out_bird_25$outages_weight, plot=FALSE)$out
anom_out <- boxplot(out_bird_25$st_anomaly, plot=FALSE)$out


out_bird_25_clean<-out_bird_25[-which(out_bird_25$avg_encount_rate %in% encount_out|
                                        out_bird_25$coh_weight %in% coh_weight_out|
                                        out_bird_25$diff %in% diff_out|
                                        out_bird_25$outages_weight %in% outages_out|
                                        out_bird_25$st_anomaly %in% anom_out),]%>%
  ungroup()



#correlations

out_corr<-out_bird_75_clean%>%
  select(c("avg_encount_rate","coh_weight","outages_weight","diff","st_anomaly"))%>%
  rename(ER=avg_encount_rate, ERdiff=diff, ERstanom=st_anomaly,COH=coh_weight, Number_Outages=outages_weight)%>%
  cor(use = "pairwise.complete.obs", method="spearman")%>%
 # write.csv("Acorr_table_75.csv")
out_corr<-out_bird_25_clean%>%
  select(c("avg_encount_rate","coh_weight","outages_weight","diff","st_anomaly"))%>%
  rename(ER=avg_encount_rate, ERdiff=diff, ERstanom=st_anomaly,COH=coh_weight, Number_Outages=outages_weight)%>%
  cor(use = "pairwise.complete.obs", method="spearman")%>%
 # write.csv("Acorr_table_25.csv")



#scatter plots


#sink(file="out_lm_75.txt")
print(summary(lm(avg_encount_rate~outages_weight,data=out_bird_75_clean)))
#sink() 
#sink("coh_lm_75.txt")
print(summary(lm(avg_encount_rate~coh_weight,data=out_bird_75_clean)))
#sink() 

par(mar=c(5,6,4,1)+.1)
p1<-ggplot(out_bird_75_clean,
           aes(x=avg_encount_rate, y=outages_weight)) + geom_point(color='blue',alpha =5/10)+
  geom_smooth(method='lm',color='red')+
  labs(x = "Encounter Rate", y = "Number of Outages",
       title= paste0("Weekly Animal Outages-", name_list$sp_name[i], " Encounter Rates"))

p3<-ggplot(out_bird_75_clean,
           aes(x=avg_encount_rate, y=coh_weight)) + geom_point(color='blue',alpha =5/10)+
  geom_smooth(method='lm',color='red')+
  labs(x = "Encounter Rate", y = "Customer Outage Hours")
ggarrange(p1,p3, ncol=1, nrow=2)

#ggsave("Aoutage_encounter_scatter75.jpeg",
#       width=7.822,
#       height = 9.375,
#       dpi = 96,
#       units = "in")

#sink("propout_diff_lm_75.txt")
print(summary(lm(diff~out_weight_diff,data=out_bird_75_clean)))
#sink() 
#sink("propcoh_weight_diff_lm_75.txt")
print(summary(lm(diff~coh_weight_diff,data=out_bird_75_clean)))
#sink() 

p5<-ggplot(out_bird_75_clean,
           aes(x=diff, y=out_weight_diff)) + geom_point(color='blue',alpha =5/10)+
  geom_smooth(method='lm',color='red')+
  labs(x = "Encounter Rate", y = "Change in Number of Outages",
       title = paste0("Change in Weekly Animal Outages-", name_list$sp_name[i], " Encounter Rates"))
p6<-ggplot(out_bird_75_clean,
           aes(x=diff, y=coh_weight_diff)) + geom_point(color='blue',alpha =5/10)+
  geom_smooth(method='lm',color='red')+
  labs(x = "Encounter Rate", y = "Change in Customer Outage Hours")
ggarrange(p5,p6, ncol=1, nrow=2)

#ggsave("Aoutage_encounter_change_scatter75.jpeg",
#       width=7.822,
#       height = 9.375,
#       dpi = 96,
#       units = "in")

#sink(file="out_lm_25.txt")
print(summary(lm(avg_encount_rate~outages_weight,data=out_bird_25_clean)))
#sink() 
#sink("coh_lm_25.txt")
print(summary(lm(avg_encount_rate~coh_weight,data=out_bird_25_clean)))
#sink() 

par(mar=c(5,6,4,1)+.1)
p1<-ggplot(out_bird_25_clean,
           aes(x=avg_encount_rate, y=outages_weight)) + geom_point(color='blue',alpha =5/10)+
  geom_smooth(method='lm',color='red')+
  labs(x = "Encounter Rate", y = "Number of Outages",
       title= paste0("Weekly Animal Outages-", name_list$sp_name[i], " Encounter Rates"))

p3<-ggplot(out_bird_25_clean,
           aes(x=avg_encount_rate, y=coh_weight)) + geom_point(color='blue',alpha =5/10)+
  geom_smooth(method='lm',color='red')+
  labs(x = "Encounter Rate", y = "Customer Outage Hours")
ggarrange(p1,p3, ncol=1, nrow=2)

#ggsave("Aoutage_encounter_scatter25.jpeg",
#       width=7.822,
#       height = 9.375,
#       dpi = 96,
#       units = "in")

#sink("propout_diff_lm_25.txt")
print(summary(lm(diff~out_weight_diff,data=out_bird_25_clean)))
#sink() 
#sink("propcoh_weight_diff_lm_25.txt")
print(summary(lm(diff~coh_weight_diff,data=out_bird_25_clean)))
#sink() 

p5<-ggplot(out_bird_25_clean,
           aes(x=diff, y=out_weight_diff)) + geom_point(color='blue',alpha =5/10)+
  geom_smooth(method='lm',color='red')+
  labs(x = "Encounter Rate", y = "Change in Number of Outages",
       title = paste0("Change in Weekly Animal Outages-", name_list$sp_name[i], " Encounter Rates"))
p6<-ggplot(out_bird_25_clean,
           aes(x=diff, y=coh_weight_diff)) + geom_point(color='blue',alpha =5/10)+
  geom_smooth(method='lm',color='red')+
  labs(x = "Encounter Rate", y = "Change in Customer Outage Hours")
ggarrange(p5,p6, ncol=1, nrow=2)

#ggsave("Aoutage_encounter_change_scatter25.jpeg",
#       width=7.822,
#       height = 9.375,
#       dpi = 96,
 #      units = "in")

#}


#Time series of outages by quantile
#Biggest spikes in 2014, summer of 2016,and summer 2018. Consistencies in 2014 as a big year
#for animal outages. A seasonal trend with increases in summer months is apparent. Large average spikes in 
#2013 and 2015. When smoothed by month, consistent spring and late sumer peaks with exception of a winter
#peak in 2014.
dat4 <- out_bird%>%mutate(date=floor_date(observation_date, unit = "month"))%>%
  group_by(date)%>%
  summarise(Average = mean(num_outages,na.rm = TRUE), 
            Median = median(num_outages,na.rm = TRUE),
            Q25 = quantile(num_outages, probs = c(0.25),na.rm = TRUE),
            Q80 = quantile(num_outages, probs = c(0.8),na.rm = TRUE),
  ) %>% 
  gather(quant, value, Average:Q75)     ## stack quantile values into 1 column for plotting


ggplot(data = dat4, aes(x=date, y=value)) +
  geom_line() +
  xlab("") +
  ylab("Outages per Town") + 
  facet_grid(quant ~ ., scales='free')


#Time Series of COH by quantiles
#Big spike in COH early 2013, and a large average spike in mid 2015. Moderate spikes all throughout 2014.
#When smoothed by months, the spike in 2013 disappears. 2014 is the prominant year with COH. Seems to be
#a mid year (Summer season) pattern of increase.
dat4 <- out_bird%>%mutate(date=floor_date(observation_date, unit = "month"))%>%
  group_by(observation_date)%>%
  summarise(Average = mean(coh,na.rm = TRUE), 
            Median = median(coh,na.rm = TRUE),
            Q25 = quantile(coh, probs = c(0.25),na.rm = TRUE),
            Q75 = quantile(coh, probs = c(0.75),na.rm = TRUE),
  ) %>% 
  gather(quant, value, Average:Q75)    ## stack quantile values into 1 column for plotting


ggplot(data = dat4, aes(x=observation_date, y=value)) +
  geom_line() +
  xlab("") +
  ylab("Proportion COH per Town") + 
  facet_grid(quant ~ ., scales='free')



towns2<-left_join(towns,out_bird, by="city")


brks <- quantile(towns2$coh_weight, probs = seq(0,1,by=0.2),na.rm = TRUE)
facets<-c(29,57,85,120,148,176,211,239,267,302,330,365)
theme_set(theme_bw())
ggplot(data = towns) +
  geom_sf(size=0.1, color=gray(0.7), fill=NA) +
  geom_sf(data = filter(towns2, year==2015), aes(fill = coh_weight), color=NA)+
  facet_wrap(~month,ncol=3)+
  scale_y_continuous(breaks=brks)+
  labs(fill= "Monthly COH")
#ggsave("ER_weekly_townmaps_2018.jpeg",
#       width=12.5,
#       height = 9.375,
#       dpi = 96,
#       units = "in")


library(ggplot2)
library(rgdal)
library(lubridate)
library(sf)
library(gridExtra)
library(tidyverse)
library(dplyr)
library(ggpubr)
library(stringr)

memory.limit(100000)

#Load predictions 
load("Objects and Data/3_DP_predictions.rda")
load("Objects and Data/0_MA_town_shapefiles.rda")
load("Objects and Data/0_species_list.rda")


#--------------------------------------
###Plot bird detection probability predictions
#--------------------------------------

#MAP PREDICTIONS 
#------------------------
for (j in 1:length(name_list$sp_file)) {
  pred_er<-ebirdRF_results_town[[j]]
  pred_sub<-pred_er%>%
    filter(Year == 2018&week%in%c(2,12,22,32,42,52))%>%#sample every 10 weeks
    left_join(ma_towns,by="city")%>%
    rename(Week=week)
  
                                
#2018 by weeks

                            
ggplot()+
  geom_sf(data=pred_sub, 
          aes(geometry=geometry,fill=eBird.DP.RF))+
  facet_wrap(~Week,ncol=2, labeller = label_both)+
  theme_classic(base_size = 12)+
  scale_fill_viridis_c(option = "plasma")+
  labs(fill="Weekly Detection \nProbability in 2018")+
  theme(legend.position = "top",
        strip.background = element_rect(color=NA),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank())
                                
ggsave(paste0("../Outputs/",name_list$sp_file[[j]],"/",name_list$sp_file[[j]],"_weekly_townmaps_2018.jpeg"),
   width=8,
   height = 9,
   dpi = 300,
   units = "in")
                                

#PLOT TIME SERIES
#---------------------------
pred_state<-ebirdRF_results_state[[j]]
#Annual encounter rate statewide across years
annual_state_yrs<-ggplot(data=pred_state[pred_state$week==24,],
                         aes(x=as.factor(Year), y=eBird.DP.RF, group=1)) +
  geom_line()+
  theme_classic(base_size = 12)+
  scale_x_discrete(breaks = seq(2005,2018,2)) +
  labs(x = "Year",
       y = "Detection Probability",
       title= str_wrap(paste0(name_list$sp_name[[j]],": Annual State Detection Probability"),
                       50))

#weekly encounter rate statewide across years
weekly_state_yrs<-ggplot(data=pred_state, aes(x=date, y=eBird.DP.RF, group=1)) +
  geom_line()+
  theme_classic(base_size = 12)+
  labs(x = "Date",
       y = "Detection Probability",
       title= str_wrap(paste0(name_list$sp_name[[j]],": Weekly State Detection Probability"),
                       50))


#weekly encounter rate statewide in 2018
weekly_state_18<-ggplot(data=filter(pred_state, Year==2018), aes(x=as.factor(week), y=eBird.DP.RF, group=1)) +
  geom_line()+
  geom_point()+
  theme_classic(base_size = 12)+
  scale_x_discrete(breaks = seq(from=1, to=52, by=4)) +
  labs(x = "Week",
       y = "Detection Probability",
       title= str_wrap(paste0(name_list$sp_name[[j]],": Weekly State Detection Probability in 2018"),
       50))


ggarrange(annual_state_yrs,weekly_state_yrs,weekly_state_18, ncol=1)

ggsave(paste0("../Outputs/",name_list$sp_file[[j]],"/",name_list$sp_file[[j]],"_state_plots.jpeg"),
       width=7,
       height = 11,
       dpi = 300,
       units = "in")

} 

for (j in 1:length(name_list$sp_file)) {
  pred_er<-ebirdRF_results_town[[j]]%>%
    filter(city %in% c("AVON","BERLIN","MILLVILLE",
                      "WALES","WHITMAN","BOSTON", "SPRINGFIELD"))
  
#weekly encounter rate by town in 2018
towns_18<-ggplot(data=filter(pred_er,Year==2018),
                             aes(x=as.factor(week), y=eBird.DP.RF, group=city)) +
  geom_line(aes(color=city), show.legend = TRUE)+
  theme_classic(base_size = 12)+
  scale_color_viridis_d(option = "turbo")+
  scale_x_discrete(breaks = seq(from=1, to=52, by=4)) +
  labs(x = "Week",
       y = "2018 Weekly Detection Probability",
       title= str_wrap(paste0(name_list$sp_name[[j]],": Weekly Town Detection Probability in 2018"),
                       50))
towns_18

#Weekly encounter rate by town across years
towns<-ggplot(data=pred_er,
                          aes(x=date, y=eBird.DP.RF, group=city)) +
  geom_line(aes(color=city), show.legend = TRUE)+
  theme_classic(base_size = 12)+
  scale_color_viridis_d(option = "turbo")+
  labs(x = "Date",
       y = "Weekly Detection Probability",
       title= str_wrap(paste0(name_list$sp_name[[j]],": Weekly Town Detection Probability"),
                       50))
towns

ggarrange(towns,towns_18, ncol=1)

ggsave(paste0("../Outputs/",name_list$sp_file[[j]],"/",name_list$sp_file[[j]],"_town_plots.jpeg"),
       width=7,
       height = 7,
       dpi = 300,
       units = "in")
}

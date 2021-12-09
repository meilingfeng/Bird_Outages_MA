library(tidyverse)

load("Objects and Data/4_dp_outs_towns.rda")
load("Objects and Data/5_DP_PCA.rda")
load("Objects and Data/0_MA_town_shapefiles.rda")
load("Objects and Data/0_species_list.rda")

#Summarize the average and sd of SAIDI and bird DPs for each week across years and towns
dp_outs_towns<-dp_outs_towns[,c("actual_city_town","week","year","saidi",
                                toupper(name_list$sp_file))]
summ<- dp_outs_towns%>%
  group_by(week)%>%
  summarise(across(c("saidi", toupper(name_list$sp_file)), list(mean = mean, 
                                                                sd = sd,
                                                                n=length)))%>%
  gather(measure, value, -week)     ## stack metrics into 1 column for plotting
summ2<-summ%>%
  filter(grepl("_sd",measure))%>%
  rename(sd=value)%>%
  mutate(measure=str_replace(measure,"_.*", ""))
summ3<-summ%>%
  filter(grepl("_mean",measure))%>%
  rename(mean=value)%>%
  mutate(measure=str_replace(measure,"_.*", ""))
summ4<-summ%>%
  filter(grepl("_n",measure))%>%
  rename(n=value)%>%
  mutate(measure=str_replace(measure,"_.*", ""))%>%
  left_join(summ2)%>%
  left_join(summ3)%>%
  filter(!is.na(sd))%>%
  mutate(week=as.numeric(week),
         ci=qnorm(0.975)*sd/sqrt(n))

#plot time series of average and 95th CI for weeks in a year
#plot species grouped by PC1 (Assumed to be grouping by temporal trends)
g1<-row.names(vectors[vectors$PC1>=0.2,])
g2<-row.names(vectors[vectors$PC1>-0.2&vectors$PC1<0.2,])
g3<-row.names(vectors[vectors$PC1<=-0.2,])

p4<-ggplot(data = summ4%>%filter(measure%in%g3), aes(x=week, y=mean,group=measure)) +
  geom_line(aes(color=measure)) +
  geom_ribbon(aes(ymin=mean-ci, ymax=mean+ci, fill=measure),alpha=0.3)+
  theme_bw(base_size = 12)+
  scale_x_continuous(breaks=seq(1,max(summ3$week),5))+
  guides(color=FALSE)+
  labs(x="Week of the Year", y="Mean Detection Probability",
       fill="Species",title="A")+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p5<-ggplot(data = summ4%>%filter(measure%in%g2), aes(x=week, y=mean,group=measure)) +
  geom_line(aes(color=measure)) +
  geom_ribbon(aes(ymin=mean-ci, ymax=mean+ci, fill=measure),alpha=0.3)+
  theme_bw(base_size = 12)+
  scale_x_continuous(breaks=seq(1,max(summ3$week),5))+
  guides(color=FALSE)+
  labs(x="Week of the Year", y="Mean Detection Probability",
       fill="Species",title="B") +
  theme(axis.title.x = element_blank())

p6<-ggplot(data = summ4%>%filter(measure%in%g1), aes(x=week, y=mean,group=measure)) +
  geom_line(aes(color=measure)) +
  geom_ribbon(aes(ymin=mean-ci, ymax=mean+ci, fill=measure),alpha=0.3)+
  theme_bw(base_size = 12)+
  scale_x_continuous(breaks=seq(1,max(summ3$week),5))+
  guides(color=FALSE)+
  labs(x="Week of the Year", y="Mean Detection Probability",
       fill="Species",title="C") +
  theme(axis.title.y = element_blank())

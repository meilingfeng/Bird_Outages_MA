library(tidyverse)
library(ggcorrplot)
library(AICcmodavg)
library(regclass)
library(sf)

#load outage and bird data
#---------------------------------------------------------------------------------------------
#SAIDI
dp<-read.csv("Outputs/dp_out_towns.csv")%>%
  #remove missing data
  filter(saidi!=0 & !(is.na(RWBL)))%>%
  #log-transform SAIDI
  mutate(log_saidi=log(saidi),
         week=as.factor(week),
         year=as.factor(year),
         season=case_when(
           month%in%c(12,1,2)~ "winter",
           month%in%c(3,4,5) ~ "spring",
           month%in%c(6,7,8) ~ "summer",
           month%in%c(9,10,11) ~ "fall"),
         month=as.factor(month))
  #create a weekly bird DP average across species
dp$avg_DP<-rowMeans(dp[c(16:29)],na.rm=T)

#-------------------------------------------------------------------------------------------
#Check for correlation and collinearity between predictors
#Between species
cor1<-round(cor(dp[,c("HOSP","MODO","EUST","TUVU","OSPR","RTHA","RWBL","COGR","BHCO",
                   "PIWO","RBWO","DOWO","HAWO","NOFL")]),1)
cor1p<-round(cor_pmat(dp[,c("HOSP","MODO","EUST","TUVU","OSPR","RTHA","RWBL","COGR","BHCO",
                        "PIWO","RBWO","DOWO","HAWO","NOFL")]),4)
ggcorrplot(cor1,type = "lower", outline.color = "white",hc.order = T, 
           p.mat=cor1p,insig="blank",
           lab=T)

#blackbirds and woodpeckers correlate. Remove COGR, BHCO, DOWO, HAWO/RBWO

#Between species and habitat
cor2<-round(cor(dp[,c("PIWO","DOWO","HAWO","NOFL",
                        "Forest","Developed","Grassland", "Open_Water", "Barren_Land")])
              ,1)
cor2p<-round(cor_pmat(dp[,c("PIWO","DOWO","HAWO","NOFL",
                               "Forest","Developed","Grassland", "Open_Water", "Barren_Land")])
                ,4)
ggcorrplot(cor2,type = "lower", outline.color = "white", 
           p.mat=cor2p,insig="blank",
           lab=T)

#PIWO correlated with forested habitat, NOFL with developed habitat
cor3<-round(cor(dp[,c("HOSP","MODO","EUST",
                      "Forest","Developed","Grassland", "Open_Water", "Barren_Land")])
            ,1)
cor3p<-round(cor_pmat(dp[,c("HOSP","MODO","EUST",
                            "Forest","Developed","Grassland", "Open_Water", "Barren_Land")])
             ,4)
ggcorrplot(cor3,type = "lower", outline.color = "white",
           p.mat=cor3p,insig="blank",
           lab=T)
#HOSP correlated with developed

cor4<-round(cor(dp[,c("TUVU","OSPR","RTHA",
                      "Forest","Developed","Grassland", "Open_Water", "Barren_Land")])
            ,1)
cor4p<-round(cor_pmat(dp[,c("TUVU","OSPR","RTHA",
                            "Forest","Developed","Grassland", "Open_Water", "Barren_Land")])
             ,4)
ggcorrplot(cor4,type = "lower", outline.color = "white",
           p.mat=cor3p,insig="blank",
           lab=T)

cor5<-round(cor(dp[,c("RWBL","COGR","BHCO",
                      "Forest","Developed","Grassland", "Open_Water", "Barren_Land")])
            ,1)
cor5p<-round(cor_pmat(dp[,c("RWBL","COGR","BHCO",
                            "Forest","Developed","Grassland", "Open_Water", "Barren_Land")])
             ,4)
ggcorrplot(cor5,type = "lower", outline.color = "white",
           p.mat=cor3p,insig="blank",
           lab=T)

#smallest value of VIF is 1 = no collinearity. Exceeds 5 or 10 = collinearity.

m<-lm(log_saidi~
        TUVU+MODO+HOSP+OSPR+RTHA+RWBL+RBWO+PIWO+HAWO+NOFL+EUST+
        month+year+
        Developed+Forest+Barren_Land+Open_Water+Grassland,data=dp)
VIF(m)
m2<-lm(log_saidi~
         TUVU+MODO+HOSP+OSPR+RTHA+RWBL+RBWO+PIWO+HAWO+NOFL+EUST+
        month+year+
        Forest+Barren_Land+Open_Water+Grassland,data=dp)
VIF(m2)

#Removing Developed/Forest habitat reduces multicollinearity


#Modeling SAIDI with DP, habitat, and time
#-------------------------------------------------------------------------------------------------
#Determine if species, habitat, or time variables are more important


#species (removing correlated blackbirds and woodpeckers)
m.s<-lm(log_saidi~TUVU+MODO+HOSP+OSPR+RTHA+RWBL+RBWO+PIWO+HAWO+NOFL+EUST,data=dp)

#Time (week does best but month is close. Season does poorly.)
m.t<-lm(log_saidi~month+year,data=dp)

#Habitat (Remove forest or developed, correlated. Forest performs better)
m.h<-lm(log_saidi~Forest+Barren_Land+Open_Water+Grassland,data=dp)


#species+time
m.st<-lm(log_saidi~(TUVU*month)+(MODO*month)+(HOSP*month)+(OSPR*month)+(RTHA*month)+(RWBL*month)+
  (PIWO*month)+(HAWO*month)+(NOFL*month)+(EUST*month)+year,data=dp)

#species+habitat 
#use interaction with species. Species are more likely to use equipment in areas
#lacking habitat or in habitat where they like to forage, nest, etc.
#Species and habitat separately
m.sh<-lm(log_saidi~TUVU+MODO+HOSP+OSPR+RTHA+RWBL+PIWO+HAWO+NOFL+EUST+
           Barren_Land+Open_Water+Grassland,data=dp)
#Species interact with the grid more in habitats they actively use 
#or less because they are less likely to use equipment when they have suitable habitat
m.sh<-lm(log_saidi~(TUVU*Forest)+(MODO*Forest)+(HOSP*Forest)+(OSPR*Forest)+
           (RTHA*Forest)+RWBL*(Forest)+
           (PIWO*Forest)+(HAWO*Forest)+
           NOFL+(EUST*Forest)+
           Barren_Land,data=dp)
#Species interact with the grid more in barren/developed habitat because
#They are forced to use equipment in replace of natural habitat
m.sh<-lm(log_saidi~(TUVU*Developed)+(MODO*Developed)+(HOSP*Developed)+(OSPR*Developed)+
           (RTHA*Developed)+(RWBL*Developed)+
           (PIWO*Developed)+(HAWO*Developed)+
           NOFL+(EUST*Developed),data=dp)

#model using species* forest habitat performs the best (Highest R2, lowest RSE)


#species+habitat+time
#MODO,RTHA,PIWO,EUST have significant habitat interaction
#HAWO,RWBL,HOSP,MODO have significant month interactions
m.sth<-lm(log_saidi~TUVU+(MODO*month)+(MODO*Forest)+(HOSP*month)+OSPR+(RTHA*Forest)+
  (RWBL*month)+(PIWO*Forest)+(HAWO*month)+NOFL+(EUST*Forest)+year+
    Barren_Land,data=dp)

#time+habitat 
m.th<-lm(log_saidi~month+year+Forest+Barren_Land+Open_Water+Grassland,data=dp)

models <- list(m.s, m.t, m.h, m.st, m.sh, m.sth,
               m.th)

mod.names <- c('Species','Time','Habitat','Species.Time',
               'Species.Habitat', 'Species.Time.Habitat', 
               'Time.Habitat')
  
aic<-aictab(cand.set = models, modnames = mod.names)

#Compare model performance in a table
df<-as.data.frame(matrix(ncol = 5 , nrow= 7))
colnames(df)<- c("Model","F.Statistic","P","RSE","R.Squared")
df$Model<-mod.names
df$F.Statistic<-lapply(models, summary)%>%
  sapply('[', "fstatistic")%>%
  sapply('[',"value")
df$R.Squared<-lapply(models, summary)%>%
  sapply('[', "r.squared")%>%
  unlist()
df$RSE<-lapply(models, summary)%>%
  sapply('[', "sigma")%>%
  unlist()
df$P<-sapply(models, function(x){
  summary(x)$fstatistic %>% 
    {unname(pf(.[1],.[2],.[3],lower.tail=F))}})

df<-left_join(df,as.data.frame(aic)%>%select(Modnames,Delta_AICc),
              by=c("Model"="Modnames"))
df<-df%>%
  mutate(across(-c("Model","P"), round, 4))%>%
  mutate(P.Value=case_when(
    P<0.05~"<0.05",
    P>=0.05~as.character(P)
  ))%>%
  select(-P)%>%
  rename(Delta.AIC=Delta_AICc)%>%
  arrange(Delta.AIC)

write.csv(df,"Outputs/Regression/predictor_selection_models.csv",row.names = F)

#Compare models using species from distinct spatial and temporal patterns from the PCA
#HAWO and RTHA represent residents vs OSPR and RWBL who represent summer migrants
#PIWO and TUVU occupy western, rural areas vs HOSP and NOFL in eastern urban areas
m.resident<-lm(log_saidi~(RWBL*month)+OSPR+year+
            Barren_Land,data=dp)
m.migrant<-lm(log_saidi~(RTHA*Forest)+(HAWO*month)+year+
                Barren_Land,data=dp)
m.rural<-lm(log_saidi~TUVU+(PIWO*Forest)+year+
            Barren_Land,data=dp)
m.urban<-lm(log_saidi~(HOSP*month)+NOFL+year+
            Barren_Land,data=dp)




#AIC
#define list of models
models <- list(m.sth,m.resident,m.migrant,m.rural, m.urban)

#specify model names
mod.names <- c('All.Species',
               'Residents', 
               'Migrants', 
               'Rural',
               'Urban')

#calculate AIC of each model
aic.pc<-aictab(cand.set = models, modnames = mod.names)

#Compare model performance in a table
df.pc<-as.data.frame(matrix(ncol = 5 , nrow= 5))
colnames(df.pc)<- c("Model","F.Statistic","P","RSE","R.Squared")
df.pc$Model<-mod.names
df.pc$F.Statistic<-lapply(models, summary)%>%
  sapply('[', "fstatistic")%>%
  sapply('[',"value")
df.pc$R.Squared<-lapply(models, summary)%>%
  sapply('[', "r.squared")%>%
  unlist()
df.pc$RSE<-lapply(models, summary)%>%
  sapply('[', "sigma")%>%
  unlist()
df.pc$P<-sapply(models, function(x){
  summary(x)$fstatistic %>% 
    {unname(pf(.[1],.[2],.[3],lower.tail=F))}})

df.pc<-left_join(df.pc,as.data.frame(aic.pc)%>%select(Modnames,Delta_AICc),
              by=c("Model"="Modnames"))
df.pc<-df.pc%>%
  mutate(across(-c("Model","P"), round, 4))%>%
  mutate(P.Value=case_when(
    P<0.05~"<0.05",
    P>=0.05~as.character(P)
  ))%>%
  select(-P)%>%
  rename(Delta.AIC=Delta_AICc)%>%
  arrange(Delta.AIC)

write.csv(df.pc,"Outputs/Regression/species_subset_models.csv",row.names = F)

#Multispecies model is the best
#Bird outages seem most correlated with activity levels of spring/fall migrants


#Compare species*habitat*time models using subsets of data in each season (remove month)
#and in forest vs developed+barren habitat types (remove habitat)
m.summer<-lm(log_saidi~TUVU+(MODO*Forest)+HOSP+OSPR+(RTHA*Forest)+
            RWBL+(PIWO*Forest)+HAWO+NOFL+(EUST*Forest)+year+
            Barren_Land,data=dp%>%filter(season=="summer"))
m.winter<-lm(log_saidi~TUVU+(MODO*Forest)+HOSP+OSPR+(RTHA*Forest)+
            RWBL+(PIWO*Forest)+HAWO+NOFL+(EUST*Forest)+year+
            Barren_Land,data=dp%>%filter(season=="winter"))
m.Forest<-lm(log_saidi~TUVU+(MODO*month)+(HOSP*month)+OSPR+RTHA+
               (RWBL*month)+PIWO+(HAWO*month)+NOFL+EUST+year+
               Barren_Land,data=dp%>%filter(Forest>quantile(Forest,0.75)))
m.Developed<-lm(log_saidi~TUVU+(MODO*month)+(HOSP*month)+OSPR+RTHA+
               (RWBL*month)+PIWO+(HAWO*month)+NOFL+EUST+year+
               Barren_Land,data=dp%>%filter(Developed>quantile(Developed,0.75)))


#AIC
#define list of models
models <- list(m.sth,m.summer,m.winter,m.Forest, m.Developed)

#specify model names
mod.names <- c('All.Space.Time',
               'Summer', 
               'Winter', 
               'Forested',
               'Developed')

#calculate AIC of each model
aic.patterns<-aictab(cand.set = models, modnames = mod.names)

#Compare model performance in a table
df.patterns<-as.data.frame(matrix(ncol = 5 , nrow= 5))
colnames(df.patterns)<- c("Model","F.Statistic","P","RSE","R.Squared")
df.patterns$Model<-mod.names
df.patterns$F.Statistic<-lapply(models, summary)%>%
  sapply('[', "fstatistic")%>%
  sapply('[',"value")
df.patterns$R.Squared<-lapply(models, summary)%>%
  sapply('[', "r.squared")%>%
  unlist()
df.patterns$RSE<-lapply(models, summary)%>%
  sapply('[', "sigma")%>%
  unlist()
df.patterns$P<-sapply(models, function(x){
  summary(x)$fstatistic %>% 
    {unname(pf(.[1],.[2],.[3],lower.tail=F))}})

df.patterns<-left_join(df.patterns,as.data.frame(aic.patterns)%>%select(Modnames,Delta_AICc),
                 by=c("Model"="Modnames"))
df.patterns<-df.patterns%>%
  mutate(across(-c("Model","P"), round, 4))%>%
  mutate(P.Value=case_when(
    P<0.05~"<0.05",
    P>=0.05~as.character(P)
  ))%>%
  select(-P)%>%
  rename(Delta.AIC=Delta_AICc)%>%
  arrange(Delta.AIC)

write.csv(df.patterns,"Outputs/Regression/space_time_subset_models.csv",row.names = F)

#Make table of species coefficients in each model
sp_list<-c("TUVU","MODO","HOSP","OSPR","RTHA",
             "RWBL","PIWO","HAWO","NOFL","EUST")

df.sp<-df.sp.p<-as.data.frame(matrix(ncol = 5 , nrow= length(sp_list)))

colnames(df.sp)<-colnames(df.sp.p)<-mod.names


row.names(df.sp)<-row.names(df.sp.p)<-sp_list



for (i in length(models)) {

  coefs <- round(models[[5]][["coefficients"]],3)
  coefs <- coefs[names(coefs)%in%sp_list]
  df.sp[[5]]<-coefs
  
}
write.csv(df.sp,"Outputs/Regression/species_coefficients.csv",row.names = T)

for (i in length(models)) {

    p<-round(summary(models[[1]])$coefficients[,4],3)
    p <- p[names(p)%in%sp_list]
    df.sp.p[[1]]<-p
  
}
write.csv(df.sp.p,"Outputs/Regression/species_coefficients_signif.csv",row.names = T)



#---------------------------------------------------------------------------------------------------
#look at species/habitat/time model residuals against time and space

dp$resid<-resid(m.sth)

cent<-st_read("Raw_Data/MA_Shapefiles/town_cent_namescorrected.shp")%>%
  st_set_crs(st_crs(4326))

coords<-as.data.frame(st_coordinates(cent))
cent$long<-round(coords$X,1)
cent$lat<-round(coords$Y,1)
cent<-st_drop_geometry(cent)
resid_dat<-left_join(dp,cent[c("city","long","lat")],by=c("actual_city_town"="city"))%>%
  select("actual_city_town","week","month","year","date","long","lat",
         "resid")%>%
  mutate(date=as.Date(date),
         Longitude=cut(long,
                       breaks=seq(from=min(long),to=max(long),by=0.5),
                       ordered_result = T))
  

ggplot(data=resid_dat,aes(x=month,y=resid))+
  geom_violin()+
  geom_boxplot(width = 0.1)+
  theme_classic(base_size = 12)+
  labs(y="Residuals",x="Month")

ggplot(data=resid_dat%>%filter(!is.na(Longitude)),aes(x=Longitude,y=resid))+
  geom_violin()+
  geom_boxplot(width = 0.1)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme_classic(base_size = 12)+
  labs(y="Residuals",x="Longitude")

#No spatial/temporal patterns left in residuals





#----------------------------------------------------------------------------------------
#Show predicted saidi across weeks using comparative models
newdata1 <- dp%>%
  group_by(year,month)%>%
  summarise(across(c("Barren_Land","Forest",14:27), mean, na.rm=T))%>%
  filter(year=="2018")%>%
  ungroup()

newdata2 <- cbind(newdata1, predict(m.resident, newdata = newdata1, type = "response",
                                    se.fit = TRUE))
newdata2 <- newdata2%>%
  mutate(Resident.P = fit,
         Resident.LL = fit - (1.96 * se.fit),
         Resident.UL = fit + (1.96 * se.fit))%>%
  select(month,Resident.P,Resident.LL,Resident.UL)

newdata3 <- cbind(newdata1, predict(m.migrant, newdata = newdata1, type = "response",
                                    se = TRUE))
newdata3 <- newdata3%>%
  mutate(Migrant.P = fit,
         Migrant.LL = fit - (1.96 * se.fit),
         Migrant.UL = fit + (1.96 * se.fit))%>%
  select(month,Migrant.P,Migrant.LL,Migrant.UL)

newdata4 <- cbind(newdata1, predict(m.sth, newdata = newdata1, type = "response",
                                    se.fit = TRUE))
newdata4 <- newdata4%>%
  mutate(All.P = fit,
         All.LL = fit - (1.96 * se.fit),
         All.UL = fit + (1.96 * se.fit))%>%
  select(month,All.P,All.LL,All.UL)

preds<-left_join(newdata2,newdata3)%>%
  left_join(newdata4)%>%
  pivot_longer(cols = -c("month"),
               names_to = c("Model","Model.Output"),
               names_pattern= "(.{3,8})\\.(.*)",
               values_to="value")%>%
  pivot_wider(names_from=Model.Output,
              values_from=value)

ggplot(preds, aes(x = month, y = exp(P),group=Model)) + 
  geom_ribbon(aes(ymin = exp(LL),
                  ymax = exp(UL), 
                  fill = Model), alpha = 0.2) +
  geom_line(aes(colour = Model), size = 1)+
  scale_x_discrete("Month")+
  labs(x="Month",y="SAIDI")+
  theme_bw(base_size = 12)

ggsave("Figures/model_comparisons.jpeg",
       width=9,
       height = 8,
       dpi = 300,
       units = "in")

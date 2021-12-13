library(tidyverse)
library(ggcorrplot)
library(AICcmodavg)
library(regclass)


load("Objects and Data/4_dp_outs_towns.rda")
load("Objects and Data/0_species_list.rda")

dp<-dp_outs_towns%>%
  #remove missing data
  filter(saidi!=0 & !(is.na(RWBL)))%>%
  mutate(week=as.factor(week),
         year=as.factor(year),
         month=as.factor(month))

#-------------------------------------------------------------------------------------------
#Check for correlation and collinearity between predictors
#1. Between species
cor1<-round(cor(dp[,toupper(name_list$sp_file)]),1)
cor1p<-round(cor_pmat(dp[,toupper(name_list$sp_file)]),4)
ggcorrplot(cor1,type = "lower", outline.color = "white",hc.order = T, 
           p.mat=cor1p,insig="blank",
           lab=T)

  #blackbird species correlate and woodpecker species correlate. 
  #Select representative species.
  #Remove COGR, BHCO, DOWO, HAWO/RBWO

#2. Between species and habitat
cor2<-round(cor(dp[,c(toupper(name_list$sp_file),
                        "Forest","Developed","Grassland", "Open_Water", "Barren_Land")])
              ,1)
cor2p<-round(cor_pmat(dp[,c(toupper(name_list$sp_file),
                               "Forest","Developed","Grassland", "Open_Water", "Barren_Land")])
                ,4)
ggcorrplot(cor2,type = "lower", outline.color = "white", 
           p.mat=cor2p,insig="blank",
           lab=T)

  #PIWO correlated with forested habitat, NOFL with developed habitat
  #HOSP correlated with developed


#3. Variance inflation factor
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




#-------------------------------------------------------------------------------------------------
#Model SAIDI with species DPs, habitat, and time

#1. Determine if species, habitat, or time variables are more important

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
m.sh<-lm(log_saidi~(TUVU*Forest)+(MODO*Forest)+(HOSP*Forest)+(OSPR*Forest)+
           (RTHA*Forest)+RWBL*(Forest)+
           (PIWO*Forest)+(HAWO*Forest)+
           NOFL+(EUST*Forest)+
           Barren_Land,data=dp)
  #model using species* forest habitat interaction performs the best 
  # (Highest R2, lowest RSE)

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

df<-left_join(df,as.data.frame(aic)%>%dplyr::select(Modnames,Delta_AICc),
              by=c("Model"="Modnames"))
df<-df%>%
  mutate(across(-c("Model","P"), round, 4))%>%
  mutate(P.Value=case_when(
    P<0.05~"<0.05",
    P>=0.05~as.character(P)
  ))%>%
  dplyr::select(-P)%>%
  rename(Delta.AIC=Delta_AICc)%>%
  arrange(Delta.AIC)

write.csv(df,"Outputs/predictor_selection_models.csv",row.names = F)



#2.Compare models using species from distinct spatial and temporal patterns from the PCA
#HAWO and RTHA represent residents vs OSPR and RWBL who represent summer migrants
#PIWO and TUVU occupy rural areas vs HOSP and NOFL occupying urban areas
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

df.pc<-left_join(df.pc,as.data.frame(aic.pc)%>%dplyr::select(Modnames,Delta_AICc),
              by=c("Model"="Modnames"))
df.pc<-df.pc%>%
  mutate(across(-c("Model","P"), round, 4))%>%
  mutate(P.Value=case_when(
    P<0.05~"<0.05",
    P>=0.05~as.character(P)
  ))%>%
  dplyr::select(-P)%>%
  rename(Delta.AIC=Delta_AICc)%>%
  arrange(Delta.AIC)

write.csv(df.pc,"Outputs/species_subset_models.csv",row.names = F)

#Multispecies model is the best
#Bird outages seem most correlated with activity levels of spring/fall migrants

sth.res = resid(m.sth)
hist(sth.res)


#3. Compare species*habitat*time models using subsets of data in each season (remove month)
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

df.patterns<-left_join(df.patterns,as.data.frame(aic.patterns)%>%dplyr::select(Modnames,Delta_AICc),
                 by=c("Model"="Modnames"))
df.patterns<-df.patterns%>%
  mutate(across(-c("Model","P"), round, 4))%>%
  mutate(P.Value=case_when(
    P<0.05~"<0.05",
    P>=0.05~as.character(P)
  ))%>%
  dplyr::select(-P)%>%
  rename(Delta.AIC=Delta_AICc)%>%
  arrange(Delta.AIC)

write.csv(df.patterns,"Outputs/space_time_subset_models.csv",row.names = F)

#Make table of species coefficients in each model
sp_list<-c("TUVU","MODO","HOSP","OSPR","RTHA",
             "RWBL","PIWO","HAWO","NOFL","EUST")

df.sp<-df.sp.p<-as.data.frame(matrix(ncol = 5 , nrow= length(sp_list)))

colnames(df.sp)<-colnames(df.sp.p)<-mod.names


row.names(df.sp)<-row.names(df.sp.p)<-sp_list



for (i in length(models)) {

  coefs <- round(models[[5]][["coefficients"]],3)#need to input models manually
  coefs <- coefs[names(coefs)%in%sp_list]
  df.sp[[5]]<-coefs
  
}
write.csv(df.sp,"Outputs/species_coefficients.csv",row.names = T)

for (i in length(models)) {

    p<-round(summary(models[[1]])$coefficients[,4],3)
    p <- p[names(p)%in%sp_list]
    df.sp.p[[1]]<-p
  
}
write.csv(df.sp.p,"Outputs/species_coefficients_signif.csv",row.names = T)


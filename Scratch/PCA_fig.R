library(ggplot2)
library(gridExtra)
library(tidyverse)
library(stringr)
library(factoextra)


memory.limit(100000)

#Load data
dp<-read.csv("Outputs/dp_out_towns.csv")


#------------
#####PCAs####
#------------
#bird species dimension reduction
#----------------------------------
#Maybe week misalignment? Bird data missing week 53 in some years. Remove dates with NAs
pr.all<-dp[complete.cases(dp), ]  

#Run PCAs
#select just species DPs
#PCA outputs, create arranged outputs for plotting categorizing variables

pr.out.all.outages=prcomp(pr.all[,c(16:29)] , scale=TRUE)


#PC loading vectors

#pr.out.all.outages$rotation%>%write.csv("../Outputs/PCA/allspacetime_pcloadings.csv")


#plot first 2 pc loading vectors
fviz_pca_var(pr.out.all.outages,
                     axes = c(1,2),
                     labelsize=5,
                     col.var = "contrib", # Color by contributions to the PC
                     gradient.cols = c("#13306dff","#7e4e90ff","#f9b641ff"),
                     repel = TRUE ,   # Avoid text overlapping
             ggtheme = theme_bw(base_size = 12),
             title=""
)


ggsave("Figures/Paper Figures/pc1_2.jpeg",
       width=6,
       height = 7,
       dpi = 300,
       units = "in")


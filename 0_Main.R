library(tidyverse)
library(patchwork)

memory.limit(100000)

#---------------------------------------------------------------------------------------------------
# source analysis script
source("1_Habitat_Data_Preparation.R")
source("2_Bird_DP_Model.R")
source("3_Outage_birdDP_merge.R")
source("4_BirdDP_PCA.R")
source("5_Regression.R")

#-----------------------------------------------------------------------------------------
# source figure script
source("Fig_timeseries.R")
source("Fig_maps.R")
source("Fig_pc_loadings.R")

#---------------------------------------------------------------------------------------------------
# plot and save / = below | = beside

p1
ggsave("Outputs/Figures/SpeciesMaps_group1.jpeg", width = 9, height = 5, dpi = "retina")

p2
ggsave("Outputs/Figures/SpeciesMaps_group2.jpeg", width = 9, height = 8, dpi = "retina")

p3
ggsave("Outputs/Figures/SpeciesMaps_group3.jpeg", width = 9, height = 10, dpi = "retina")

p_map
ggsave("Outputs/Figures/SpeciesMaps_contrast.jpeg", width = 9, height = 5, dpi = "retina")

g4 <- (p4 / p5 / p6) 
ggsave("Outputs/Figures/SpeciesTS_groups.jpeg", width = 8, height = 13, dpi = "retina")

p7
ggsave("Outputs/Figures/PC1_PC2_loadings.jpeg", width = 7, height = 7, dpi = "retina")


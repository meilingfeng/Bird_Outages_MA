library(tidyverse)
library(factoextra)


load("Objects and Data/5_DP_PCA.rda")


#plot loading vectors of first 2 pcs (species activity (DP))
#Positively correlated birds point to same side,
#negatively correlated birds point to opposite sides
p7<-fviz_pca_var(pr.out.all,
                     axes = c(1,2),
                     labelsize=5,
                     col.var = "contrib", # Color by contributions to the PC
                     gradient.cols = c("#13306dff","#7e4e90ff","#f9b641ff"),
                     repel = TRUE ,   # Avoid text overlapping
             ggtheme = theme_bw(base_size = 12))+
labs(x="PC1 (23.4%)",y="PC2 (18.9%)",title = "",
     color="Contribution")


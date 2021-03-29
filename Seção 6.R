library(tidyverse)  
library(cluster) 
library(gclus)
library(factoextra) 
library(dendextend) 
library("openxlsx") 
library(sf)
library(geobr)
library(dendextend)
library(ade4)
library(vegan)

WAV <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S6/WAV-E.xlsx", sheet = "Geral")

WAV <- t(WAV)
is.numeric(WAV)

SLI <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S6/SPL-E.xlsx", sheet = "Geral")

SLI <- t(SLI)
is.numeric(SLI)

dados <- read.xlsx("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S6/Mapas.xlsx")
mc <- read_municipality(code_muni= "SP")

A <- vegdist(WAV, method = "jaccard", binary = TRUE)
B <- vegdist(SLI, method = "jaccard", binary = TRUE)

hc1 <- hclust(A, method = "ward.D2" )
hc2 <- hclust(B, method = "ward.D2" )

gap_stat1 <- clusGap(WAV, FUN = hcut, K.max = 25, B = 10)
fviz_gap_stat(gap_stat1)
gap_stat2 <- clusGap(SLI, FUN = hcut, K.max = 25, B = 10)
fviz_gap_stat(gap_stat2)

plot(hc1, cex = 0.6, hang = -1)
rect.hclust(hc1, k = 6, border = 2:5)

plot(hc2, cex = 0.6, hang = -1)
rect.hclust(hc2, k = 6, border = 2:5)

dend1 <- as.dendrogram (hc1)
dend2 <- as.dendrogram (hc2)

entanglement(dend1, 
             dend2,
             L = 1,
             leaves_matching_method = c("order"))


tanglegram(dend1, dend2,
           highlight_distinct_edges = FALSE, 
           common_subtrees_color_lines = TRUE, 
           common_subtrees_color_branches = FALSE,
           main_left = "Wikiaves",
           main_right = "SpeciesLink",
           lwd = .5,
           #sort = TRUE,
           k_branches = 6)
           #main = paste("entanglement =", round(entanglement(dendlist), 2))

grp1 <- cutree(hc1, k = 6)
grp1

grp2 <- cutree(hc2, k = 6)
grp2


maps <- merge(mc,dados)

maps$WAV <- cut(maps$dendWAV, breaks = c(-2,0,1,2,3,4,5,6),
              labels = c("Sem Registro","Grupo 1","Grupo 2","Grupo 3","Grupo 4","Grupo 5","Grupo 6"))

maps$SLI <- cut(maps$dendSLI, breaks = c(-2,0,1,2,3,4,5,6),
                labels = c("Sem Registro","Grupo 1","Grupo 2","Grupo 3","Grupo 4","Grupo 5","Grupo 6"))

ggplot(maps) +
  geom_sf(aes(fill=WAV), color = NA) +
  labs(subtitle="Wikiaves", size=8)+ 
  scale_fill_manual (values = c('#f0f0f0','#B646C7','#228B00','#9F7000','#0082CE','#CC476B','#009681'))+
  labs(fill = " ")+
  theme_minimal()

ggplot(maps) +
  geom_sf(aes(fill=SLI), color = NA) +
  labs(subtitle="SpeciesLink", size=8)+ 
  scale_fill_manual (values = c('#f0f0f0','#B646C7','#228B00','#9F7000','#0082CE','#CC476B','#009681'))+
  labs(fill = " ")+
  theme_minimal()


rm(list = ls())



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

# Todos os Municípios

WAV <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S6/WAV-E.xlsx", sheet = "Geral")

WAV <- t(WAV)
is.numeric(WAV)

SLI <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S6/SPL-E.xlsx", sheet = "Geral")

SLI <- t(SLI)
is.numeric(SLI)

dados <- read.xlsx("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S6/Mapas.xlsx")
mc <- read_municipality(code_muni= "SP")

maps <- merge(mc,dados)

A <- vegdist(WAV, method = "jaccard", binary = TRUE)
B <- vegdist(SLI, method = "jaccard", binary = TRUE)

hc1 <- hclust(A, method = "ward.D2" )
hc2 <- hclust(B, method = "ward.D2" )

gap_stat1 <- clusGap(WAV, FUN = hcut, K.max = 25, B = 10)
fviz_gap_stat(gap_stat1)
gap_stat2 <- clusGap(SLI, FUN = hcut, K.max = 25, B = 10)
fviz_gap_stat(gap_stat2)

dend1 <- as.dendrogram (hc1)
dend2 <- as.dendrogram (hc2)

entanglement(dend1, 
             dend2,
             L = 1,
             leaves_matching_method = c("order"))

## K = 2

tanglegram(dend1, dend2,
           highlight_distinct_edges = FALSE, 
           common_subtrees_color_lines = TRUE, 
           common_subtrees_color_branches = FALSE,
           main_left = "Wikiaves",
           main_right = "SpeciesLink",
           lwd = .5,
           k_branches = 2)


grp1 <- cutree(hc1, k = 2)
grp1

grp2 <- cutree(hc2, k = 2)
grp2

maps$WAV2 <- cut(maps$dendWAV2, breaks = c(-2,0,1,2),
                 labels = c("Sem Registro","Grupo 1","Grupo 2"))

maps$SLI2 <- cut(maps$dendSLI2, breaks = c(-2,0,1,2),
                 labels = c("Sem Registro","Grupo 1","Grupo 2"))

ggplot(maps) +
  geom_sf(aes(fill=WAV2), color = NA) +
  labs(subtitle="Wikiaves", size=8)+ 
  scale_fill_manual (values = c('#f0f0f0','#009681','#CC476B'))+
  labs(fill = " ")+
  theme_minimal()

ggplot(maps) +
  geom_sf(aes(fill=SLI2), color = NA) +
  labs(subtitle="SpeciesLink", size=8)+ 
  scale_fill_manual (values = c('#f0f0f0','#009681','#CC476B'))+
  labs(fill = " ")+
  theme_minimal()



## K = 3

tanglegram(dend1, dend2,
           highlight_distinct_edges = FALSE, 
           common_subtrees_color_lines = TRUE, 
           common_subtrees_color_branches = FALSE,
           main_left = "Wikiaves",
           main_right = "SpeciesLink",
           lwd = .5,
           k_branches = 3)


grp1 <- cutree(hc1, k = 3)
grp1

grp2 <- cutree(hc2, k = 3)
grp2

maps$WAV3 <- cut(maps$dendWAV3, breaks = c(-2,0,1,2,3),
                 labels = c("Sem Registro","Grupo 1","Grupo 2","Grupo 3"))

maps$SLI3 <- cut(maps$dendSLI3, breaks = c(-2,0,1,2,3),
                 labels = c("Sem Registro","Grupo 1","Grupo 2","Grupo 3"))

ggplot(maps) +
  geom_sf(aes(fill=WAV3), color = NA) +
  labs(subtitle="Wikiaves", size=8)+ 
  scale_fill_manual (values = c('#f0f0f0','#0082CE','#228B00','#CC476B'))+
  labs(fill = " ")+
  theme_minimal()

ggplot(maps) +
  geom_sf(aes(fill=SLI3), color = NA) +
  labs(subtitle="SpeciesLink", size=8)+ 
  scale_fill_manual (values = c('#f0f0f0','#0082CE','#228B00','#CC476B'))+
  labs(fill = " ")+
  theme_minimal()

## K = 4

tanglegram(dend1, dend2,
           highlight_distinct_edges = FALSE, 
           common_subtrees_color_lines = TRUE, 
           common_subtrees_color_branches = FALSE,
           main_left = "Wikiaves",
           main_right = "SpeciesLink",
           lwd = .5,
           k_branches = 4)


grp1 <- cutree(hc1, k = 4)
grp1

grp2 <- cutree(hc2, k = 4)
grp2

maps$WAV4 <- cut(maps$dendWAV4, breaks = c(-2,0,1,2,3,4),
                 labels = c("Sem Registro","Grupo 1","Grupo 2","Grupo 3","Grupo 4"))

maps$SLI4 <- cut(maps$dendSLI4, breaks = c(-2,0,1,2,3,4),
                 labels = c("Sem Registro","Grupo 1","Grupo 2","Grupo 3","Grupo 4"))

ggplot(maps) +
  geom_sf(aes(fill=WAV4), color = NA) +
  labs(subtitle="Wikiaves", size=8)+ 
  scale_fill_manual (values = c('#f0f0f0','#B646C7','#228B00','#9F7000','#0082CE','#CC476B','#009681'))+
  labs(fill = " ")+
  theme_minimal()

ggplot(maps) +
  geom_sf(aes(fill=SLI4), color = NA) +
  labs(subtitle="SpeciesLink", size=8)+ 
  scale_fill_manual (values = c('#f0f0f0','#B646C7','#228B00','#9F7000','#0082CE','#CC476B','#009681'))+
  labs(fill = " ")+
  theme_minimal()

## K = 5

tanglegram(dend1, dend2,
           highlight_distinct_edges = FALSE, 
           common_subtrees_color_lines = TRUE, 
           common_subtrees_color_branches = FALSE,
           main_left = "Wikiaves",
           main_right = "SpeciesLink",
           lwd = .5,
           k_branches = 5)


grp1 <- cutree(hc1, k = 5)
grp1

grp2 <- cutree(hc2, k = 5)
grp2

maps$WAV5 <- cut(maps$dendWAV5, breaks = c(-2,0,1,2,3,4,5),
                 labels = c("Sem Registro","Grupo 1","Grupo 2","Grupo 3","Grupo 4","Grupo 5"))

maps$SLI5 <- cut(maps$dendSLI5, breaks = c(-2,0,1,2,3,4,5),
                 labels = c("Sem Registro","Grupo 1","Grupo 2","Grupo 3","Grupo 4","Grupo 5"))

ggplot(maps) +
  geom_sf(aes(fill=WAV5), color = NA) +
  labs(subtitle="Wikiaves", size=8)+ 
  scale_fill_manual (values = c('#f0f0f0','#B646C7','#228B00','#9F7000','#0082CE','#CC476B','#009681'))+
  labs(fill = " ")+
  theme_minimal()

ggplot(maps) +
  geom_sf(aes(fill=SLI5), color = NA) +
  labs(subtitle="SpeciesLink", size=8)+ 
  scale_fill_manual (values = c('#f0f0f0','#B646C7','#228B00','#9F7000','#0082CE','#CC476B','#009681'))+
  labs(fill = " ")+
  theme_minimal()

## K = 6

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


maps$WAV6 <- cut(maps$dendWAV6, breaks = c(-2,0,1,2,3,4,5,6),
              labels = c("Sem Registro","Grupo 1","Grupo 2","Grupo 3","Grupo 4","Grupo 5","Grupo 6"))

maps$SLI6 <- cut(maps$dendSLI6, breaks = c(-2,0,1,2,3,4,5,6),
                labels = c("Sem Registro","Grupo 1","Grupo 2","Grupo 3","Grupo 4","Grupo 5","Grupo 6"))

ggplot(maps) +
  geom_sf(aes(fill=WAV6), color = NA) +
  labs(subtitle="Wikiaves", size=8)+ 
  scale_fill_manual (values = c('#f0f0f0','#B646C7','#228B00','#9F7000','#0082CE','#CC476B','#009681'))+
  labs(fill = " ")+
  theme_minimal()

ggplot(maps) +
  geom_sf(aes(fill=SLI6), color = NA) +
  labs(subtitle="SpeciesLink", size=8)+ 
  scale_fill_manual (values = c('#f0f0f0','#B646C7','#228B00','#9F7000','#0082CE','#CC476B','#009681'))+
  labs(fill = " ")+
  theme_minimal()

## K = 7

tanglegram(dend1, dend2,
           highlight_distinct_edges = FALSE, 
           common_subtrees_color_lines = TRUE, 
           common_subtrees_color_branches = FALSE,
           main_left = "Wikiaves",
           main_right = "SpeciesLink",
           lwd = .5,
           k_branches = 7)

grp1 <- cutree(hc1, k = 7)
grp1

grp2 <- cutree(hc2, k = 7)
grp2

## K = 8

tanglegram(dend1, dend2,
           highlight_distinct_edges = FALSE, 
           common_subtrees_color_lines = TRUE, 
           common_subtrees_color_branches = FALSE,
           main_left = "Wikiaves",
           main_right = "SpeciesLink",
           lwd = .5,
           k_branches = 8)


grp1 <- cutree(hc1, k = 8)
grp1

grp2 <- cutree(hc2, k = 8)
grp2

## K = 9

tanglegram(dend1, dend2,
           highlight_distinct_edges = FALSE, 
           common_subtrees_color_lines = TRUE, 
           common_subtrees_color_branches = FALSE,
           main_left = "Wikiaves",
           main_right = "SpeciesLink",
           lwd = .5,
           k_branches = 9)


grp1 <- cutree(hc1, k = 9)
grp1

grp2 <- cutree(hc2, k = 9)
grp2


# Top 50

# Top 20

# Top 10


rm(list = ls())



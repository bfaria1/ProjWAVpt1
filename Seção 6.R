library(gclus)
library(tidyverse)  
library(cluster)   
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
WAV <- WAV[-1,]
is.numeric(WAV)

SLI <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S6/SPL-E.xlsx", sheet = "Geral")

SLI <- t(SLI)
SLI <- SLI[-1,]
is.numeric(SLI)

A <- vegdist(WAV, method = "jaccard", binary = TRUE)
B <- vegdist(SLI, method = "jaccard", binary = TRUE)

hc1 <- hclust(A, method = "Ward.D2" )
hc2 <- hclust(B, method = "ward.D2" )

gap_stat1 <- clusGap(WAV, FUN = hcut, K.max = 25, B = 10)
fviz_gap_stat(gap_stat1)
gap_stat2 <- clusGap(SLI, FUN = hcut, K.max = 25, B = 10)
fviz_gap_stat(gap_stat2)

plot(hc1, cex = 0.6, hang = -1)
rect.hclust(hc1, k = 25, border = 2:5)

plot(hc2, cex = 0.6, hang = -1)
rect.hclust(hc2, k = 25, border = 2:5)

dend1 <- as.dendrogram (hc1)
dend2 <- as.dendrogram (hc2)

dend_list <- dendlist(dend1, dend2)


tanglegram(dend1, dend2,
           highlight_distinct_edges = FALSE, 
           common_subtrees_color_lines = FALSE, 
           common_subtrees_color_branches = TRUE,
           main_left = "Wikiaves",
           main_right = "SpeciesLink",
           lwd = .5)
           #main = paste("entanglement =", round(entanglement(dendlist), 2))



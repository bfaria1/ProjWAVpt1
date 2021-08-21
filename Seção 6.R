library(pacman)

pacman::p_load(tidyverse, cluster, gclus, factoextra, dendextend, sf, psych, geobr, ggplot2, openxlsx, ade4, vegan, car, rstatix, multcomp, emmeans, RVAideMemoire, psych, DescTools)

WAV1 <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S6/Dados.xlsx", sheet = "WAV1")
WAV1 <- t(WAV1)
is.numeric(WAV1)

WAV2 <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S6/Dados.xlsx", sheet = "WAV2")
WAV2 <- t(WAV2)
is.numeric(WAV2)

SLI1 <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S6/Dados.xlsx", sheet = "SLI1")
SLI1 <- t(SLI1)
is.numeric(SLI1)

SLI2 <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S6/Dados.xlsx", sheet = "SLI2")
SLI2 <- t(SLI2)
is.numeric(SLI2)

dados1 <-read.xlsx("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S6/Dados.xlsx", sheet = "N71")
dados2 <-read.xlsx("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S6/Dados.xlsx", sheet = "N46")

mapas<-read.xlsx("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S6/Dados.xlsx", sheet = "Mapas")
mc <- read_municipality(code_muni= "SP")

FE1 <- dados1[,12:16]
ALX <- dados1[,12:12]
ARX <- dados1[,13:13]
POPX <- dados1[,14:14]
LAX <- dados1[,15:15]
LOX <- dados1[,16:16]
LALOX <- dados1[,15:16]

FE2 <- dados2[,12:16]
ALY <- dados2[,12:12]
ARY <- dados2[,13:13]
POPY <- dados2[,14:14]
LAY <- dados2[,15:15]
LOY <- dados2[,16:16]
LALOY <- dados2[,15:16]

mapas <- merge(mc,mapas)

A1 <- vegdist(WAV1, method = "jaccard", binary = TRUE)
B1 <- vegdist(SLI1, method = "jaccard", binary = TRUE)
A2 <- vegdist(WAV2, method = "jaccard", binary = TRUE)
B2 <- vegdist(SLI2, method = "jaccard", binary = TRUE)


dist.amb1 <- vegdist(FE1, method = "euclid")
dist.geo1 <- vegdist(LALOX, method = "euclid")

AL1 <- vegdist(ALX, method = "euclid")
AR1 <- vegdist(ARX, method = "euclid")
POP1 <- vegdist(POPX, method = "euclid")
LA1 <- vegdist(LAX, method = "euclid")
LO1 <- vegdist(LOX, method = "euclid")

dist.amb2 <- vegdist(FE2, method = "euclid")
dist.geo2 <- vegdist(LALOY, method = "euclid")

AL2 <- vegdist(ALY, method = "euclid")
AR2 <- vegdist(ARY, method = "euclid")
POP2 <- vegdist(POPY, method = "euclid")
LA2 <- vegdist(LAY, method = "euclid")
LO2 <- vegdist(LOY, method = "euclid")

CA1 <- hclust(A1, method = "ward.D2")
CB1 <- hclust(B1, method = "ward.D2")
CA2 <- hclust(A2, method = "ward.D2")
CB2 <- hclust(B2, method = "ward.D2")

dendA1 <- as.dendrogram (CA1)
dendA2 <- as.dendrogram (CA2)
dendB1 <- as.dendrogram (CB1)
dendB2 <- as.dendrogram (CB2)

# N = 71


mantel(A1, B1, permutations = 10000)

mantel(dist.amb1, A1, permutations = 10000)
mantel(dist.amb1, B1, permutations = 10000)

mantel(dist.geo1, A1, permutations = 10000)
mantel(dist.geo1, B1, permutations = 10000)

mantel(AL1, A1, permutations = 10000)
mantel(AL1, B1, permutations = 10000)

mantel(AR1, A1, permutations = 10000)
mantel(AR1, B1, permutations = 10000)

mantel(POP1, A1, permutations = 10000)
mantel(POP1, B1, permutations = 10000)

mantel(LA1, A1, permutations = 10000)
mantel(LA1, B1, permutations = 10000)

mantel(LO1, A1, permutations = 10000)
mantel(LO1, B1, permutations = 10000)

## K = 2

grp1 <- cutree(CA1, k = 2)
grp1

grp2 <- cutree(CB1, k = 2)
grp2

oneway.test(AL ~ WK2, data = dados1, var.equal=FALSE)
oneway.test(AR ~ WK2, data = dados1, var.equal=FALSE)
oneway.test(POP ~ WK2, data = dados1, var.equal=FALSE)
oneway.test(LA ~ WK2, data = dados1, var.equal=FALSE)
oneway.test(LO ~ WK2, data = dados1, var.equal=FALSE)

oneway.test(AL ~ SK2, data = dados1, var.equal=FALSE)
oneway.test(AR ~ SK2, data = dados1, var.equal=FALSE)
oneway.test(POP ~ SK2, data = dados1, var.equal=FALSE)
oneway.test(LA ~ SK2, data = dados1, var.equal=FALSE)
oneway.test(LO ~ SK2, data = dados1, var.equal=FALSE)

mapas$N1WK2 <- factor(mapas$N1WK2, levels=c("Sem Registro", "Grupo 1", "Grupo 2"))

mapas$N1SK2 <- factor(mapas$N1SK2, levels=c("Sem Registro", "Grupo 1", "Grupo 2"))

png("01.png", width = 1500, height = 2000)
tanglegram(dendA1, dendB1,
           highlight_distinct_edges = FALSE, 
           common_subtrees_color_lines = TRUE, 
           common_subtrees_color_branches = FALSE,
           main_left = "Wikiaves",
           main_right = "SpeciesLink",
           lwd = .5,
           k_branches = 2)
dev.off()

png("02.png", width = 600, height = 300)
ggplot(mapas) +
  geom_sf(aes(fill=N1WK2), color = NA) +
  labs(subtitle="Wikiaves", size=8)+ 
  scale_fill_manual (values = c('#f0f0f0','#009681','#CC476B'))+
  labs(fill = " ")+
  theme_minimal()
dev.off()

png("03.png", width = 600, height = 300)
ggplot(mapas) +
  geom_sf(aes(fill=N1SK2), color = NA) +
  labs(subtitle="SpeciesLink", size=8)+ 
  scale_fill_manual (values = c('#f0f0f0','#009681','#CC476B'))+
  labs(fill = " ")+
  theme_minimal()
dev.off()

## K = 3

grp1 <- cutree(CA1, k = 3)
grp1

grp2 <- cutree(CB1, k = 3)
grp2

oneway.test(AL ~ WK3, data = dados1, var.equal=FALSE)
oneway.test(AR ~ WK3, data = dados1, var.equal=FALSE)
oneway.test(POP ~ WK3, data = dados1, var.equal=FALSE)
oneway.test(LA ~ WK3, data = dados1, var.equal=FALSE)
oneway.test(LO ~ WK3, data = dados1, var.equal=FALSE)

oneway.test(AL ~ SK3, data = dados1, var.equal=FALSE)
oneway.test(AR ~ SK3, data = dados1, var.equal=FALSE)
oneway.test(POP ~ SK3, data = dados1, var.equal=FALSE)
oneway.test(LA ~ SK3, data = dados1, var.equal=FALSE)
oneway.test(LO ~ SK3, data = dados1, var.equal=FALSE)

mapas$N1WK3 <- factor(mapas$N1WK3, levels=c("Sem Registro", "Grupo 1", "Grupo 2","Grupo 3"))

mapas$N1SK3 <- factor(mapas$N1SK3, levels=c("Sem Registro", "Grupo 1", "Grupo 2","Grupo 3"))

png("04.png", width = 1500, height = 2000)
tanglegram(dendA1, dendB1,
           highlight_distinct_edges = FALSE, 
           common_subtrees_color_lines = TRUE, 
           common_subtrees_color_branches = FALSE,
           main_left = "Wikiaves",
           main_right = "SpeciesLink",
           lwd = .5,
           k_branches = 3)
dev.off()

png("05.png", width = 600, height = 300)
ggplot(mapas) +
  geom_sf(aes(fill=N1WK3), color = NA) +
  labs(subtitle="Wikiaves", size=8)+ 
  scale_fill_manual (values = c('#f0f0f0','#0082CE','#228B00','#CC476B'))+
  labs(fill = " ")+
  theme_minimal()
dev.off()

png("06.png", width = 600, height = 300)
ggplot(mapas) +
  geom_sf(aes(fill=N1SK3), color = NA) +
  labs(subtitle="SpeciesLink", size=8)+ 
  scale_fill_manual (values = c('#f0f0f0','#0082CE','#228B00','#CC476B'))+
  labs(fill = " ")+
  theme_minimal()
dev.off()

## K = 4

grp1 <- cutree(CA1, k = 4)
grp1

grp2 <- cutree(CB1, k = 4)
grp2

oneway.test(AL ~ WK4, data = dados1, var.equal=FALSE)
oneway.test(AR ~ WK4, data = dados1, var.equal=FALSE)
oneway.test(POP ~ WK4, data = dados1, var.equal=FALSE)
oneway.test(LA ~ WK4, data = dados1, var.equal=FALSE)
oneway.test(LO ~ WK4, data = dados1, var.equal=FALSE)

oneway.test(AL ~ SK4, data = dados1, var.equal=FALSE)
oneway.test(AR ~ SK4, data = dados1, var.equal=FALSE)
oneway.test(POP ~ SK4, data = dados1, var.equal=FALSE)
oneway.test(LA ~ SK4, data = dados1, var.equal=FALSE)
oneway.test(LO ~ SK4, data = dados1, var.equal=FALSE)

mapas$N1WK4 <- factor(mapas$N1WK4, levels=c("Sem Registro", "Grupo 1", "Grupo 2","Grupo 3", "Grupo 4"))

mapas$N1SK4 <- factor(mapas$N1SK4, levels=c("Sem Registro", "Grupo 1", "Grupo 2","Grupo 3", "Grupo 4"))

png("07.png", width = 1500, height = 2000)
tanglegram(dendA1, dendB1,
           highlight_distinct_edges = FALSE, 
           common_subtrees_color_lines = TRUE, 
           common_subtrees_color_branches = FALSE,
           main_left = "Wikiaves",
           main_right = "SpeciesLink",
           lwd = .5,
           k_branches = 4)
dev.off()

png("08.png", width = 600, height = 300)
ggplot(mapas) +
  geom_sf(aes(fill=N1WK4), color = NA) +
  labs(subtitle="Wikiaves", size=8)+ 
  scale_fill_manual (values = c('#f0f0f0','#7866D8','#767F00','#CC476B','#009681'))+
  labs(fill = " ")+
  theme_minimal()
dev.off()

png("09.png", width = 600, height = 300)
ggplot(mapas) +
  geom_sf(aes(fill=N1SK4), color = NA) +
  labs(subtitle="SpeciesLink", size=8)+ 
  scale_fill_manual (values = c('#f0f0f0','#7866D8','#767F00','#CC476B','#009681'))+
  labs(fill = " ")+
  theme_minimal()
dev.off()


## K = 5

grp1 <- cutree(CA1, k = 5)
grp1

grp2 <- cutree(CB1, k = 5)
grp2

oneway.test(AL ~ WK5, data = dados1, var.equal=FALSE)
oneway.test(AR ~ WK5, data = dados1, var.equal=FALSE)
oneway.test(POP ~ WK5, data = dados1, var.equal=FALSE)
oneway.test(LA ~ WK5, data = dados1, var.equal=FALSE)
oneway.test(LO ~ WK5, data = dados1, var.equal=FALSE)

oneway.test(AL ~ SK5, data = dados1, var.equal=FALSE)
oneway.test(AR ~ SK5, data = dados1, var.equal=FALSE)
oneway.test(POP ~ SK5, data = dados1, var.equal=FALSE)
oneway.test(LA ~ SK5, data = dados1, var.equal=FALSE)
oneway.test(LO ~ SK5, data = dados1, var.equal=FALSE)

mapas$N1WK5 <- factor(mapas$N1WK5, levels=c("Sem Registro", "Grupo 1", "Grupo 2","Grupo 3", "Grupo 4", "Grupo 5"))

mapas$N1SK5 <- factor(mapas$N1SK5, levels=c("Sem Registro", "Grupo 1", "Grupo 2","Grupo 3", "Grupo 4", "Grupo 5"))

png("10.png", width = 1500, height = 2000)
tanglegram(dendA1, dendB1,
           highlight_distinct_edges = FALSE, 
           common_subtrees_color_lines = TRUE, 
           common_subtrees_color_branches = FALSE,
           main_left = "Wikiaves",
           main_right = "SpeciesLink",
           lwd = .5,
           k_branches = 5)
dev.off()

png("11.png", width = 600, height = 300)
ggplot(mapas) +
  geom_sf(aes(fill=N1WK5), color = NA) +
  labs(subtitle="Wikiaves", size=8)+ 
  scale_fill_manual (values = c('#f0f0f0','#B646C7','#228B00','#9F7000','#0082CE','#CC476B'))+
  labs(fill = " ")+
  theme_minimal()
dev.off()

png("12.png", width = 600, height = 300)
ggplot(mapas) +
  geom_sf(aes(fill=N1SK5), color = NA) +
  labs(subtitle="SpeciesLink", size=8)+ 
  scale_fill_manual (values = c('#f0f0f0','#B646C7','#228B00','#9F7000','#0082CE','#CC476B'))+
  labs(fill = " ")+
  theme_minimal()
dev.off()

# N = 46

mantel(A2, B2, permutations = 10000)

mantel(dist.amb2, A2, permutations = 10000)
mantel(dist.amb2, B2, permutations = 10000)

mantel(dist.geo2, A2, permutations = 10000)
mantel(dist.geo2, B2, permutations = 10000)

mantel(AL2, A2, permutations = 10000)
mantel(AL2, B2, permutations = 10000)

mantel(AR2, A2, permutations = 10000)
mantel(AR2, B2, permutations = 10000)

mantel(POP2, A2, permutations = 10000)
mantel(POP2, B2, permutations = 10000)

mantel(LA2, A2, permutations = 10000)
mantel(LA2, B2, permutations = 10000)

mantel(LO2, A2, permutations = 10000)
mantel(LO2, B2, permutations = 10000)

## K = 2

grp1 <- cutree(CA2, k = 2)
grp1

grp2 <- cutree(CB2, k = 2)
grp2

oneway.test(AL ~ WK2, data = dados2, var.equal=FALSE)
oneway.test(AR ~ WK2, data = dados2, var.equal=FALSE)
oneway.test(POP ~ WK2, data = dados2, var.equal=FALSE)
oneway.test(LA ~ WK2, data = dados2, var.equal=FALSE)
oneway.test(LO ~ WK2, data = dados2, var.equal=FALSE)

oneway.test(AL ~ SK2, data = dados2, var.equal=FALSE)
oneway.test(AR ~ SK2, data = dados2, var.equal=FALSE)
oneway.test(POP ~ SK2, data = dados2, var.equal=FALSE)
oneway.test(LA ~ SK2, data = dados2, var.equal=FALSE)
oneway.test(LO ~ SK2, data = dados2, var.equal=FALSE)

mapas$N2WK2 <- factor(mapas$N2WK2, levels=c("Sem Registro", "Grupo 1", "Grupo 2"))

mapas$N2SK2 <- factor(mapas$N2SK2, levels=c("Sem Registro", "Grupo 1", "Grupo 2"))

png("13.png", width = 1500, height = 2000)
tanglegram(dendA2, dendB2,
           highlight_distinct_edges = FALSE, 
           common_subtrees_color_lines = TRUE, 
           common_subtrees_color_branches = FALSE,
           main_left = "Wikiaves",
           main_right = "SpeciesLink",
           lwd = .5,
           k_branches = 2)
dev.off()

png("14.png", width = 600, height = 300)
ggplot(mapas) +
  geom_sf(aes(fill=N2WK2), color = NA) +
  labs(subtitle="Wikiaves", size=8)+ 
  scale_fill_manual (values = c('#f0f0f0','#009681','#CC476B'))+
  labs(fill = " ")+
  theme_minimal()
dev.off()

png("15.png", width = 600, height = 300)
ggplot(mapas) +
  geom_sf(aes(fill=N2SK2), color = NA) +
  labs(subtitle="SpeciesLink", size=8)+ 
  scale_fill_manual (values = c('#f0f0f0','#009681','#CC476B'))+
  labs(fill = " ")+
  theme_minimal()
dev.off()

## K = 3

grp1 <- cutree(CA2, k = 3)
grp1

grp2 <- cutree(CB2, k = 3)
grp2

oneway.test(AL ~ WK3, data = dados2, var.equal=FALSE)
oneway.test(AR ~ WK3, data = dados2, var.equal=FALSE)
oneway.test(POP ~ WK3, data = dados2, var.equal=FALSE)
oneway.test(LA ~ WK3, data = dados2, var.equal=FALSE)
oneway.test(LO ~ WK3, data = dados2, var.equal=FALSE)

oneway.test(AL ~ SK3, data = dados2, var.equal=FALSE)
oneway.test(AR ~ SK3, data = dados2, var.equal=FALSE)
oneway.test(POP ~ SK3, data = dados2, var.equal=FALSE)
oneway.test(LA ~ SK3, data = dados2, var.equal=FALSE)
oneway.test(LO ~ SK3, data = dados2, var.equal=FALSE)

mapas$N2WK3 <- factor(mapas$N2WK3, levels=c("Sem Registro", "Grupo 1", "Grupo 2","Grupo 3"))

mapas$N2SK3 <- factor(mapas$N2SK3, levels=c("Sem Registro", "Grupo 1", "Grupo 2","Grupo 3"))



png("16.png", width = 1500, height = 2000)
tanglegram(dendA2, dendB2,
           highlight_distinct_edges = FALSE, 
           common_subtrees_color_lines = TRUE, 
           common_subtrees_color_branches = FALSE,
           main_left = "Wikiaves",
           main_right = "SpeciesLink",
           lwd = .5,
           k_branches = 3)
dev.off()

png("17.png", width = 600, height = 300)
ggplot(mapas) +
  geom_sf(aes(fill=N2WK3), color = NA) +
  labs(subtitle="Wikiaves", size=8)+ 
  scale_fill_manual (values = c('#f0f0f0','#0082CE','#228B00','#CC476B'))+
  labs(fill = " ")+
  theme_minimal()
dev.off()

png("18.png", width = 600, height = 300)
ggplot(mapas) +
  geom_sf(aes(fill=N2SK3), color = NA) +
  labs(subtitle="SpeciesLink", size=8)+ 
  scale_fill_manual (values = c('#f0f0f0','#0082CE','#228B00','#CC476B'))+
  labs(fill = " ")+
  theme_minimal()
dev.off()

## K = 4

grp1 <- cutree(CA2, k = 4)
grp1

grp2 <- cutree(CB2, k = 4)
grp2

oneway.test(AL ~ WK4, data = dados2, var.equal=FALSE)
oneway.test(AR ~ WK4, data = dados2, var.equal=FALSE)
oneway.test(POP ~ WK4, data = dados2, var.equal=FALSE)
oneway.test(LA ~ WK4, data = dados2, var.equal=FALSE)
oneway.test(LO ~ WK4, data = dados2, var.equal=FALSE)

oneway.test(AL ~ SK4, data = dados2, var.equal=FALSE)
oneway.test(AR ~ SK4, data = dados2, var.equal=FALSE)
oneway.test(POP ~ SK4, data = dados2, var.equal=FALSE)
oneway.test(LA ~ SK4, data = dados2, var.equal=FALSE)
oneway.test(LO ~ SK4, data = dados2, var.equal=FALSE)

mapas$N2WK4 <- factor(mapas$N2WK4, levels=c("Sem Registro", "Grupo 1", "Grupo 2","Grupo 3", "Grupo 4"))

mapas$N2SK4 <- factor(mapas$N2SK4, levels=c("Sem Registro", "Grupo 1", "Grupo 2","Grupo 3", "Grupo 4"))

png("19.png", width = 1500, height = 2000)
tanglegram(dendA2, dendB2,
           highlight_distinct_edges = FALSE, 
           common_subtrees_color_lines = TRUE, 
           common_subtrees_color_branches = FALSE,
           main_left = "Wikiaves",
           main_right = "SpeciesLink",
           lwd = .5,
           k_branches = 4)
dev.off()

png("20.png", width = 600, height = 300)
ggplot(mapas) +
  geom_sf(aes(fill=N2WK4), color = NA) +
  labs(subtitle="Wikiaves", size=8)+ 
  scale_fill_manual (values = c('#f0f0f0','#7866D8','#767F00','#CC476B','#009681'))+
  labs(fill = " ")+
  theme_minimal()
dev.off()

png("21.png", width = 600, height = 300)
ggplot(mapas) +
  geom_sf(aes(fill=N2SK4), color = NA) +
  labs(subtitle="SpeciesLink", size=8)+ 
  scale_fill_manual (values = c('#f0f0f0','#7866D8','#767F00','#CC476B','#009681'))+
  labs(fill = " ")+
  theme_minimal()
dev.off()


## K = 5

grp1 <- cutree(CA2, k = 5)
grp1

grp2 <- cutree(CB2, k = 5)
grp2

oneway.test(AL ~ WK5, data = dados2, var.equal=FALSE)
oneway.test(AR ~ WK5, data = dados2, var.equal=FALSE)
oneway.test(POP ~ WK5, data = dados2, var.equal=FALSE)
oneway.test(LA ~ WK5, data = dados2, var.equal=FALSE)
oneway.test(LO ~ WK5, data = dados2, var.equal=FALSE)

oneway.test(AL ~ SK5, data = dados2, var.equal=FALSE)
oneway.test(AR ~ SK5, data = dados2, var.equal=FALSE)
oneway.test(POP ~ SK5, data = dados2, var.equal=FALSE)
oneway.test(LA ~ SK5, data = dados2, var.equal=FALSE)
oneway.test(LO ~ SK5, data = dados2, var.equal=FALSE)

mapas$N2WK5 <- factor(mapas$N2WK5, levels=c("Sem Registro", "Grupo 1", "Grupo 2","Grupo 3", "Grupo 4", "Grupo 5"))

mapas$N2SK5 <- factor(mapas$N2SK5, levels=c("Sem Registro", "Grupo 1", "Grupo 2","Grupo 3", "Grupo 4", "Grupo 5"))

png("22.png", width = 1500, height = 2000)
tanglegram(dendA2, dendB2,
           highlight_distinct_edges = FALSE, 
           common_subtrees_color_lines = TRUE, 
           common_subtrees_color_branches = FALSE,
           main_left = "Wikiaves",
           main_right = "SpeciesLink",
           lwd = .5,
           k_branches = 5)
dev.off()

png("23.png", width = 600, height = 300)
ggplot(mapas) +
  geom_sf(aes(fill=N2WK5), color = NA) +
  labs(subtitle="Wikiaves", size=8)+ 
  scale_fill_manual (values = c('#f0f0f0','#B646C7','#228B00','#9F7000','#0082CE','#CC476B'))+
  labs(fill = " ")+
  theme_minimal()
dev.off()

png("24.png", width = 600, height = 300)
ggplot(mapas) +
  geom_sf(aes(fill=N2SK5), color = NA) +
  labs(subtitle="SpeciesLink", size=8)+ 
  scale_fill_manual (values = c('#f0f0f0','#B646C7','#228B00','#9F7000','#0082CE','#CC476B'))+
  labs(fill = " ")+
  theme_minimal()
dev.off()

rm(list = ls())



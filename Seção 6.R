library(pacman)

pacman::p_load(tidyverse, cluster, gclus, factoextra, dendextend, sf, psych, geobr, ggplot2, openxlsx, ade4, vegan, car, rstatix, multcomp, emmeans, RVAideMemoire, psych, DescTools)

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

N1 <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S6/Grupos.xlsx", sheet = "K = 2")
N2 <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S6/Grupos.xlsx", sheet = "K = 3")
N3 <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S6/Grupos.xlsx", sheet = "K = 4")
N4 <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S6/Grupos.xlsx", sheet = "K = 5")
N5 <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S6/Grupos.xlsx", sheet = "K = 6")

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

png("6112.png", width = 1500, height = 2000)
tanglegram(dend1, dend2,
           highlight_distinct_edges = FALSE, 
           common_subtrees_color_lines = TRUE, 
           common_subtrees_color_branches = FALSE,
           main_left = "Wikiaves",
           main_right = "SpeciesLink",
           lwd = .5,
           k_branches = 2)
dev.off()

grp1 <- cutree(hc1, k = 2)
grp1

grp2 <- cutree(hc2, k = 2)
grp2

maps$WAV2 <- cut(maps$dendWAV2, breaks = c(-2,0,1,2),
                 labels = c("Sem Registro","Grupo 1","Grupo 2"))

maps$SLI2 <- cut(maps$dendSLI2, breaks = c(-2,0,1,2),
                 labels = c("Sem Registro","Grupo 1","Grupo 2"))

png("61221.png", width = 600, height = 300)
ggplot(maps) +
  geom_sf(aes(fill=WAV2), color = NA) +
  labs(subtitle="Wikiaves", size=8)+ 
  scale_fill_manual (values = c('#f0f0f0','#009681','#CC476B'))+
  labs(fill = " ")+
  theme_minimal()
dev.off()


png("61222.png", width = 600, height = 300)
ggplot(maps) +
  geom_sf(aes(fill=SLI2), color = NA) +
  labs(subtitle="SpeciesLink", size=8)+ 
  scale_fill_manual (values = c('#f0f0f0','#009681','#CC476B'))+
  labs(fill = " ")+
  theme_minimal()
dev.off()



byf.shapiro(WAL ~ Grupo.W, N1)
byf.shapiro(WAR ~ Grupo.W, N1)
byf.shapiro(WP ~ Grupo.W, N1)
byf.shapiro(WLA ~ Grupo.W, N1)
byf.shapiro(WLO ~ Grupo.W, N1)

byf.shapiro(SAL ~ Grupo.S, N1)
byf.shapiro(SAR ~ Grupo.S, N1)
byf.shapiro(SP ~ Grupo.S, N1)
byf.shapiro(SLA ~ Grupo.S, N1)
byf.shapiro(SLO ~ Grupo.S, N1)

leveneTest(WAL ~ Grupo.W, N1, mean)
leveneTest(WAR ~ Grupo.W, N1, mean)
leveneTest(WP ~ Grupo.W, N1, mean)
leveneTest(WLA ~ Grupo.W, N1, mean)
leveneTest(WLO ~ Grupo.W, N1, mean)

leveneTest(SAL ~ Grupo.S, N1, mean)
leveneTest(SAR ~ Grupo.S, N1, mean)
leveneTest(SP ~ Grupo.S, N1, mean)
leveneTest(SLA ~ Grupo.S, N1, mean)
leveneTest(SLO ~ Grupo.S, N1, mean)


oneway.test(WAL ~ Grupo.W, data = N1, var.equal=FALSE)
oneway.test(WAR ~ Grupo.W, data = N1, var.equal=TRUE)
oneway.test(WP ~ Grupo.W, data = N1, var.equal=TRUE)
oneway.test(WLA ~ Grupo.W, data = N1, var.equal=FALSE)
oneway.test(WLO ~ Grupo.W, data = N1, var.equal=TRUE)

oneway.test(SAL ~ Grupo.S, data = N1, var.equal=FALSE)
oneway.test(SAR ~ Grupo.S, data = N1, var.equal=TRUE)
oneway.test(SP ~ Grupo.S, data = N1, var.equal=TRUE)
oneway.test(SLA ~ Grupo.S, data = N1, var.equal=TRUE)
oneway.test(SLO ~ Grupo.S, data = N1, var.equal=FALSE)


mod <- aov(WAL ~ Grupo.W, N1)
PostHocTest(mod, method = "hsd")
mod <- aov(WAR ~ Grupo.W, N1)
PostHocTest(mod, method = "hsd")
mod <- aov(WP ~ Grupo.W, N1)
PostHocTest(mod, method = "hsd")
mod <- aov(WLA ~ Grupo.W, N1)
PostHocTest(mod, method = "hsd")
mod <- aov(WLO ~ Grupo.W, N1)
PostHocTest(mod, method = "hsd")

mod <- aov(SAL ~ Grupo.S, N1)
PostHocTest(mod, method = "hsd")
mod <- aov(SAR ~ Grupo.S, N1)
PostHocTest(mod, method = "hsd")
mod <- aov(SP ~ Grupo.S, N1)
PostHocTest(mod, method = "hsd")
mod <- aov(SLA ~ Grupo.S, N1)
PostHocTest(mod, method = "hsd")
mod <- aov(SLO ~ Grupo.S, N1)
PostHocTest(mod, method = "hsd")

## K = 3

png("6113.png", width = 1500, height = 2000)
tanglegram(dend1, dend2,
           highlight_distinct_edges = FALSE, 
           common_subtrees_color_lines = TRUE, 
           common_subtrees_color_branches = FALSE,
           main_left = "Wikiaves",
           main_right = "SpeciesLink",
           lwd = .5,
           k_branches = 3)
dev.off()


grp1 <- cutree(hc1, k = 3)
grp1

grp2 <- cutree(hc2, k = 3)
grp2

maps$WAV3 <- cut(maps$dendWAV3, breaks = c(-2,0,1,2,3),
                 labels = c("Sem Registro","Grupo 1","Grupo 2","Grupo 3"))

maps$SLI3 <- cut(maps$dendSLI3, breaks = c(-2,0,1,2,3),
                 labels = c("Sem Registro","Grupo 1","Grupo 2","Grupo 3"))

png("61231.png", width = 600, height = 300)
ggplot(maps) +
  geom_sf(aes(fill=WAV3), color = NA) +
  labs(subtitle="Wikiaves", size=8)+ 
  scale_fill_manual (values = c('#f0f0f0','#0082CE','#228B00','#CC476B'))+
  labs(fill = " ")+
  theme_minimal()
dev.off()

png("612231.png", width = 600, height = 300)
ggplot(maps) +
  geom_sf(aes(fill=SLI3), color = NA) +
  labs(subtitle="SpeciesLink", size=8)+ 
  scale_fill_manual (values = c('#f0f0f0','#0082CE','#228B00','#CC476B'))+
  labs(fill = " ")+
  theme_minimal()
dev.off()

byf.shapiro(WAL ~ Grupo.W, N2)
byf.shapiro(WAR ~ Grupo.W, N2)
byf.shapiro(WP ~ Grupo.W, N2)
byf.shapiro(WLA ~ Grupo.W, N2)
byf.shapiro(WLO ~ Grupo.W, N2)

byf.shapiro(SAL ~ Grupo.S, N2)
byf.shapiro(SAR ~ Grupo.S, N2)
byf.shapiro(SP ~ Grupo.S, N2)
byf.shapiro(SLA ~ Grupo.S, N2)
byf.shapiro(SLO ~ Grupo.S, N2)

leveneTest(WAL ~ Grupo.W, N2, mean)
leveneTest(WAR ~ Grupo.W, N2, mean)
leveneTest(WP ~ Grupo.W, N2, mean)
leveneTest(WLA ~ Grupo.W, N2, mean)
leveneTest(WLO ~ Grupo.W, N2, mean)

leveneTest(SAL ~ Grupo.S, N2, mean)
leveneTest(SAR ~ Grupo.S, N2, mean)
leveneTest(SP ~ Grupo.S, N2, mean)
leveneTest(SLA ~ Grupo.S, N2, mean)
leveneTest(SLO ~ Grupo.S, N2, mean)

oneway.test(WAL ~ Grupo.W, data = N2, var.equal=FALSE)
oneway.test(WAR ~ Grupo.W, data = N2, var.equal=TRUE)
oneway.test(WP ~ Grupo.W, data = N2, var.equal=TRUE)
oneway.test(WLA ~ Grupo.W, data = N2, var.equal=FALSE)
oneway.test(WLO ~ Grupo.W, data = N2, var.equal=FALSE)

oneway.test(SAL ~ Grupo.S, data = N2, var.equal=FALSE)
oneway.test(SAR ~ Grupo.S, data = N2, var.equal=FALSE)
oneway.test(SP ~ Grupo.S, data = N2, var.equal= FALSE)
oneway.test(SLA ~ Grupo.S, data = N2, var.equal=FALSE)
oneway.test(SLO ~ Grupo.S, data = N2, var.equal=TRUE)


mod <- aov(WAL ~ Grupo.W, N2)
PostHocTest(mod, method = "hsd")
mod <- aov(WAR ~ Grupo.W, N2)
PostHocTest(mod, method = "hsd")
mod <- aov(WP ~ Grupo.W, N2)
PostHocTest(mod, method = "hsd")
mod <- aov(WLA ~ Grupo.W, N2)
PostHocTest(mod, method = "hsd")
mod <- aov(WLO ~ Grupo.W, N2)
PostHocTest(mod, method = "hsd")

mod <- aov(SAL ~ Grupo.S, N2)
PostHocTest(mod, method = "hsd")
mod <- aov(SAR ~ Grupo.S, N2)
PostHocTest(mod, method = "hsd")
mod <- aov(SP ~ Grupo.S, N2)
PostHocTest(mod, method = "hsd")
mod <- aov(SLA ~ Grupo.S, N2)
PostHocTest(mod, method = "hsd")
mod <- aov(SLO ~ Grupo.S, N2)
PostHocTest(mod, method = "hsd")


## K = 4

png("6114.png", width = 1500, height = 2000)
tanglegram(dend1, dend2,
           highlight_distinct_edges = FALSE, 
           common_subtrees_color_lines = TRUE, 
           common_subtrees_color_branches = FALSE,
           main_left = "Wikiaves",
           main_right = "SpeciesLink",
           lwd = .5,
           k_branches = 4)
dev.off()

grp1 <- cutree(hc1, k = 4)
grp1

grp2 <- cutree(hc2, k = 4)
grp2

maps$WAV4 <- cut(maps$dendWAV4, breaks = c(-2,0,1,2,3,4),
                 labels = c("Sem Registro","Grupo 1","Grupo 2","Grupo 3","Grupo 4"))

maps$SLI4 <- cut(maps$dendSLI4, breaks = c(-2,0,1,2,3,4),
                 labels = c("Sem Registro","Grupo 1","Grupo 2","Grupo 3","Grupo 4"))

png("61241.png", width = 600, height = 300)
ggplot(maps) +
  geom_sf(aes(fill=WAV4), color = NA) +
  labs(subtitle="Wikiaves", size=8)+ 
  scale_fill_manual (values = c('#f0f0f0','#7866D8','#767F00','#CC476B','#009681'))+
  labs(fill = " ")+
  theme_minimal()
dev.off()

png("61242.png", width = 600, height = 300)
ggplot(maps) +
  geom_sf(aes(fill=SLI4), color = NA) +
  labs(subtitle="SpeciesLink", size=8)+ 
  scale_fill_manual (values = c('#f0f0f0','#7866D8','#767F00','#CC476B','#009681'))+
  labs(fill = " ")+
  theme_minimal()
dev.off()

byf.shapiro(WAL ~ Grupo.W, N3)
byf.shapiro(WAR ~ Grupo.W, N3)
byf.shapiro(WP ~ Grupo.W, N3)
byf.shapiro(WLA ~ Grupo.W, N3)
byf.shapiro(WLO ~ Grupo.W, N3)

byf.shapiro(SAL ~ Grupo.S, N3)
byf.shapiro(SAR ~ Grupo.S, N3)
byf.shapiro(SP ~ Grupo.S, N3)
byf.shapiro(SLA ~ Grupo.S, N3)
byf.shapiro(SLO ~ Grupo.S, N3)

leveneTest(WAL ~ Grupo.W, N3, mean)
leveneTest(WAR ~ Grupo.W, N3, mean)
leveneTest(WP ~ Grupo.W, N3, mean)
leveneTest(WLA ~ Grupo.W, N3, mean)
leveneTest(WLO ~ Grupo.W, N3, mean)

leveneTest(SAL ~ Grupo.S, N3, mean)
leveneTest(SAR ~ Grupo.S, N3, mean)
leveneTest(SP ~ Grupo.S, N3, mean)
leveneTest(SLA ~ Grupo.S, N3, mean)
leveneTest(SLO ~ Grupo.S, N3, mean)

oneway.test(WAL ~ Grupo.W, data = N3, var.equal=FALSE)
oneway.test(WAR ~ Grupo.W, data = N3, var.equal= TRUE)
oneway.test(WP ~ Grupo.W, data = N3, var.equal= FALSE)
oneway.test(WLA ~ Grupo.W, data = N3, var.equal=FALSE)
oneway.test(WLO ~ Grupo.W, data = N3, var.equal=FALSE)

oneway.test(SAL ~ Grupo.S, data = N3, var.equal=FALSE)
oneway.test(SAR ~ Grupo.S, data = N3, var.equal=TRUE)
oneway.test(SP ~ Grupo.S, data = N3, var.equal=TRUE)
oneway.test(SLA ~ Grupo.S, data = N3, var.equal=FALSE)
oneway.test(SLO ~ Grupo.S, data = N3, var.equal=TRUE)


mod <- aov(WAL ~ Grupo.W, N3)
PostHocTest(mod, method = "hsd")
mod <- aov(WAR ~ Grupo.W, N3)
PostHocTest(mod, method = "hsd")
mod <- aov(WP ~ Grupo.W, N3)
PostHocTest(mod, method = "hsd")
mod <- aov(WLA ~ Grupo.W, N3)
PostHocTest(mod, method = "hsd")
mod <- aov(WLO ~ Grupo.W, N3)
PostHocTest(mod, method = "hsd")

mod <- aov(SAL ~ Grupo.S, N3)
PostHocTest(mod, method = "hsd")
mod <- aov(SAR ~ Grupo.S, N3)
PostHocTest(mod, method = "hsd")
mod <- aov(SP ~ Grupo.S, N3)
PostHocTest(mod, method = "hsd")
mod <- aov(SLA ~ Grupo.S, N3)
PostHocTest(mod, method = "hsd")
mod <- aov(SLO ~ Grupo.S, N3)
PostHocTest(mod, method = "hsd")


## K = 5

png("6115.png", width = 1500, height = 2000)
tanglegram(dend1, dend2,
           highlight_distinct_edges = FALSE, 
           common_subtrees_color_lines = TRUE, 
           common_subtrees_color_branches = FALSE,
           main_left = "Wikiaves",
           main_right = "SpeciesLink",
           lwd = .5,
           k_branches = 5)
dev.off()

grp1 <- cutree(hc1, k = 5)
grp1

grp2 <- cutree(hc2, k = 5)
grp2

maps$WAV5 <- cut(maps$dendWAV5, breaks = c(-2,0,1,2,3,4,5),
                 labels = c("Sem Registro","Grupo 1","Grupo 2","Grupo 3","Grupo 4","Grupo 5"))

maps$SLI5 <- cut(maps$dendSLI5, breaks = c(-2,0,1,2,3,4,5),
                 labels = c("Sem Registro","Grupo 1","Grupo 2","Grupo 3","Grupo 4","Grupo 5"))

png("61251.png", width = 600, height = 300)
ggplot(maps) +
  geom_sf(aes(fill=WAV5), color = NA) +
  labs(subtitle="Wikiaves", size=8)+ 
  scale_fill_manual (values = c('#f0f0f0','#B646C7','#228B00','#9F7000','#0082CE','#CC476B'))+
  labs(fill = " ")+
  theme_minimal()
dev.off()

png("61252.png", width = 600, height = 300)
ggplot(maps) +
  geom_sf(aes(fill=SLI5), color = NA) +
  labs(subtitle="SpeciesLink", size=8)+ 
  scale_fill_manual (values = c('#f0f0f0','#B646C7','#228B00','#9F7000','#0082CE','#CC476B'))+
  labs(fill = " ")+
  theme_minimal()
dev.off()

byf.shapiro(WAL ~ Grupo.W, N4)
byf.shapiro(WAR ~ Grupo.W, N4)
byf.shapiro(WP ~ Grupo.W, N4)
byf.shapiro(WLA ~ Grupo.W, N4)
byf.shapiro(WLO ~ Grupo.W, N4)

byf.shapiro(SAL ~ Grupo.S, N4)
byf.shapiro(SAR ~ Grupo.S, N4)
byf.shapiro(SP ~ Grupo.S, N4)
byf.shapiro(SLA ~ Grupo.S, N4)
byf.shapiro(SLO ~ Grupo.S, N4)

leveneTest(WAL ~ Grupo.W, N4, mean)
leveneTest(WAR ~ Grupo.W, N4, mean)
leveneTest(WP ~ Grupo.W, N4, mean)
leveneTest(WLA ~ Grupo.W, N4, mean)
leveneTest(WLO ~ Grupo.W, N4, mean)

leveneTest(SAL ~ Grupo.S, N4, mean)
leveneTest(SAR ~ Grupo.S, N4, mean)
leveneTest(SP ~ Grupo.S, N4, mean)
leveneTest(SLA ~ Grupo.S, N4, mean)
leveneTest(SLO ~ Grupo.S, N4, mean)

oneway.test(WAL ~ Grupo.W, data = N4, var.equal=FALSE)
oneway.test(WAR ~ Grupo.W, data = N4, var.equal=TRUE)
oneway.test(WP ~ Grupo.W, data = N4, var.equal=FALSE)
oneway.test(WLA ~ Grupo.W, data = N4, var.equal=FALSE)
oneway.test(WLO ~ Grupo.W, data = N4, var.equal=FALSE)

oneway.test(SAL ~ Grupo.S, data = N4, var.equal=FALSE)
oneway.test(SAR ~ Grupo.S, data = N4, var.equal=TRUE)
oneway.test(SP ~ Grupo.S, data = N4, var.equal=TRUE)
oneway.test(SLA ~ Grupo.S, data = N4, var.equal=FALSE)
oneway.test(SLO ~ Grupo.S, data = N4, var.equal=TRUE)


mod <- aov(WAL ~ Grupo.W, N4)
PostHocTest(mod, method = "hsd")
mod <- aov(WAR ~ Grupo.W, N4)
PostHocTest(mod, method = "hsd")
mod <- aov(WP ~ Grupo.W, N4)
PostHocTest(mod, method = "hsd")
mod <- aov(WLA ~ Grupo.W, N4)
PostHocTest(mod, method = "hsd")
mod <- aov(WLO ~ Grupo.W, N4)
PostHocTest(mod, method = "hsd")

mod <- aov(SAL ~ Grupo.S, N4)
PostHocTest(mod, method = "hsd")
mod <- aov(SAR ~ Grupo.S, N4)
PostHocTest(mod, method = "hsd")
mod <- aov(SP ~ Grupo.S, N4)
PostHocTest(mod, method = "hsd")
mod <- aov(SLA ~ Grupo.S, N4)
PostHocTest(mod, method = "hsd")
mod <- aov(SLO ~ Grupo.S, N4)
PostHocTest(mod, method = "hsd")

## K = 6

png("6116.png", width = 1500, height = 2000)
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
dev.off()

grp1 <- cutree(hc1, k = 6)
grp1

grp2 <- cutree(hc2, k = 6)
grp2


maps$WAV6 <- cut(maps$dendWAV6, breaks = c(-2,0,1,2,3,4,5,6),
              labels = c("Sem Registro","Grupo 1","Grupo 2","Grupo 3","Grupo 4","Grupo 5","Grupo 6"))

maps$SLI6 <- cut(maps$dendSLI6, breaks = c(-2,0,1,2,3,4,5,6),
                labels = c("Sem Registro","Grupo 1","Grupo 2","Grupo 3","Grupo 4","Grupo 5","Grupo 6"))

png("61261.png", width = 600, height = 300)
ggplot(maps) +
  geom_sf(aes(fill=WAV6), color = NA) +
  labs(subtitle="Wikiaves", size=8)+ 
  scale_fill_manual (values = c('#f0f0f0','#B646C7','#228B00','#9F7000','#0082CE','#CC476B','#009681'))+
  labs(fill = " ")+
  theme_minimal()
dev.off()

png("61262.png", width = 600, height = 300)
ggplot(maps) +
  geom_sf(aes(fill=SLI6), color = NA) +
  labs(subtitle="SpeciesLink", size=8)+ 
  scale_fill_manual (values = c('#f0f0f0','#B646C7','#228B00','#9F7000','#0082CE','#CC476B','#009681'))+
  labs(fill = " ")+
  theme_minimal()
dev.off()

byf.shapiro(WAL ~ Grupo.W, N5)
byf.shapiro(WAR ~ Grupo.W, N5)
byf.shapiro(WP ~ Grupo.W, N5)
byf.shapiro(WLA ~ Grupo.W, N5)
byf.shapiro(WLO ~ Grupo.W, N5)

byf.shapiro(SAL ~ Grupo.S, N5)
byf.shapiro(SAR ~ Grupo.S, N5)
byf.shapiro(SP ~ Grupo.S, N5)
byf.shapiro(SLA ~ Grupo.S, N5)
byf.shapiro(SLO ~ Grupo.S, N5)

leveneTest(WAL ~ Grupo.W, N5, mean)
leveneTest(WAR ~ Grupo.W, N5, mean)
leveneTest(WP ~ Grupo.W, N5, mean)
leveneTest(WLA ~ Grupo.W, N5, mean)
leveneTest(WLO ~ Grupo.W, N5, mean)

leveneTest(SAL ~ Grupo.S, N5, mean)
leveneTest(SAR ~ Grupo.S, N5, mean)
leveneTest(SP ~ Grupo.S, N5, mean)
leveneTest(SLA ~ Grupo.S, N5, mean)
leveneTest(SLO ~ Grupo.S, N5, mean)

oneway.test(WAL ~ Grupo.W, data = N5, var.equal=FALSE)
oneway.test(WAR ~ Grupo.W, data = N5, var.equal=FALSE)
oneway.test(WP ~ Grupo.W, data = N5, var.equal=FALSE)
oneway.test(WLA ~ Grupo.W, data = N5, var.equal=FALSE)
oneway.test(WLO ~ Grupo.W, data = N5, var.equal=FALSE)

oneway.test(SAL ~ Grupo.S, data = N5, var.equal=FALSE)
oneway.test(SAR ~ Grupo.S, data = N5, var.equal=TRUE)
oneway.test(SP ~ Grupo.S, data = N5, var.equal=TRUE)
oneway.test(SLA ~ Grupo.S, data = N5, var.equal=FALSE)
oneway.test(SLO ~ Grupo.S, data = N5, var.equal=TRUE)


mod <- aov(WAL ~ Grupo.W, N5)
PostHocTest(mod, method = "hsd")
mod <- aov(WAR ~ Grupo.W, N5)
PostHocTest(mod, method = "hsd")
mod <- aov(WP ~ Grupo.W, N5)
PostHocTest(mod, method = "hsd")
mod <- aov(WLA ~ Grupo.W, N5)
PostHocTest(mod, method = "hsd")
mod <- aov(WLO ~ Grupo.W, N5)
PostHocTest(mod, method = "hsd")

mod <- aov(SAL ~ Grupo.S, N5)
PostHocTest(mod, method = "hsd")
mod <- aov(SAR ~ Grupo.S, N5)
PostHocTest(mod, method = "hsd")
mod <- aov(SP ~ Grupo.S, N5)
PostHocTest(mod, method = "hsd")
mod <- aov(SLA ~ Grupo.S, N5)
PostHocTest(mod, method = "hsd")
mod <- aov(SLO ~ Grupo.S, N5)
PostHocTest(mod, method = "hsd")

## K = 7

png("6117.png", width = 1500, height = 2000)
tanglegram(dend1, dend2,
           highlight_distinct_edges = FALSE, 
           common_subtrees_color_lines = TRUE, 
           common_subtrees_color_branches = FALSE,
           main_left = "Wikiaves",
           main_right = "SpeciesLink",
           lwd = .5,
           k_branches = 7)
dev.off()

grp1 <- cutree(hc1, k = 7)
grp1

grp2 <- cutree(hc2, k = 7)
grp2

maps$WAV7 <- cut(maps$dendWAV7, breaks = c(-2,0,1,2,3,4,5,6,7),
                 labels = c("Sem Registro","Grupo 1","Grupo 2","Grupo 3","Grupo 4","Grupo 5","Grupo 6","Grupo 7"))

maps$SLI7 <- cut(maps$dendSLI7, breaks = c(-2,0,1,2,3,4,5,6,7),
                 labels = c("Sem Registro","Grupo 1","Grupo 2","Grupo 3","Grupo 4","Grupo 5","Grupo 6","Grupo 7"))

png("61271.png", width = 600, height = 300)
ggplot(maps) +
  geom_sf(aes(fill=WAV7), color = NA) +
  labs(subtitle="Wikiaves", size=8)+ 
  scale_fill_manual (values = c('#f0f0f0','#C03FBE','#4473D7','#0093A9','#00944F','#5D8400','#A86B00','#CC476B'))+
  labs(fill = " ")+
  theme_minimal()
dev.off()

png("61272.png", width = 600, height = 300)
ggplot(maps) +
  geom_sf(aes(fill=SLI7), color = NA) +
  labs(subtitle="SpeciesLink", size=8)+ 
  scale_fill_manual (values = c('#f0f0f0','#C03FBE','#4473D7','#0093A9','#00944F','#5D8400','#A86B00','#CC476B'))+
  labs(fill = " ")+
  theme_minimal()
dev.off()

## K = 8

png("6118.png", width = 1500, height = 2000)
tanglegram(dend1, dend2,
           highlight_distinct_edges = FALSE, 
           common_subtrees_color_lines = TRUE, 
           common_subtrees_color_branches = FALSE,
           main_left = "Wikiaves",
           main_right = "SpeciesLink",
           lwd = .5,
           k_branches = 8)
dev.off()

grp1 <- cutree(hc1, k = 8)
grp1

grp2 <- cutree(hc2, k = 8)
grp2


maps$WAV8 <- cut(maps$dendWAV8, breaks = c(-2,0,1,2,3,4,5,6,7,8),
                 labels = c("Sem Registro","Grupo 1","Grupo 2","Grupo 3","Grupo 4","Grupo 5","Grupo 6","Grupo 7","Grupo 8"))

maps$SLI8 <- cut(maps$dendSLI8, breaks = c(-2,0,1,2,3,4,5,6,7,8),
                 labels = c("Sem Registro","Grupo 1","Grupo 2","Grupo 3","Grupo 4","Grupo 5","Grupo 6","Grupo 7","Grupo 8"))

png("61281.png", width = 600, height = 300)
ggplot(maps) +
  geom_sf(aes(fill=WAV8), color = NA) +
  labs(subtitle="Wikiaves", size=8)+ 
  scale_fill_manual (values = c('#f0f0f0','#C03FBE','#4473D7','#0093A9','#00944F','#5D8400','#A86B00','#CC476B','#7866D8'))+
  labs(fill = " ")+
  theme_minimal()
dev.off()

png("61282.png", width = 600, height = 300)
ggplot(maps) +
  geom_sf(aes(fill=SLI8), color = NA) +
  labs(subtitle="SpeciesLink", size=8)+ 
  scale_fill_manual (values = c('#f0f0f0','#C03FBE','#4473D7','#0093A9','#00944F','#5D8400','#A86B00','#CC476B','#7866D8'))+
  labs(fill = " ")+
  theme_minimal()
dev.off()

## K = 9

png("6119.png", width = 1500, height = 2000)
tanglegram(dend1, dend2,
           highlight_distinct_edges = FALSE, 
           common_subtrees_color_lines = TRUE, 
           common_subtrees_color_branches = FALSE,
           main_left = "Wikiaves",
           main_right = "SpeciesLink",
           lwd = .5,
           k_branches = 9)
dev.off()


grp1 <- cutree(hc1, k = 9)
grp1

grp2 <- cutree(hc2, k = 9)
grp2

maps$WAV9 <- cut(maps$dendWAV9, breaks = c(-2,0,1,2,3,4,5,6,7,8,9),
                 labels = c("Sem Registro","Grupo 1","Grupo 2","Grupo 3","Grupo 4","Grupo 5","Grupo 6","Grupo 7","Grupo 8", "Grupo 9"))

maps$SLI9 <- cut(maps$dendSLI9, breaks = c(-2,0,1,2,3,4,5,6,7,8,9),
                 labels = c("Sem Registro","Grupo 1","Grupo 2","Grupo 3","Grupo 4","Grupo 5","Grupo 6","Grupo 7","Grupo 8", "Grupo 9"))

png("61291.png", width = 600, height = 300)
ggplot(maps) +
  geom_sf(aes(fill=WAV9), color = NA) +
  labs(subtitle="Wikiaves", size=8)+ 
  scale_fill_manual (values = c('#f0f0f0','#C03FBE','#4473D7','#0093A9','#00944F','#5D8400','#A86B00','#CC476B','#7866D8','#009681'))+
  labs(fill = " ")+
  theme_minimal()
dev.off()

png("61292.png", width = 600, height = 300)
ggplot(maps) +
  geom_sf(aes(fill=SLI9), color = NA) +
  labs(subtitle="SpeciesLink", size=8)+ 
  scale_fill_manual (values = c('#f0f0f0','#C03FBE','#4473D7','#0093A9','#00944F','#5D8400','#A86B00','#CC476B','#7866D8','#009681'))+
  labs(fill = " ")+9

  theme_minimal()
dev.off()

# Top 50



rm(list = ls())



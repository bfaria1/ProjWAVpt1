library(pacman)

pacman::p_load(tidyverse, cluster, gclus, factoextra, dendextend, sf, psych, geobr, ggplot2, openxlsx, ade4, vegan, car, rstatix, multcomp, emmeans, RVAideMemoire, psych, DescTools)

# Todos os Municípios

WAV <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S6/WAV-E.xlsx", sheet = "Geral")

WAV <- t(WAV)
is.numeric(WAV)

SLI <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S6/SPL-E.xlsx", sheet = "Geral")

SLI <- t(SLI)
is.numeric(SLI)

WAV2 <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S6/Grupos2.xlsx", sheet = "WAVE")

WAV2 <- t(WAV2)
is.numeric(WAV2)

SLI2 <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S6/Grupos2.xlsx", sheet = "SLIE")

SLI2 <- t(SLI2)
is.numeric(SLI2)

dados <- read.xlsx("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S6/Mapas.xlsx")
dados2 <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S6/Grupos2.xlsx", sheet = "Mapas")
mc <- read_municipality(code_muni= "SP")

maps <- merge(mc,dados)
maps2 <- merge(mc, dados2)

N1 <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S6/Grupos.xlsx", sheet = "K = 2")
N2 <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S6/Grupos.xlsx", sheet = "K = 3")
N3 <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S6/Grupos.xlsx", sheet = "K = 4")
N4 <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S6/Grupos.xlsx", sheet = "K = 5")
N5 <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S6/Grupos.xlsx", sheet = "K = 6")

N <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S6/Grupos2.xlsx", sheet = "Dados")

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
  labs(fill = " ")+
  theme_minimal()
dev.off()

# Top 50


WJ <- vegdist(WAV2, method = "jaccard", binary = TRUE)
SJ <- vegdist(SLI2, method = "jaccard", binary = TRUE)

hc3 <- hclust(WJ, method = "ward.D2" )
hc4 <- hclust(SJ, method = "ward.D2" )


dend3 <- as.dendrogram (hc3)
dend4 <- as.dendrogram (hc4)

# k = 2

png("6112.png", width = 1500, height = 2000)
tanglegram(dend3, dend4,
           highlight_distinct_edges = FALSE, 
           common_subtrees_color_lines = TRUE, 
           common_subtrees_color_branches = FALSE,
           main_left = "Wikiaves",
           main_right = "SpeciesLink",
           lwd = .5,
           k_branches = 2)
dev.off()

maps2$WK2 <- factor(maps2$WK2, levels=c("Sem Registro", "Grupo 1", "Grupo 2"))

maps2$SK2 <- factor(maps2$SK2, levels=c("Sem Registro", "Grupo 1", "Grupo 2"))

png("62221.png", width = 600, height = 300)
ggplot(maps2) +
  geom_sf(aes(fill=WK2), color = NA) +
  labs(subtitle="Wikiaves", size=8)+ 
  scale_fill_manual (values = c('#f0f0f0','#009681','#CC476B'))+
  labs(fill = " ")+
  theme_minimal()
dev.off()

png("62222.png", width = 600, height = 300)
ggplot(maps2) +
  geom_sf(aes(fill=SK2), color = NA) +
  labs(subtitle="SpeciesLink", size=8)+ 
  scale_fill_manual (values = c('#f0f0f0','#009681','#CC476B'))+
  labs(fill = " ")+
  theme_minimal()
dev.off()

grp1 <- cutree(hc3, k = 2)
grp1

grp2 <- cutree(hc4, k = 2)
grp2

byf.shapiro(AL ~ GW2, N)
byf.shapiro(AR ~ GW2, N)
byf.shapiro(POP ~ GW2, N)
byf.shapiro(LA ~ GW2, N)
byf.shapiro(LO ~ GW2, N)

byf.shapiro(AL ~ GS2, N)
byf.shapiro(AR ~ GS2, N)
byf.shapiro(POP ~ GS2, N)
byf.shapiro(LA ~ GS2, N)
byf.shapiro(LO ~ GS2, N)


leveneTest(AL ~ GW2, N, mean)
leveneTest(AR ~ GW2, N, mean)
leveneTest(POP ~ GW2, N, mean)
leveneTest(LA ~ GW2, N, mean)
leveneTest(LO ~ GW2, N, mean)

leveneTest(AL ~ GS2, N, mean)
leveneTest(AR ~ GS2, N, mean)
leveneTest(POP ~ GS2, N, mean)
leveneTest(LA ~ GS2, N, mean)
leveneTest(LO ~ GS2, N, mean)

oneway.test(AL ~ GW2, data = N, var.equal=FALSE)
oneway.test(AR ~ GW2, data = N, var.equal=TRUE)
oneway.test(POP ~ GW2, data = N, var.equal=TRUE)
oneway.test(LA ~ GW2, data = N, var.equal=TRUE)
oneway.test(LO ~ GW2, data = N, var.equal=TRUE)

oneway.test(AL ~ GS2, data = N, var.equal=TRUE)
oneway.test(AR ~ GS2, data = N, var.equal=TRUE)
oneway.test(POP ~ GS2, data = N, var.equal=TRUE)
oneway.test(LA ~ GS2, data = N, var.equal=FALSE)
oneway.test(LO ~ GS2, data = N, var.equal=FALSE)


mod <- aov(AL ~ GW2, N)
PostHocTest(mod, method = "hsd")
mod <- aov(AR ~ GW2, N)
PostHocTest(mod, method = "hsd")
mod <- aov(POP ~ GW2, N)
PostHocTest(mod, method = "hsd")
mod <- aov(LA ~ GW2, N)
PostHocTest(mod, method = "hsd")
mod <- aov(LO ~ GW2, N)
PostHocTest(mod, method = "hsd")

mod <- aov(AL ~ GS2, N)
PostHocTest(mod, method = "hsd")
mod <- aov(AR ~ GS2, N)
PostHocTest(mod, method = "hsd")
mod <- aov(POP ~ GS2, N5)
PostHocTest(mod, method = "hsd")
mod <- aov(LA ~ GS2, N)
PostHocTest(mod, method = "hsd")
mod <- aov(LO ~ GS2, N)
PostHocTest(mod, method = "hsd")


# k = 3




png("6213.png", width = 1500, height = 2000)
tanglegram(dend3, dend4,
           highlight_distinct_edges = FALSE, 
           common_subtrees_color_lines = TRUE, 
           common_subtrees_color_branches = FALSE,
           main_left = "Wikiaves",
           main_right = "SpeciesLink",
           lwd = .5,
           k_branches = 3)
dev.off()

maps2$WK3 <- factor(maps2$WK3, levels=c("Sem Registro", "Grupo 1", "Grupo 2","Grupo 3"))

maps2$SK3 <- factor(maps2$SK3, levels=c("Sem Registro", "Grupo 1", "Grupo 2","Grupo 3"))

png("62231.png", width = 600, height = 300)
ggplot(maps2) +
  geom_sf(aes(fill=WK3), color = NA) +
  labs(subtitle="Wikiaves", size=8)+ 
  scale_fill_manual (values = c('#f0f0f0','#0082CE','#228B00','#CC476B'))+
  labs(fill = " ")+
  theme_minimal()
dev.off()

png("62232.png", width = 600, height = 300)
ggplot(maps2) +
  geom_sf(aes(fill=SK3), color = NA) +
  labs(subtitle="SpeciesLink", size=8)+ 
  scale_fill_manual (values = c('#f0f0f0','#0082CE','#228B00','#CC476B'))+
  labs(fill = " ")+
  theme_minimal()
dev.off()

grp1 <- cutree(hc3, k = 3)
grp1

grp2 <- cutree(hc4, k = 3)
grp2


byf.shapiro(AL ~ GW3, N)
byf.shapiro(AR ~ GW3, N)
byf.shapiro(POP ~ GW3, N)
byf.shapiro(LA ~ GW3, N)
byf.shapiro(LO ~ GW3, N)

byf.shapiro(AL ~ GS3, N)
byf.shapiro(AR ~ GS3, N)
byf.shapiro(POP ~ GS3, N)
byf.shapiro(LA ~ GS3, N)
byf.shapiro(LO ~ GS3, N)


leveneTest(AL ~ GW3, N, mean)
leveneTest(AR ~ GW3, N, mean)
leveneTest(POP ~ GW3, N, mean)
leveneTest(LA ~ GW3, N, mean)
leveneTest(LO ~ GW3, N, mean)

leveneTest(AL ~ GS3, N, mean)
leveneTest(AR ~ GS3, N, mean)
leveneTest(POP ~ GS3, N, mean)
leveneTest(LA ~ GS3, N, mean)
leveneTest(LO ~ GS3, N, mean)

oneway.test(AL ~ GW3, data = N, var.equal=TRUE)
oneway.test(AR ~ GW3, data = N, var.equal=TRUE)
oneway.test(POP ~ GW3, data = N, var.equal=FALSE)
oneway.test(LA ~ GW3, data = N, var.equal=TRUE)
oneway.test(LO ~ GW3, data = N, var.equal=TRUE)

oneway.test(AL ~ GS3, data = N, var.equal=FALSE)
oneway.test(AR ~ GS3, data = N, var.equal=TRUE)
oneway.test(POP ~ GS3, data = N, var.equal=TRUE)
oneway.test(LA ~ GS3, data = N, var.equal=FALSE)
oneway.test(LO ~ GS3, data = N, var.equal=FALSE)


mod <- aov(AL ~ GW3, N)
PostHocTest(mod, method = "hsd")
mod <- aov(AR ~ GW3, N)
PostHocTest(mod, method = "hsd")
mod <- aov(POP ~ GW3, N)
PostHocTest(mod, method = "hsd")
mod <- aov(LA ~ GW3, N)
PostHocTest(mod, method = "hsd")
mod <- aov(LO ~ GW3, N)
PostHocTest(mod, method = "hsd")

mod <- aov(AL ~ GS3, N)
PostHocTest(mod, method = "hsd")
mod <- aov(AR ~ GS3, N)
PostHocTest(mod, method = "hsd")
mod <- aov(POP ~ GS3, N5)
PostHocTest(mod, method = "hsd")
mod <- aov(LA ~ GS3, N)
PostHocTest(mod, method = "hsd")
mod <- aov(LO ~ GS3, N)
PostHocTest(mod, method = "hsd")

# k = 4

png("6214.png", width = 1500, height = 2000)
tanglegram(dend3, dend4,
           highlight_distinct_edges = FALSE, 
           common_subtrees_color_lines = TRUE, 
           common_subtrees_color_branches = FALSE,
           main_left = "Wikiaves",
           main_right = "SpeciesLink",
           lwd = .5,
           k_branches = 4)
dev.off()

maps2$WK4 <- factor(maps2$WK4, levels=c("Sem Registro", "Grupo 1", "Grupo 2","Grupo 3", "Grupo 4"))

maps2$SK4 <- factor(maps2$SK4, levels=c("Sem Registro", "Grupo 1", "Grupo 2","Grupo 3", "Grupo 4"))

png("62241.png", width = 600, height = 300)
ggplot(maps2) +
  geom_sf(aes(fill=WK4), color = NA) +
  labs(subtitle="Wikiaves", size=8)+ 
  scale_fill_manual (values = c('#f0f0f0','#7866D8','#767F00','#CC476B','#009681'))+
  labs(fill = " ")+
  theme_minimal()
dev.off()

png("62242.png", width = 600, height = 300)
ggplot(maps2) +
  geom_sf(aes(fill=SK4), color = NA) +
  labs(subtitle="SpeciesLink", size=8)+ 
  scale_fill_manual (values = c('#f0f0f0','#7866D8','#767F00','#CC476B','#009681'))+
  labs(fill = " ")+
  theme_minimal()
dev.off()


grp1 <- cutree(hc3, k = 4)
grp1

grp2 <- cutree(hc4, k = 4)
grp2

byf.shapiro(AL ~ GW4, N)
byf.shapiro(AR ~ GW4, N)
byf.shapiro(POP ~ GW4, N)
byf.shapiro(LA ~ GW4, N)
byf.shapiro(LO ~ GW4, N)

byf.shapiro(AL ~ GS4, N)
byf.shapiro(AR ~ GS4, N)
byf.shapiro(POP ~ GS4, N)
byf.shapiro(LA ~ GS4, N)
byf.shapiro(LO ~ GS4, N)


leveneTest(AL ~ GW4, N, mean)
leveneTest(AR ~ GW4, N, mean)
leveneTest(POP ~ GW4, N, mean)
leveneTest(LA ~ GW4, N, mean)
leveneTest(LO ~ GW4, N, mean)

leveneTest(AL ~ GS4, N, mean)
leveneTest(AR ~ GS4, N, mean)
leveneTest(POP ~ GS4, N, mean)
leveneTest(LA ~ GS4, N, mean)
leveneTest(LO ~ GS4, N, mean)

oneway.test(AL ~ GW4, data = N, var.equal=FALSE)
oneway.test(AR ~ GW4, data = N, var.equal=TRUE)
oneway.test(POP ~ GW4, data = N, var.equal=FALSE)
oneway.test(LA ~ GW4, data = N, var.equal=TRUE)
oneway.test(LO ~ GW4, data = N, var.equal=TRUE)

oneway.test(AL ~ GS4, data = N, var.equal=FALSE)
oneway.test(AR ~ GS4, data = N, var.equal=TRUE)
oneway.test(POP ~ GS4, data = N, var.equal=TRUE)
oneway.test(LA ~ GS4, data = N, var.equal=FALSE)
oneway.test(LO ~ GS4, data = N, var.equal=TRUE)


mod <- aov(AL ~ GW4, N)
PostHocTest(mod, method = "hsd")
mod <- aov(AR ~ GW4, N)
PostHocTest(mod, method = "hsd")
mod <- aov(POP ~ GW4, N)
PostHocTest(mod, method = "hsd")
mod <- aov(LA ~ GW4, N)
PostHocTest(mod, method = "hsd")
mod <- aov(LO ~ GW4, N)
PostHocTest(mod, method = "hsd")

mod <- aov(AL ~ GS4, N)
PostHocTest(mod, method = "hsd")
mod <- aov(AR ~ GS4, N)
PostHocTest(mod, method = "hsd")
mod <- aov(POP ~ GS4, N5)
PostHocTest(mod, method = "hsd")
mod <- aov(LA ~ GS4, N)
PostHocTest(mod, method = "hsd")
mod <- aov(LO ~ GS4, N)
PostHocTest(mod, method = "hsd")

# k = 5

png("6215.png", width = 1500, height = 2000)
tanglegram(dend3, dend4,
           highlight_distinct_edges = FALSE, 
           common_subtrees_color_lines = TRUE, 
           common_subtrees_color_branches = FALSE,
           main_left = "Wikiaves",
           main_right = "SpeciesLink",
           lwd = .5,
           k_branches = 5)
dev.off()

maps2$WK5 <- factor(maps2$WK5, levels=c("Sem Registro", "Grupo 1", "Grupo 2","Grupo 3", "Grupo 4", "Grupo 5"))

maps2$SK5 <- factor(maps2$SK5, levels=c("Sem Registro", "Grupo 1", "Grupo 2","Grupo 3", "Grupo 4", "Grupo 5"))

png("62251.png", width = 600, height = 300)
ggplot(maps2) +
  geom_sf(aes(fill=WK5), color = NA) +
  labs(subtitle="Wikiaves", size=8)+ 
  scale_fill_manual (values = c('#f0f0f0','#B646C7','#228B00','#9F7000','#0082CE','#CC476B'))+
  labs(fill = " ")+
  theme_minimal()
dev.off()

png("62252.png", width = 600, height = 300)
ggplot(maps2) +
  geom_sf(aes(fill=SK5), color = NA) +
  labs(subtitle="SpeciesLink", size=8)+ 
  scale_fill_manual (values = c('#f0f0f0','#B646C7','#228B00','#9F7000','#0082CE','#CC476B'))+
  labs(fill = " ")+
  theme_minimal()
dev.off()

grp1 <- cutree(hc3, k = 5)
grp1

grp2 <- cutree(hc4, k = 5)
grp2

byf.shapiro(AL ~ GW5, N)
byf.shapiro(AR ~ GW5, N)
byf.shapiro(POP ~ GW5, N)
byf.shapiro(LA ~ GW5, N)
byf.shapiro(LO ~ GW5, N)

byf.shapiro(AL ~ GS5, N)
byf.shapiro(AR ~ GS5, N)
byf.shapiro(POP ~ GS5, N)
byf.shapiro(LA ~ GS5, N)
byf.shapiro(LO ~ GS5, N)


leveneTest(AL ~ GW5, N, mean)
leveneTest(AR ~ GW5, N, mean)
leveneTest(POP ~ GW5, N, mean)
leveneTest(LA ~ GW5, N, mean)
leveneTest(LO ~ GW5, N, mean)

leveneTest(AL ~ GS5, N, mean)
leveneTest(AR ~ GS5, N, mean)
leveneTest(POP ~ GS5, N, mean)
leveneTest(LA ~ GS5, N, mean)
leveneTest(LO ~ GS5, N, mean)

oneway.test(AL ~ GW5, data = N, var.equal=TRUE)
oneway.test(AR ~ GW5, data = N, var.equal=TRUE)
oneway.test(POP ~ GW5, data = N, var.equal=FALSE)
oneway.test(LA ~ GW5, data = N, var.equal=FALSE)
oneway.test(LO ~ GW5, data = N, var.equal=TRUE)

oneway.test(AL ~ GS5, data = N, var.equal=TRUE)
oneway.test(AR ~ GS5, data = N, var.equal=TRUE)
oneway.test(POP ~ GS5, data = N, var.equal=FALSE)
oneway.test(LA ~ GS5, data = N, var.equal=FALSE)
oneway.test(LO ~ GS5, data = N, var.equal=TRUE)


mod <- aov(AL ~ GW5, N)
PostHocTest(mod, method = "hsd")
mod <- aov(AR ~ GW5, N)
PostHocTest(mod, method = "hsd")
mod <- aov(POP ~ GW5, N)
PostHocTest(mod, method = "hsd")
mod <- aov(LA ~ GW5, N)
PostHocTest(mod, method = "hsd")
mod <- aov(LO ~ GW5, N)
PostHocTest(mod, method = "hsd")

mod <- aov(AL ~ GS5, N)
PostHocTest(mod, method = "hsd")
mod <- aov(AR ~ GS5, N)
PostHocTest(mod, method = "hsd")
mod <- aov(POP ~ GS5, N5)
PostHocTest(mod, method = "hsd")
mod <- aov(LA ~ GS5, N)
PostHocTest(mod, method = "hsd")
mod <- aov(LO ~ GS5, N)
PostHocTest(mod, method = "hsd")


rm(list = ls())



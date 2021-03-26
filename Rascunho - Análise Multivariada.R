### Bibliotecas e dados

library("openxlsx") 
library(ggplot2)
library(RColorBrewer)
library(factoextra)
library(ade4)
library(vegan)
library(gclus)
library(cluster)
library(FD)
library(ggcorrplot)
library(sf)
library(geobr)
library(dendextend)

WAV <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S6/WAV-E.xlsx", sheet = "10")
WAV2 <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S6/WAV-E.xlsx", sheet = "20")
WAV3 <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S6/WAV-E.xlsx", sheet = "25")
WAV4 <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S6/WAV-E.xlsx", sheet = "50")
WAV5 <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S6/WAV-E.xlsx", sheet = "100")
WAV6 <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S6/WAV-E.xlsx", sheet = "Geral")


SLI <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S6/SPL-E.xlsx", sheet = "10")
SLI2 <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S6/SPL-E.xlsx", sheet = "20")
SLI3 <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S6/SPL-E.xlsx", sheet = "25")
SLI4 <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S6/SPL-E.xlsx", sheet = "50")
SLI5 <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S6/SPL-E.xlsx", sheet = "100")
SLI6 <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S6/SPL-E.xlsx", sheet = "Geral")

dados <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S6/FE.xlsx")
maps <- read_municipality(code_muni= "SP")


FE <- data.frame(dados[,6:10], row.names = dados$Cidades)
tFE <- t(FE)


## Arrumando os dados

WAV <- t(WAV)
WAV <- WAV[-1,]
is.numeric(WAV)

WAV2 <- t(WAV2)
WAV2 <- WAV2[-1,]
is.numeric(WAV2)

WAV3 <- t(WAV3)
WAV3 <- WAV3[-1,]
is.numeric(WAV3)

WAV4 <- t(WAV4)
WAV4 <- WAV4[-1,]
is.numeric(WAV4)

WAV5 <- t(WAV5)
WAV5 <- WAV5[-1,]
is.numeric(WAV5)

WAV6 <- t(WAV6)
WAV6 <- WAV6[-1,]
is.numeric(WAV6)

SLI <- t(SLI)
SLI <- SLI[-1,]
is.numeric(SLI)

SLI2 <- t(SLI2)
SLI2 <- SLI2[-1,]
is.numeric(SLI2)

SLI3 <- t(SLI3)
SLI3 <- SLI3[-1,]
is.numeric(SLI3)

SLI4 <- t(SLI4)
SLI4 <- SLI4[-1,]
is.numeric(SLI4)

SLI5<- t(SLI5)
SLI5 <- SLI5[-1,]
is.numeric(SLI)

SLI6 <- t(SLI6)
SLI6 <- SLI6[-1,]
is.numeric(SLI6)

## Matrizes de distancia (Jaccard)

### Wikiaves

A1 <- vegdist(WAV, method = "jaccard", binary = TRUE)
A2 <- vegdist(WAV2, method = "jaccard", binary = TRUE)
A3 <- vegdist(WAV3, method = "jaccard", binary = TRUE)
A4 <- vegdist(WAV4, method = "jaccard", binary = TRUE)
A5 <- vegdist(WAV5, method = "jaccard", binary = TRUE)
A6 <- vegdist(WAV6, method = "jaccard", binary = TRUE)

### SpeciesLink

B1 <- vegdist(SLI, method = "jaccard", binary = TRUE)
B2 <- vegdist(SLI2, method = "jaccard", binary = TRUE)
B3 <- vegdist(SLI3, method = "jaccard", binary = TRUE)
B4 <- vegdist(SLI4, method = "jaccard", binary = TRUE)
B5 <- vegdist(SLI5, method = "jaccard", binary = TRUE)
B6 <- vegdist(SLI6, method = "jaccard", binary = TRUE)

### Vizualização



#### Wikiaves 1

#png("6111.png", width = 500, height = 500)
fviz_dist(A1) + 
  scale_fill_gradient(low = "yellow", high = "red")
#dev.off()

#### Wikiaves 2

#png("6112.png",  width = 500, height = 500)
fviz_dist(A2) + 
  scale_fill_gradient(low = "yellow", high = "red")
#dev.off()

#### Wikiaves 3

#png("6113.png",  width = 500, height = 500)
fviz_dist(A3) + 
  scale_fill_gradient(low = "yellow", high = "red")
#dev.off()

#### Wikiaves 4

#png("6114.png",  width = 500, height = 500)
fviz_dist(A4) + 
  scale_fill_gradient(low = "yellow", high = "red")
#dev.off()

#### Wikiaves 5

#png("6115.png",  width = 500, height = 500)
fviz_dist(A5) + 
  scale_fill_gradient(low = "yellow", high = "red")
#dev.off()

#### Wikiaves 6

#png("6116.png",  width = 500, height = 500)
fviz_dist(A6) + 
  scale_fill_gradient(low = "yellow", high = "red")
#dev.off()

#### SpeciesLink 1

#png("6117.png",  width = 500, height = 500)
fviz_dist(B1) + 
  scale_fill_gradient(low = "yellow", high = "red")
#dev.off()

#### SpeciesLink 2

#png("6118.png",  width = 500, height = 500)
fviz_dist(B2) + 
  scale_fill_gradient(low = "yellow", high = "red")
#dev.off()

#### SpeciesLink 3

#png("6119.png",  width = 500, height = 500)
fviz_dist(B3) + 
  scale_fill_gradient(low = "yellow", high = "red")
#dev.off()

#### SpeciesLink 4

#png("61110.png",  width = 500, height = 500)
fviz_dist(B4) + 
  scale_fill_gradient(low = "yellow", high = "red")
#dev.off()

#### SpeciesLink 5

#png("61111.png",  width = 500, height = 500)
fviz_dist(B5) + 
  scale_fill_gradient(low = "yellow", high = "red")
#dev.off()

#### SpeciesLink 6

#png("61112.png",  width = 500, height = 500)
fviz_dist(B6) + 
  scale_fill_gradient(low = "yellow", high = "red")
#dev.off()

## Dendograma

#png("611.png", width = 2000, height = 1000)
X1 <- hclust(A1, "ward.D2")
fviz_dend(X1)
#dev.off()

#png("612.png", width = 2000, height = 1000)
X2 <- hclust(A2, "ward.D2")
fviz_dend(X2)
#dev.off()

#png("613.png", width = 2000, height = 1000)
X3 <- hclust(A3, "ward.D2")
fviz_dend(X3)
#dev.off()

#png("614.png", width = 2000, height = 1000)
X4 <- hclust(A4, "ward.D2")
fviz_dend(X4)
#dev.off()

#png("615.png", width = 2000, height = 1000)
X5 <- hclust(A5, "ward.D2")
fviz_dend(X5)
#dev.off()

#png("616.png", width = 2000, height = 1000)
X6 <- hclust(A6, "ward.D2")
fviz_dend(X6)
#dev.off()

#png("621.png", width = 2000, height = 1000)
Y1 <- hclust(B1, "ward.D2")
fviz_dend(Y1)
#dev.off()

#png("622.png", width = 2000, height = 1000)
Y2 <- hclust(B2, "ward.D2")
fviz_dend(Y2)
#dev.off()

#png("623.png", width = 2000, height = 1000)
Y3 <- hclust(B3, "ward.D2")
fviz_dend(Y3)
#dev.off()

#png("624.png", width = 2000, height = 1000)
Y4 <- hclust(B4, "ward.D2")
fviz_dend(Y4)
#dev.off()

#png("625.png", width = 2000, height = 1000)
Y5 <- hclust(B5, "ward.D2")
fviz_dend(Y5)
#dev.off()

#png("626.png", width = 2000, height = 1000)
Y6 <- hclust(B6, "ward.D2")
fviz_dend(Y6)
#dev.off()


M1 <- as.dendrogram(X6)

M2 <- as.dendrogram(Y6)

## Cophenetic Correlation

#### Wikiaves

CPW1 <- cophenetic(X1)
cor (A1, CPW1)

CPW2 <- cophenetic(X2)
cor (A2, CPW2)

CPW3 <- cophenetic(X3)
cor (A3, CPW3)

CPW4 <- cophenetic(X4)
cor (A4, CPW4)

CPW5 <- cophenetic(X5)
cor (A5, CPW5)

CPW6 <- cophenetic(X6)
cor (A6, CPW6)

#### SpeciesLink

CPS1 <- cophenetic(Y1)
cor (B1, CPS1)

CPS2 <- cophenetic(Y2)
cor (B2, CPS2)

CPS3 <- cophenetic(Y3)
cor (B3, CPS3)

CPS4 <- cophenetic(Y4)
cor (B4, CPS4)

CPS5 <- cophenetic(Y5)
cor (B5, CPS5)

CPS6 <- cophenetic(Y6)
cor (B6, CPS6)



#####################################################################################3

A <- cor(FE)
B <- cor(tFE)

ggcorrplot(A,
           hc.order = TRUE,
           colors = c("blue", "white", "red"),
           lab = TRUE)

Y <- dist(FE)
X <- hclust(Y, "ward.D2")
fviz_dend(X)

FE <- data.frame(dados[,2:10], row.names = dados$Cidades)
A <- cor(FE)

ggcorrplot(A,
           hc.order = TRUE,
           colors = c("blue", "white", "red"),
           lab = TRUE)

B <- dist(FE)
X <- hclust(B, "ward.D2")
fviz_dend(X)

K <- kmeans(FE, 2)
C <- cbind(K$cluster)
cluSLIot (FE, K$cluster, shade = TRUE, labels = 0)

rm(list = ls())

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

WAV <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/R/S6/WAV-E.xlsx", sheet = "10")
WAV2 <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/R/S6/WAV-E.xlsx", sheet = "20")
WAV3 <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/R/S6/WAV-E.xlsx", sheet = "25")
WAV4 <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/R/S6/WAV-E.xlsx", sheet = "50")
WAV5 <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/R/S6/WAV-E.xlsx", sheet = "100")
WAV6 <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/R/S6/WAV-E.xlsx", sheet = "Geral")


SLI <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/R/S6/SPL-E.xlsx", sheet = "10")
SLI2 <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/R/S6/SPL-E.xlsx", sheet = "20")
SLI3 <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/R/S6/SPL-E.xlsx", sheet = "25")
SLI4 <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/R/S6/SPL-E.xlsx", sheet = "50")
SLI5 <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/R/S6/SPL-E.xlsx", sheet = "100")
SLI6 <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/R/S6/SPL-E.xlsx", sheet = "Geral")

dados <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/R/S6/FE.xlsx")
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

fviz_dist(A1) + 
  scale_fill_gradient(low = "yellow", high = "red")

#### Wikiaves 2

fviz_dist(A2) + 
  scale_fill_gradient(low = "yellow", high = "red")

#### Wikiaves 3

fviz_dist(A3) + 
  scale_fill_gradient(low = "yellow", high = "red")

#### Wikiaves 4

fviz_dist(A4) + 
  scale_fill_gradient(low = "yellow", high = "red")

#### Wikiaves 5

fviz_dist(A5) + 
  scale_fill_gradient(low = "yellow", high = "red")

#### Wikiaves 6

fviz_dist(A6) + 
  scale_fill_gradient(low = "yellow", high = "red")

#### SpeciesLink 1

fviz_dist(B1) + 
  scale_fill_gradient(low = "yellow", high = "red")

#### SpeciesLink 2

fviz_dist(B2) + 
  scale_fill_gradient(low = "yellow", high = "red")

#### SpeciesLink 3

fviz_dist(B3) + 
  scale_fill_gradient(low = "yellow", high = "red")

#### SpeciesLink 4

fviz_dist(B4) + 
  scale_fill_gradient(low = "yellow", high = "red")

#### SpeciesLink 5

fviz_dist(B5) + 
  scale_fill_gradient(low = "yellow", high = "red")

#### SpeciesLink 6

fviz_dist(B6) + 
  scale_fill_gradient(low = "yellow", high = "red")

## Dendograma

X1 <- hclust(A1, "ward.D2")
fviz_dend(X1)

X2 <- hclust(A2, "ward.D2")
fviz_dend(X2)

X3 <- hclust(A3, "ward.D2")
fviz_dend(X3)

X4 <- hclust(A4, "ward.D2")
fviz_dend(X4)

X5 <- hclust(A5, "ward.D2")
fviz_dend(X5)

X6 <- hclust(A6, "ward.D2")
fviz_dend(X6)

Y1 <- hclust(B1, "ward.D2")
fviz_dend(Y1)

Y2 <- hclust(B2, "ward.D2")
fviz_dend(Y2)

Y3 <- hclust(B3, "ward.D2")
fviz_dend(Y3)

Y4 <- hclust(B4, "ward.D2")
fviz_dend(Y4)

Y5 <- hclust(B5, "ward.D2")
fviz_dend(Y5)

Y6 <- hclust(B6, "ward.D2")
fviz_dend(Y6)

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

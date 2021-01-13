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

WAV <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/R/S6/WAV-E.xlsx")
SPL <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/R/S6/SPL-E.xlsx")
dados <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/R/S6/FE.xlsx")
FE <- data.frame(dados[,6:10], row.names = dados$Cidades)
tFE <- t(FE)


## Arrumando os dados

WAV <- t(WAV)
WAV <- WAV[-1,]
is.numeric(WAV)
SPL <- t(SPL)
SPL <- SPL[-1,]
is.numeric(SPL)

## Matrizes de distancia (Jaccard)

### Wikiaves

A1 <- vegdist(WAV, method = "jaccard", binary = TRUE)
A2 <- dist.binary(WAV, method = "1")
A3 <- dist(WAV, method = "binary")

### SpeciesLink

B1 <- vegdist(SPL, method = "jaccard", binary = TRUE)
B2 <- dist.binary(SPL, method = "1")
B3 <- dist(SPL, method = "binary")

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

#### SpeciesLink 1

fviz_dist(B1) + 
  scale_fill_gradient(low = "yellow", high = "red")

#### SpeciesLink 2

fviz_dist(B2) + 
  scale_fill_gradient(low = "yellow", high = "red")

#### SpeciesLink 3

fviz_dist(B3) + 
  scale_fill_gradient(low = "yellow", high = "red")

## Dendograma

X1 <- hclust(A1, "ward.D2")
fviz_dend(X1)

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
clusplot (FE, K$cluster, shade = TRUE, labels = 0)

rm(list = ls())
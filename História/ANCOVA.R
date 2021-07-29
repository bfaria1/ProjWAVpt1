library(rstatix)
library(car)
library(emmeans)
library(multcomp)
library(dplyr)
library("openxlsx") 

dados <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/R/ProjWAV/Excel/S7/ancova.xlsx", rowNames = TRUE)

# Teste se os fatores externos têm influência quanto a quantidade de espécies (N=173)

A <- aov(Altitude ~ WAVE, data = dados)
summary(A)

A <- aov(Área ~ WAVE, data = dados)
summary(A)

A <- aov(População ~ WAVE, data = dados)
summary(A)

A <- aov(Latitude ~ WAVE, data = dados)
summary(A)

A <- aov(Longitude ~ WAVE, data = dados)
summary(A)

A <- aov(Altitude ~ SLIE, data = dados)
summary(A)

A <- aov(Área ~ SLIE, data = dados)
summary(A)

A <- aov(População ~ SLIE, data = dados)
summary(A)

A <- aov(Latitude ~ SLIE, data = dados)
summary(A)

A <- aov(Longitude ~ SLIE, data = dados)
summary(A)


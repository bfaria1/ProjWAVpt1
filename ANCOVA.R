library(rstatix)
library(car)
library(emmeans)
library(multcomp)
library(dplyr)
library("openxlsx") 

dados <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/R/ProjWAV/Excel/S7/ancova.xlsx", rowNames = TRUE)

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


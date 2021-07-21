library(pacman)

pacman::p_load(dplyr, car, rstatix, lmtest, ggpubr, QuantPsyc, psych, scatterplot3d, ggplot2, openxlsx, permute, lattice, vegan,geosphere)

dados1 <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S7/RM.xlsx", sheet = "coutliers")
dados2 <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S7/RM.xlsx", sheet = "soutliers")

# Com Outliers

modWR <- lm(WAVR ~ AL + PO + AR + LA + LO, dados1)
summary(modWR)

modWE <- lm(WAVE ~ WAVR + AL + PO + AR + LA + LO, dados1)
summary(modWE)

modSR <- lm(SLIR ~ AL + PO + AR + LA + LO, dados1)
summary(modSR)

modSE <- lm(SLIE ~ SLIR + AL + PO + AR + LA + LO, dados1)
summary(modSE)

# Sem Outliers

modWR <- lm(WAVR ~ AL + PO + AR + LA + LO, dados2)
summary(modWR)

modWE <- lm(WAVE ~ WAVR + AL + PO + AR + LA + LO, dados2)
summary(modWE)

modSR <- lm(SLIR ~ AL + PO + AR + LA + LO, dados2)
summary(modSR)

modSE <- lm(SLIE ~ SLIR + AL + PO + AR + LA + LO, dados2)
summary(modSE)

rm(list = ls())

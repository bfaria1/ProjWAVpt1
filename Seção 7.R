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

cor.test(dados2$AL, dados2$AR,  method = "spearman")
cor.test(dados2$AL, dados2$PO,  method = "spearman")
cor.test(dados2$AL, dados2$LA,  method = "spearman")
cor.test(dados2$AL, dados2$LO,  method = "spearman")
cor.test(dados2$AL, dados2$WAVR,  method = "spearman")
cor.test(dados2$AL, dados2$SLIR,  method = "spearman")

cor.test(dados2$AR, dados2$PO,  method = "spearman")
cor.test(dados2$AR, dados2$LA,  method = "spearman")
cor.test(dados2$AR, dados2$LO,  method = "spearman")
cor.test(dados2$AR, dados2$WAVR,  method = "spearman")
cor.test(dados2$AR, dados2$SLIR,  method = "spearman")

cor.test(dados2$PO, dados2$LA,  method = "spearman")
cor.test(dados2$PO, dados2$LO,  method = "spearman")
cor.test(dados2$PO, dados2$WAVR,  method = "spearman")
cor.test(dados2$PO, dados2$SLIR,  method = "spearman")

cor.test(dados2$LA, dados2$LO,  method = "spearman")
cor.test(dados2$LA, dados2$WAVR,  method = "spearman")
cor.test(dados2$LA, dados2$SLIR,  method = "spearman")

cor.test(dados2$LO, dados2$WAVR,  method = "spearman")
cor.test(dados2$LO, dados2$SLIR,  method = "spearman")


rm(list = ls())

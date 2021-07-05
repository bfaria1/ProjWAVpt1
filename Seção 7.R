library(pacman)

pacman::p_load(dplyr, car, rstatix, lmtest, ggpubr, QuantPsyc, psych, scatterplot3d, ggplot2, openxlsx)

dados <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S7/RM.xlsx")

# Registros

modMS1 <- lm(RS ~ LAS + LOS, dados)
modMS2 <- lm(RS ~ LAS + LOS + AS, dados)
modlaS <- lm(RS ~ LAS, dados)
modloS <- lm(RS ~ LOS, dados)
modAS <- lm(RS ~ AS, dados)

modMW1 <- lm(RW ~ LAW + LOW, dados)
modMW2 <- lm(RW ~ LAW + LOW + AW, dados)
modlaW <- lm(RW ~ LAW, dados)
modloW <- lm(RW ~ LOW, dados)
modAW <- lm(RW ~ AW, dados)

modMW21 <- lm(RW2 ~ LAW2 + LOW2, dados)
modMW22 <- lm(RW2 ~ LAW2 + LOW2 + AW2, dados)
modlaW2 <- lm(RW2 ~ LAW2, dados)
modloW2 <- lm(RW2 ~ LOW2, dados)
modAW2 <- lm(RW2 ~ AW2, dados)

par(mfrow =c(2,2))

plot(modMW1)
plot(modMS1)
plot(modMW21)

plot(modMW2)
plot(modMS2)
plot(modMW22)

par(mfrow =c(1,1))

vif(modMW1)
vif(modMS1)
vif(modMS2)
vif(modMW2)
vif(modMW21)
vif(modMW22)

# não há multicolinearidade

summary(modMW1)
summary(modMS1)
summary(modMW21)

summary(modMW2)
summary(modMS2)
summary(modMW22)


# Qual o melhor modelo?

anova(modMS1, modMS2, modlaS, modloS, modAS)
AIC(modMS1, modMS2, modlaS, modloS, modAS)
BIC(modMS1, modMS2, modlaS, modloS, modAS)

anova(modMW1, modMW2, modlaW, modloW, modAW)
AIC(modMW1, modMW2, modlaW, modloW, modAW)
BIC(modMW1, modMW2, modlaW, modloW, modAW)

anova(modMW21, modMW22, modlaW2, modloW2, modAW2)
AIC(modMW21, modMW22, modlaW2, modloW2, modAW2)
BIC(modMW21, modMW22, modlaW2, modloW2, modAW2)

G1 <- scatterplot3d(dados$RS ~ dados$LAS + dados$LOS, 
                   pch = 16,
                   angle = 45,
                   box = FALSE,
                   color = "red",
                   xlab = "Latitude (º)", ylab = "Longitude (º)", zlab = "Registros (Log10)")

G2 <- scatterplot3d(dados$RW ~ dados$LAW + dados$LOW, 
                   pch = 16,
                   angle = 45,
                   box = FALSE,
                   color = "blue",
                   xlab = "Latitude (º)", ylab = "Longitude (º)", zlab = "Registros (Log10)")

G3 <- scatterplot3d(dados$RW2 ~ dados$LAW2 + dados$LOW2, 
                    pch = 16,
                    angle = 45,
                    box = FALSE,
                    color = "green",
                    xlab = "Latitude (º)", ylab = "Longitude (º)", zlab = "Registros (Log10)")

# Espécies

modMS1 <- lm(ES ~ LAS + LOS, dados)
modMS2 <- lm(ES ~ LAS + LOS + AS, dados)
modMS3 <- lm(ES ~ LAS + LOS + RS, dados)
modMS4 <- lm(ES ~ LAS + LOS + AS + RS, dados)
modlaS <- lm(ES ~ LAS, dados)
modloS <- lm(ES ~ LOS, dados)
modAS <- lm(ES ~ AS, dados)
modRS <- lm(ES ~ RS, dados)

modMW1 <- lm(EW ~ LAW + LOW, dados)
modMW2 <- lm(EW ~ LAW + LOW + AW, dados)
modMW3 <- lm(EW ~ LAW + LOW + RW, dados)
modMW4 <- lm(EW ~ LAW + LOW + AW + RW, dados)
modlaW <- lm(EW ~ LAW, dados)
modloW <- lm(EW ~ LOW, dados)
modAW <- lm(EW ~ AW, dados)
modRW <- lm(EW ~ RW, dados)

modMW21 <- lm(EW2 ~ LAW2 + LOW2, dados)
modMW22 <- lm(EW2 ~ LAW2 + LOW2 + AW2, dados)
modMW23 <- lm(EW2 ~ LAW2 + LOW2 + RW2, dados)
modMW24 <- lm(EW2 ~ LAW2 + LOW2 + AW2 + RW2, dados)
modlaW2 <- lm(EW2 ~ LAW2, dados)
modloW2 <- lm(EW2 ~ LOW2, dados)
modAW2 <- lm(EW2 ~ AW2, dados)
modRW2 <- lm(EW2 ~ RW2, dados)




par(mfrow =c(2,2))

plot(modMW1)
plot(modMS1)
plot(modMW21)

plot(modMW2)
plot(modMS2)
plot(modMW22)

plot(modMW3)
plot(modMS3)
plot(modMW23)

plot(modMW4)
plot(modMS4)
plot(modMW24)

par(mfrow =c(1,1))


vif(modMW1)
vif(modMW2)
vif(modMW3)
vif(modMW4)

vif(modMS1)
vif(modMS2)
vif(modMS3)
vif(modMS4)

vif(modMW21)
vif(modMW22)
vif(modMW23)
vif(modMW24)



summary(modMW1)
summary(modMS1)
summary(modMW21)

summary(modMW2)
summary(modMS2)
summary(modMW22)

summary(modMW3)
summary(modMS3)
summary(modMW23)

summary(modMW4)
summary(modMS4)
summary(modMW24)



AIC(modMS1, modMS2,modMS3,modMS4, modlaS, modloS, modAS, modRS)
BIC(modMS1, modMS2,modMS3,modMS4, modlaS, modloS, modAS, modRS)

AIC(modMW1, modMW2,modMW3, modMW4, modlaW, modloW, modAW, modRW)
BIC(modMW1, modMW2,modMW3, modMW4, modlaW, modloW, modAW, modRW)

AIC(modMW21, modMW22,modMW23, modMW24, modlaW2, modloW2, modAW2, modRW2)
BIC(modMW21, modMW22,modMW23, modMW24, modlaW2, modloW2, modAW2, modRW2)

rm(list = ls())

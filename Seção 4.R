#Importando bibliotecas e dados

library("openxlsx") 
library(ggplot2)
library(RColorBrewer)
library(hrbrthemes)
library(ggpubr)

dados <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/R/S4/WS.xlsx")
RW <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/R/S4/RW.xlsx")
RS <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/R/S4/RS.xlsx")
RW2 <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/R/S4/RW2.xlsx")


#Registros 

gráfico <- ggplot(dados, aes(x = A2, y = A1))+
  geom_point()+
  stat_smooth(method = "lm",
              se = FALSE,
              fullrange = TRUE,
              col = "red",
              size = 1)+
  labs( x ="Número de Registros SpeciesLink (Log10)",
        y =  "Número de Registros Wikiaves (Log10)")+
  theme_classic()

A <- lm(A1~A2, data = dados)
summary(A)

R <- resid(A)

resíduos <- ggplot()+
  geom_histogram(aes (x = R,
                      y = stat(count) / sum(count)*100, 
                      #fill = I("blue"),
                      col = I("black"),
                      alpha = I(.5)),
                 bins = 25)+
  labs(y = "Frequência Relativa (%)",
       x = "Resíduos") +
  theme_classic()

ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1)

#Espécies

gráfico <- ggplot(dados, aes(x = B2, y = B1))+
  geom_point()+
  stat_smooth(method = "lm",
              se = FALSE,
              fullrange = TRUE,
              col = "red",
              size = 1)+
  labs( x ="Número de Espécies SpeciesLink (Log10)",
        y =  "Número de Espécies Wikiaves (Log10)")+
  theme_classic()

A <- lm(B1~B2, data = dados)
summary(A)

R <- resid(A)

resíduos <- ggplot()+
  geom_histogram(aes (x = R,
                      y = stat(count) / sum(count)*100, 
                      #fill = I("blue"),
                      col = I("black"),
                      alpha = I(.5)),
                 bins = 25)+
  labs(y = "Frequência Relativa (%)",
       x = "Resíduos") +
  theme_classic()

ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1)

#Registros por Espécie

gráfico <- ggplot(dados, aes(x = C1, y = C2, color = L1, alpha =I(.5)))+
  geom_point()+
  scale_colour_manual(values = c("red", "blue","green")) +
  stat_smooth(method = "lm",
              formula = y~poly(x,2),
              se = FALSE,
              fullrange = TRUE,
              size = 1)+
  labs( x ="Número de Registros (Log10)",
        y =  "Número de Espécies (Log10)",
        colour = " ")+
  theme_classic()

A <- lm(WE~poly(WR,2), data = RW)
summary(A)
B <- lm(SE~poly(SR,2), data = RS)
summary(B)
C <- lm(W2E~poly(W2R,2), data = RW2)
summary(C)
R1 <- resid (A)
R2 <- resid (B)
R3 <- resid (C)

resíduos <- ggplot()+
  geom_histogram(aes (x = R1,
                      y = stat(count) / sum(count)*100, 
                      fill = I("blue"),
                      col = I("black"),
                      alpha = I(.5)),
                 bins = 25)+
  geom_histogram(aes (x = R2,
                      y = stat(count) / sum(count)*100, 
                      fill = I("red"),
                      col = I("black"),
                      alpha = I(.5)),
                 bins = 25)+
  geom_histogram(aes (x = R3,
                      y = stat(count) / sum(count)*100, 
                      fill = I("green"),
                      col = I("black"),
                      alpha = I(.5)),
                 bins = 25) +
  labs(y = "Frequência Relativa (%)",
       x = "Resíduos") +
  theme_classic()

ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")

rm(list=ls())

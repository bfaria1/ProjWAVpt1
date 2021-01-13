library("openxlsx") 
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(ggpubr)
library(tidyr)

AL <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/R/S3/Altitude.xlsx")
AR <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/R/S3/Area.xlsx")
POP <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/R/S3/Populacao.xlsx")
LA <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/R/S3/Latitude.xlsx")
LO <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/R/S3/Longitude.xlsx")
dados <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/R/S3/Auxiliar.xlsx")

#Altitude

gráfico <- ggplot(AL, aes(x = FR, y = R, colour = L1, alpha =I(.5)))+
  geom_point()+
  scale_colour_manual(values = c("red", "blue", "green")) +
  stat_smooth(method = "lm",
              se = FALSE,
              fullrange = TRUE,
              size = 1)+
  labs( x ="Altitude (m)",
        y =  "Número de Registros (Log10)",
        colour = " ")+
  #guides(colour=FALSE)+
  theme_classic()

REW <- lm(WAVRAL~ALWAVR, data = dados)
RES <- lm(SPLRAL~ALSPLR, data = dados)
RE <- lm(WAV2RAL~ALWAV2R, data = dados)

summary(REW)
summary(RES) 
summary(RE) 

R1 <- resid(REW)
R2 <- resid(RES)
R3 <- resid(RE)


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
                 bins = 25)+
  labs(y = "Frequência Relativa (%)",
       x = "Resíduos") +
  theme_classic()

ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")

gráfico <- ggplot(AL, aes(x = FE, y = E, color = L2, alpha =I(.5)))+
  geom_point()+
  scale_colour_manual(values = c("red", "blue", "green")) +
  stat_smooth(method = "lm",
              se = FALSE,
              fullrange = TRUE,
              size = 1)+
  labs( x = "Altitude (m)",
        y = "Número de Espécies (Log10)",
        colour = " ")+
  theme_classic()

REW <- lm(WAVEAL~ALWAVE, data = dados)
RES <- lm(SPLEAL~ALSPLE, data = dados)
RE <- lm(WAV2EAL~ALWAV2E, data = dados)

summary(REW)
summary(RES)
summary(RE)

R1 <- resid(REW)
R2 <- resid(RES)
R3 <- resid(RE)

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
                 bins = 25)+
  labs(y = "Frequência Relativa (%)",
       x = "Resíduos") +
  theme_classic()

ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")

#Area

gráfico <- ggplot(AR, aes(x = FR, y = R, color = L1, alpha =I(.5)))+
  geom_point()+
  scale_colour_manual(values = c("red", "blue", "green")) +
  stat_smooth(method = "lm",
              se = FALSE,
              fullrange = TRUE,
              size = 1)+
  labs( x ="Área (Log10(Km^2))",
        y =  "Número de Registros (Log10)",
        colour = " ")+
  theme_classic()

REW <- lm(WAVRAR~ARWAVR, data = dados)
RES <- lm(SPLRAR~ARSPLR, data = dados)
RE <- lm(WAV2RAR~ARWAV2R, data = dados)

summary(REW)
summary(RES)
summary(RE)

R1 <- resid(REW)
R2 <- resid(RES)
R3 <- resid(RE)

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
                 bins = 25)+
  labs(y = "Frequência Relativa (%)",
       x = "Resíduos") +
  theme_classic()

ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")

gráfico <- ggplot(AR, aes(x = FE, y = E, color = L2, alpha =I(.5)))+
  geom_point()+
  scale_colour_manual(values = c("red","blue" , "green")) +
  stat_smooth(method = "lm",
              se = FALSE,
              fullrange = TRUE,
              size = 1)+
  labs( x = "Área (Log10(Km^2))",
        y = "Número de Espécies (Log10)",
        colour = " ")+
  theme_classic()

REW <- lm(WAVEAR~ARWAVE, data = dados)
RES <- lm(SPLEAR~ARSPLE, data = dados)
RE <- lm(WAV2EAR~ARWAV2E, data = dados)

summary(REW)
summary(RES)
summary(RE) 

R1 <- resid(REW)
R2 <- resid(RES)
R3 <- resid(RE)

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
                 bins = 25)+
  labs(y = "Frequência Relativa (%)",
       x = "Resíduos") +
  theme_classic()

ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")

#População

gráfico <- ggplot(POP, aes(x = FR, y = R, color = L1, alpha =I(.5)))+
  geom_point()+
  scale_colour_manual(values = c("red","blue" , "green")) +
  stat_smooth(method = "lm",
              se = FALSE,
              fullrange = TRUE,
              size = 1)+
  labs( x ="População (Log10)",
        y =  "Número de Registros (Log10)",
        colour = " ")+
  theme_classic()

REW <- lm(WAVRP~PWAVR, data = dados)
RES <- lm(SPLRP~PSPLR, data = dados)
RE <- lm(WAV2RP~PWAV2R, data = dados)

summary(REW)
summary(RES)
summary(RE)

R1 <- resid(REW)
R2 <- resid(RES)
R3 <- resid(RE)

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
                 bins = 25)+
  labs(y = "Frequência Relativa (%)",
       x = "Resíduos",
       colour = " ") +
  theme_classic()

ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")

gráfico <- ggplot(POP, aes(x = FE, y = E, color = L2, alpha =I(.5)))+
  geom_point()+
  scale_colour_manual(values = c("red","blue" , "green")) +
  stat_smooth(method = "lm",
              se = FALSE,
              fullrange = TRUE,
              size = 1)+
  labs( x = "População (Log10)",
        y = "Número de Espécies (Log10)")+
  theme_classic()

REW <- lm(WAVRE~PWAVE, data = dados)
RES <- lm(SPLEP~PSPLE, data = dados)
RE <- lm(WAV2EP~PWAV2E, data = dados)

summary(REW)
summary(RES)
summary(RE)

R1 <- resid(REW)
R2 <- resid(RES)
R3 <- resid(RE)

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
                 bins = 25)+
  labs(y = "Frequência Relativa (%)",
       x = "Resíduos") +
  theme_classic()

ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")

#Latitude

gráfico <- ggplot(LA, aes(x = FR, y = R, color = L1, alpha =I(.5)))+
  geom_point()+
  scale_colour_manual(values = c("red","blue", "green")) +
  stat_smooth(method = "lm",
              se = FALSE,
              fullrange = TRUE,
              size = 1)+
  labs( x ="Latitude (º)",
        y =  "Número de Registros (Log10)",
        colour = " ")+
  theme_classic()

REW <- lm(WAVRLA~LAWAVR, data = dados)
RES <- lm(SPLRLA~LASPLR, data = dados)
RE <- lm(WAV2RLA~LAWAV2R, data = dados)

summary(REW) 
summary(RES)
summary(RE)

R1 <- resid(REW)
R2 <- resid(RES)
R3 <- resid(RE)

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
                 bins = 25)+
  labs(y = "Frequência Relativa (%)",
       x = "Resíduos") +
  theme_classic()

ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")


gráfico <- ggplot(LA, aes(x = FE, y = E, color = L2, alpha =I(.5)))+
  geom_point()+
  scale_colour_manual(values = c("red","blue", "green")) +
  stat_smooth(method = "lm",
              se = FALSE,
              fullrange = TRUE,
              size = 1)+
  labs( x ="Latitude (º)",
        y = "Número de Espécies (Log10)",
        colour = " ")+
  theme_classic()

REW <- lm(WAVELA~LAWAVE, data = dados)
RES <- lm(SPLELA~LASPLE, data = dados)
RE <- lm(WAV2ELA~LAWAV2E, data = dados)

summary(REW)
summary(RES)
summary(RE)

R1 <- resid(REW)
R2 <- resid(RES)
R3 <- resid(RE)

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
                 bins = 25)+
  labs(y = "Frequência Relativa (%)",
       x = "Resíduos") +
  theme_classic()

ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")

#Longitude


gráfico <- ggplot(LO, aes(x = FR, y = R, color = L1, alpha =I(.5)))+
  geom_point()+
  scale_colour_manual(values = c("red","blue", "green")) +
  stat_smooth(method = "lm",
              se = FALSE,
              fullrange = TRUE,
              size = 1)+
  labs( x ="Longitude (º)",
        y =  "Número de Registros (Log10)",
        colour = " ")+
  theme_classic()

REW <- lm(WAVRLO~LOWAVR, data = dados)
RES <- lm(SPLRLO~LOSPLR, data = dados)
RE <- lm(WAV2RLO~LOWAV2R, data = dados)

summary(REW)
summary(RES)
summary(RE)

R1 <- resid(REW)
R2 <- resid(RES)
R3 <- resid(RE)

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
                 bins = 25)+
  labs(y = "Frequência Relativa (%)",
       x = "Resíduos") +
  theme_classic()

ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")


gráfico <- ggplot(LO, aes(x = FE, y = E, color = L2, alpha =I(.5)))+
  geom_point()+
  scale_colour_manual(values = c("red", "blue", "green")) +
  stat_smooth(method = "lm",
              se = FALSE,
              fullrange = TRUE,
              size = 1)+
  labs( x ="Longitude (º)",
        y = "Número de Espécies (Log10)",
        colour = " ")+
  theme_classic()

REW <- lm(WAVELO~LOWAVE, data = dados)
RES <- lm(SPLELO~LOSPLE, data = dados)
RE <- lm(WAV2ELO~LOWAV2E, data = dados)

summary(REW) 
summary(RES)
summary(RE)

R1 <- resid(REW)
R2 <- resid(RES)
R3 <- resid(RE)

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
                 bins = 25)+
  labs(y = "Frequência Relativa (%)",
       x = "Resíduos") +
  theme_classic()

ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")

rm(list=ls())

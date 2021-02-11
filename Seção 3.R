library("openxlsx") 
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(ggpubr)
library(tidyr)
library(hrbrthemes)
library(sf)
library(geobr)
library(factoextra)
library(ade4)
library(vegan)
library(FD)
library(ggcorrplot)



# Seção 3


AL <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/R/S3/Altitude.xlsx")
AR <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/R/S3/Area.xlsx")
POP <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/R/S3/Populacao.xlsx")
LA <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/R/S3/Latitude.xlsx")
LO <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/R/S3/Longitude.xlsx")
dados <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/R/S3/Auxiliar.xlsx")


## Estatísticas 

### Altitude


REW <- lm(WAVRAL~ALWAVR, data = dados)
RES <- lm(SPLRAL~ALSPLR, data = dados)
RE <- lm(WAV2RAL~ALWAV2R, data = dados)


#### Registros Wikiaves


summary(REW)


#### Registros SpeciesLink


summary(RES) 


#### Registros Wikiaves 2


summary(RE)



REW <- lm(WAVEAL~ALWAVE, data = dados)
RES <- lm(SPLEAL~ALSPLE, data = dados)
RE <- lm(WAV2EAL~ALWAV2E, data = dados)


#### Espécies Wikiaves


summary(REW)


#### Espécies SpeciesLink


summary(RES) 


#### Espécies Wikiaves 2


summary(RE)


### Área


REW <- lm(WAVRAR~ARWAVR, data = dados)
RES <- lm(SPLRAR~ARSPLR, data = dados)
RE <- lm(WAV2RAR~ARWAV2R, data = dados)


#### Registros Wikiaves


summary(REW)


#### Registros SpeciesLink


summary(RES) 


#### Registros Wikiaves 2


summary(RE)



REW <- lm(WAVEAR~ARWAVE, data = dados)
RES <- lm(SPLEAR~ARSPLE, data = dados)
RE <- lm(WAV2EAR~ARWAV2E, data = dados)


#### Espécies Wikiaves


summary(REW)


#### Espécies SpeciesLink


summary(RES) 


#### Espécies Wikiaves 2


summary(RE)



### População


REW <- lm(WAVRP~PWAVR, data = dados)
RES <- lm(SPLRP~PSPLR, data = dados)
RE <- lm(WAV2RP~PWAV2R, data = dados)


#### Registros Wikiaves


summary(REW)


#### Registros SpeciesLink


summary(RES) 


#### Registros Wikiaves 2


summary(RE)



REW <- lm(WAVRE~PWAVE, data = dados)
RES <- lm(SPLEP~PSPLE, data = dados)
RE <- lm(WAV2EP~PWAV2E, data = dados)


#### Espécies Wikiaves


summary(REW)


#### Espécies SpeciesLink


summary(RES) 


#### Espécies Wikiaves 2


summary(RE)



### Latitude


REW <- lm(WAVRLA~LAWAVR, data = dados)
RES <- lm(SPLRLA~LASPLR, data = dados)
RE <- lm(WAV2RLA~LAWAV2R, data = dados)



#### Registros Wikiaves


summary(REW)


#### Registros SpeciesLink


summary(RES) 


#### Registros Wikiaves 2


summary(RE)



REW <- lm(WAVELA~LAWAVE, data = dados)
RES <- lm(SPLELA~LASPLE, data = dados)
RE <- lm(WAV2ELA~LAWAV2E, data = dados)


#### Espécies Wikiaves


summary(REW)


#### Espécies SpeciesLink


summary(RES) 


#### Espécies Wikiaves 2


summary(RE)


### Longitude


REW <- lm(WAVRLO~LOWAVR, data = dados)
RES <- lm(SPLRLO~LOSPLR, data = dados)
RE <- lm(WAV2RLO~LOWAV2R, data = dados)


#### Registros Wikiaves


summary(REW)


#### Registros SpeciesLink


summary(RES) 


#### Registros Wikiaves 2


summary(RE)



REW <- lm(WAVELO~LOWAVE, data = dados)
RES <- lm(SPLELO~LOSPLE, data = dados)
RE <- lm(WAV2ELO~LOWAV2E, data = dados)


#### Espécies Wikiaves


summary(REW)


#### Espécies SpeciesLink


summary(RES) 


#### Espécies Wikiaves 2


summary(RE)


## Representação 1.1.

### Altitude

#### Registros


AL$L1 <- factor(AL$L1, levels=c("WAV (N = 598)", "SLI (N = 145)", "WAV 2 (N = 141)"))

gráfico <- ggplot(AL, aes(x = FR, y = R, colour = L1))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
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

png("31111.png", width = 1000, height = 450)

ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")

dev.off()

#### Espécies


AL$L2 <- factor(AL$L2, levels=c("WAV (N = 557)", "SLI (N = 147)", "WAV 2 (N = 136)"))

gráfico <- ggplot(AL, aes(x = FE, y = E, color = L2))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
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

png("32111.png", width = 1000, height = 450)

ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")

dev.off()

### Área

#### Registros


AR$L1 <- factor(AR$L1, levels=c("WAV (N = 624)", "SLI (N = 171)", "WAV 2 (N = 168)"))

gráfico <- ggplot(AR, aes(x = FR, y = R, color = L1))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
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

png("31211.png", width = 1000, height = 450)

ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")

dev.off()

#### Espécies


AR$L2 <- factor(AR$L2, levels=c("WAV (N = 605)", "SLI (N = 171)", "WAV 2 (N = 161)"))

gráfico <- ggplot(AR, aes(x = FE, y = E, color = L2))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
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

png("32211.png", width = 1000, height = 450)

ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")

dev.off()

### População

#### Registros



POP$L1 <- factor(POP$L1, levels=c("WAV (N = 619)", "SpeciesLink", "WAV 2 (N = 167)") ,labels = c("WAV (N = 619)", "SLI (N = 173)", "WAV 2 (N = 167)"))

gráfico <- ggplot(POP, aes(x = FR, y = R, color = L1))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
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

png("31311.png", width = 1000, height = 450)

ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")

dev.off()

#### Espécies


POP$L2 <- factor(POP$L2, levels=c("WAV (N = 611)", "SLI (N = 173)", "WAV 2 (N = 158)"))

gráfico <- ggplot(POP, aes(x = FE, y = E, color = L2))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
  stat_smooth(method = "lm",
              se = FALSE,
              fullrange = TRUE,
              size = 1)+
  labs( x = "População (Log10)",
        y = "Número de Espécies (Log10)",
        color = " ")+
  theme_classic()

REW <- lm(WAVRE~PWAVE, data = dados)
RES <- lm(SPLEP~PSPLE, data = dados)
RE <- lm(WAV2EP~PWAV2E, data = dados)

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

png("32311.png", width = 1000, height = 450)

ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")

dev.off()

### Latitude

#### Registros



LA$L1 <- factor(LA$L1, levels=c("WAV (N = 629)", "SLI (N = 174)", "WAV 2 (N = 170)"))

gráfico <- ggplot(LA, aes(x = FR, y = R, color = L1))+
  geom_point()+
  scale_colour_manual(values = c("blue","red", "green")) +
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

png("31411.png", width = 1000, height = 450)

ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")

dev.off()

#### Espécies


LA$L2 <- factor(LA$L2, levels=c("WAV (N = 605)", "SLI (N = 174)", "WAV 2 (N = 164)"))

gráfico <- ggplot(LA, aes(x = FE, y = E, color = L2))+
  geom_point()+
  scale_colour_manual(values = c("blue","red", "green")) +
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

png("32411.png", width = 1000, height = 450)

ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")

dev.off()

### Longitude

#### Registros


LO$L1 <- factor(LO$L1, levels=c("WAV (N = 627)", "SLI (N = 174)", "WAV 2 (N = 170)"))

gráfico <- ggplot(LO, aes(x = FR, y = R, color = L1))+
  geom_point()+
  scale_colour_manual(values = c("blue","red", "green")) +
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

png("31511.png", width = 1000, height = 450)

ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")

dev.off()

#### Espécies


LO$L2 <- factor(LO$L2, levels=c("WAV (N = 602)", "SLI (N = 174)", "WAV 2 (N = 167)"))

gráfico <- ggplot(LO, aes(x = FE, y = E, color = L2))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
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

png("32511.png", width = 1000, height = 450)

ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")

dev.off()

## Representação 1.2.

### Altitude

#### Registros


gráfico <- ggplot(AL, aes(x = FR, y = R, colour = L1))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
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

a <- qplot(fitted(REW), resid(REW))+ 
  geom_point(col = "blue")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

b <- qplot(fitted(RES), resid(RES))+ 
  geom_point(col = "Red")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

c <- qplot(fitted(RE), resid(RE))+ 
  geom_point(col = "Green")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

resíduos <- ggarrange(a,b,c, ncol = 1, nrow = 3)

png("31112.png", width = 1000, height = 450)

ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")

dev.off()

#### Espécies


gráfico <- ggplot(AL, aes(x = FE, y = E, color = L2))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
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


a <- qplot(fitted(REW), resid(REW))+ 
  geom_point(col = "blue")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

b <- qplot(fitted(RES), resid(RES))+ 
  geom_point(col = "Red")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

c <- qplot(fitted(RE), resid(RE))+ 
  geom_point(col = "Green")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

resíduos <- ggarrange(a,b,c, ncol = 1, nrow = 3)

png("32112.png", width = 1000, height = 450)

ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")

dev.off()

### Área

#### Registros



gráfico <- ggplot(AR, aes(x = FR, y = R, color = L1))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
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

a <- qplot(fitted(REW), resid(REW))+ 
  geom_point(col = "blue")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

b <- qplot(fitted(RES), resid(RES))+ 
  geom_point(col = "Red")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

c <- qplot(fitted(RE), resid(RE))+ 
  geom_point(col = "Green")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

resíduos <- ggarrange(a,b,c, ncol = 1, nrow = 3)

png("31212.png", width = 1000, height = 450)

ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")

dev.off()

#### Espécies


gráfico <- ggplot(AR, aes(x = FE, y = E, color = L2))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
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

a <- qplot(fitted(REW), resid(REW))+ 
  geom_point(col = "blue")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

b <- qplot(fitted(RES), resid(RES))+ 
  geom_point(col = "Red")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

c <- qplot(fitted(RE), resid(RE))+ 
  geom_point(col = "Green")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

resíduos <- ggarrange(a,b,c, ncol = 1, nrow = 3)

png("32212.png", width = 1000, height = 450)

ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")

dev.off()

### População

#### Registros


gráfico <- ggplot(POP, aes(x = FR, y = R, color = L1))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
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

a <- qplot(fitted(REW), resid(REW))+ 
  geom_point(col = "blue")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

b <- qplot(fitted(RES), resid(RES))+ 
  geom_point(col = "Red")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

c <- qplot(fitted(RE), resid(RE))+ 
  geom_point(col = "Green")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

resíduos <- ggarrange(a,b,c, ncol = 1, nrow = 3)


png("31312.png", width = 1000, height = 450)

ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")

dev.off()

#### Espécies


gráfico <- ggplot(POP, aes(x = FE, y = E, color = L2))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
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

a <- qplot(fitted(REW), resid(REW))+ 
  geom_point(col = "blue")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

b <- qplot(fitted(RES), resid(RES))+ 
  geom_point(col = "Red")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

c <- qplot(fitted(RE), resid(RE))+ 
  geom_point(col = "Green")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

resíduos <- ggarrange(a,b,c, ncol = 1, nrow = 3)

png("32312.png", width = 1000, height = 450)

ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")

dev.off()

### Latitude

#### Registros


gráfico <- ggplot(LA, aes(x = FR, y = R, color = L1))+
  geom_point()+
  scale_colour_manual(values = c("blue","red", "green")) +
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

a <- qplot(fitted(REW), resid(REW))+ 
  geom_point(col = "blue")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

b <- qplot(fitted(RES), resid(RES))+ 
  geom_point(col = "Red")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

c <- qplot(fitted(RE), resid(RE))+ 
  geom_point(col = "Green")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

resíduos <- ggarrange(a,b,c, ncol = 1, nrow = 3)

png("31412.png", width = 1000, height = 450)

ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")

dev.off()

#### Espécies


gráfico <- ggplot(LA, aes(x = FE, y = E, color = L2))+
  geom_point()+
  scale_colour_manual(values = c("blue","red", "green")) +
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

a <- qplot(fitted(REW), resid(REW))+ 
  geom_point(col = "blue")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

b <- qplot(fitted(RES), resid(RES))+ 
  geom_point(col = "Red")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

c <- qplot(fitted(RE), resid(RE))+ 
  geom_point(col = "Green")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

resíduos <- ggarrange(a,b,c, ncol = 1, nrow = 3)

png("32412.png", width = 1000, height = 450)

ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")

dev.off()

### Longitude

#### Registros


gráfico <- ggplot(LO, aes(x = FR, y = R, color = L1))+
  geom_point()+
  scale_colour_manual(values = c("blue","red", "green")) +
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

a <- qplot(fitted(REW), resid(REW))+ 
  geom_point(col = "blue")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

b <- qplot(fitted(RES), resid(RES))+ 
  geom_point(col = "Red")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

c <- qplot(fitted(RE), resid(RE))+ 
  geom_point(col = "Green")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

resíduos <- ggarrange(a,b,c, ncol = 1, nrow = 3)

png("31512.png", width = 1000, height = 450)

ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")

dev.off()

#### Espécies


gráfico <- ggplot(LO, aes(x = FE, y = E, color = L2))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
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

a <- qplot(fitted(REW), resid(REW))+ 
  geom_point(col = "blue")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

b <- qplot(fitted(RES), resid(RES))+ 
  geom_point(col = "Red")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

c <- qplot(fitted(RE), resid(RE))+ 
  geom_point(col = "Green")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

resíduos <- ggarrange(a,b,c, ncol = 1, nrow = 3)

png("32512.png", width = 1000, height = 450)

ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")

dev.off()

## Representação 1.3.

### Altitude

#### Registros


gráfico <- ggplot(AL, aes(x = FR, y = R, colour = L1))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
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

R1 <- resid (REW)
R2 <- resid (RES)
R3 <- resid (RE)

min(R1,R2,R3)
max(R1,R2,R3)

AX <- hist(R1, 
           plot = FALSE, 
           breaks = c(-2.1,-1.875,-1.65,-1.425,-1.2,-0.925,-0.75,-0.525,-0.3,-0.075,0.15,0.375,0.6,0.825,1.05,1.275,1.5,1.725,1.95,2.175,2.4))
AX$density = AX$counts/sum(AX$counts)*100

BX <- hist(R2, 
           plot = FALSE, 
           breaks = c(-2.1,-1.875,-1.65,-1.425,-1.2,-0.925,-0.75,-0.525,-0.3,-0.075,0.15,0.375,0.6,0.825,1.05,1.275,1.5,1.725,1.95,2.175,2.4))
BX$density = BX$counts/sum(BX$counts)*100

CX <- hist(R3, 
           plot = FALSE, 
           breaks = c(-2.1,-1.875,-1.65,-1.425,-1.2,-0.925,-0.75,-0.525,-0.3,-0.075,0.15,0.375,0.6,0.825,1.05,1.275,1.5,1.725,1.95,2.175,2.4))
CX$density = CX$counts/sum(CX$counts)*100

y1 <- AX$density
y2 <- BX$density
y3 <- CX$density

x1 <- c()
x1[1] <- -2.1
for (i in 1:19){
  x1[i+1] = x1[i]+0.225
}


A <- c()

for (i in 1:20){
  A[i] = "WAV (N = 631)"
}
for (i in 21:40){
  A[i] = "SPL (N = 174)"
}
for (i in 41:60){
  A[i] = "WAV 2 (N = 173)"
}

B <- c()

for (i in 1:20){
  B[i] = x1[i]
}

for(i in 21:40){
  B[i] = x1[i-20]
}
for(i in 41:60){
  B[i] = x1[i-40]
}

C<- c()

for (i in 1:20){
  C[i] = y1[i]
}

for(i in 21:40){
  C[i] = y2[i-20]
}
for(i in 41:60){
  C[i] = y3[i-40]
}

data <- data.frame(L = A, x = B, y = C)

Resíduos <-  ggplot(data, aes(x = x, y = C, colour = L))+
  geom_line()+
  scale_colour_manual(values = c("red", "blue", "green")) +
  #stat_smooth(method = lm, formula = y ~ poly(x, 4),se = FALSE, fullrange = TRUE)+
  labs(x = "Resíduos",
       y = "Frequência Relativa (%)",
       colour = " ")+
  theme_classic()

png("31113.png", width = 1000, height = 450)

ggarrange(gráfico, Resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")

dev.off()

#### Espécies


gráfico <- ggplot(AL, aes(x = FE, y = E, color = L2))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
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

R1 <- resid (REW)
R2 <- resid (RES)
R3 <- resid (RE)

AX <- hist(R1, 
           plot = FALSE, 
           breaks = c(-1.3,-1.1,-0.9,-0.7,-0.5,-0.3,-0.1,0.1,0.3,0.5,0.7,0.9,1.1,1.3,1.5,1.7,1.9,2.1,2.3,2.5,2.7))
AX$density = AX$counts/sum(AX$counts)*100

BX <- hist(R2, 
           plot = FALSE, 
           breaks = c(-1.3,-1.1,-0.9,-0.7,-0.5,-0.3,-0.1,0.1,0.3,0.5,0.7,0.9,1.1,1.3,1.5,1.7,1.9,2.1,2.3,2.5,2.7))
BX$density = BX$counts/sum(BX$counts)*100

CX <- hist(R3, 
           plot = FALSE, 
           breaks = c(-1.3,-1.1,-0.9,-0.7,-0.5,-0.3,-0.1,0.1,0.3,0.5,0.7,0.9,1.1,1.3,1.5,1.7,1.9,2.1,2.3,2.5,2.7))
CX$density = CX$counts/sum(CX$counts)*100

y1 <- AX$density
y2 <- BX$density
y3 <- CX$density

x1 <- c()
x1[1] <- -1.3
for (i in 1:19){
  x1[i+1] = x1[i]+0.2
}

A <- c()

for (i in 1:20){
  A[i] = "WAV (N = 631)"
}
for (i in 21:40){
  A[i] = "SPL (N = 174)"
}
for (i in 41:60){
  A[i] = "WAV 2 (N = 173)"
}

B <- c()

for (i in 1:20){
  B[i] = x1[i]
}

for(i in 21:40){
  B[i] = x1[i-20]
}
for(i in 41:60){
  B[i] = x1[i-40]
}

C<- c()

for (i in 1:20){
  C[i] = y1[i]
}

for(i in 21:40){
  C[i] = y2[i-20]
}
for(i in 41:60){
  C[i] = y3[i-40]
}

data <- data.frame(L = A, x = B, y = C)

Resíduos <-  ggplot(data, aes(x = x, y = C, colour = L))+
  geom_line()+
  scale_colour_manual(values = c("red", "blue", "green")) +
  #stat_smooth(method = lm, formula = y ~ poly(x, 4),se = FALSE, fullrange = TRUE)+
  labs(x = "Resíduos",
       y = "Frequência Relativa (%)",
       colour = " ")+
  theme_classic()

png("32113.png", width = 1000, height = 450)

ggarrange(gráfico, Resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")

dev.off()

### Área

#### Registros



gráfico <- ggplot(AR, aes(x = FR, y = R, color = L1))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
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

R1 <- resid (REW)
R2 <- resid (RES)
R3 <- resid (RE)

min(R1,R2,R3)
max(R1,R2,R3)

AX <- hist(R1, 
           plot = FALSE, 
           breaks = c(-2.4,-2.165,-1.93,-1.695,-1.46,-1.225,-0.99,-0.755,-0.52,-0.285,-0.05,0.185,0.42,0.655,0.89,1.125,1.36,1.595,1.83,2.065,2.3))
AX$density = AX$counts/sum(AX$counts)*100

BX <- hist(R2, 
           plot = FALSE, 
           breaks = c(-2.4,-2.165,-1.93,-1.695,-1.46,-1.225,-0.99,-0.755,-0.52,-0.285,-0.05,0.185,0.42,0.655,0.89,1.125,1.36,1.595,1.83,2.065,2.3))
BX$density = BX$counts/sum(BX$counts)*100

CX <- hist(R3, 
           plot = FALSE, 
           breaks = c(-2.4,-2.165,-1.93,-1.695,-1.46,-1.225,-0.99,-0.755,-0.52,-0.285,-0.05,0.185,0.42,0.655,0.89,1.125,1.36,1.595,1.83,2.065,2.3))
CX$density = CX$counts/sum(CX$counts)*100

y1 <- AX$density
y2 <- BX$density
y3 <- CX$density

x1 <- c()
x1[1] <- -2.4
for (i in 1:19){
  x1[i+1] = x1[i]+0.235
}

A <- c()

for (i in 1:20){
  A[i] = "WAV (N = 631)"
}
for (i in 21:40){
  A[i] = "SPL (N = 174)"
}
for (i in 41:60){
  A[i] = "WAV 2 (N = 173)"
}

B <- c()

for (i in 1:20){
  B[i] = x1[i]
}

for(i in 21:40){
  B[i] = x1[i-20]
}
for(i in 41:60){
  B[i] = x1[i-40]
}

C<- c()

for (i in 1:20){
  C[i] = y1[i]
}

for(i in 21:40){
  C[i] = y2[i-20]
}
for(i in 41:60){
  C[i] = y3[i-40]
}

data <- data.frame(L = A, x = B, y = C)

Resíduos <-  ggplot(data, aes(x = x, y = C, colour = L))+
  geom_line()+
  scale_colour_manual(values = c("red", "blue", "green")) +
  #stat_smooth(method = lm, formula = y ~ poly(x, 4),se = FALSE, fullrange = TRUE)+
  labs(x = "Resíduos",
       y = "Frequência Relativa (%)",
       colour = " ")+
  theme_classic()

png("31213.png", width = 1000, height = 450)

ggarrange(gráfico, Resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")

dev.off()

#### Espécies


gráfico <- ggplot(AR, aes(x = FE, y = E, color = L2))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
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

R1 <- resid (REW)
R2 <- resid (RES)
R3 <- resid (RE)

min(R1,R2,R3)
max(R1,R2,R3)

AX <- hist(R1, 
           plot = FALSE, 
           breaks = c(-1.4,-1.26,-1.12,-0.98,-0.84,-0.7,-0.56,-0.42,-0.28,-0.14,0,0.14,0.28,0.42,0.56,0.7,0.84,0.98,1.12,1.26,1.4))
AX$density = AX$counts/sum(AX$counts)*100

BX <- hist(R2, 
           plot = FALSE, 
           breaks = c(-1.4,-1.26,-1.12,-0.98,-0.84,-0.7,-0.56,-0.42,-0.28,-0.14,0,0.14,0.28,0.42,0.56,0.7,0.84,0.98,1.12,1.26,1.4))
BX$density = BX$counts/sum(BX$counts)*100

CX <- hist(R3, 
           plot = FALSE, 
           breaks = c(-1.4,-1.26,-1.12,-0.98,-0.84,-0.7,-0.56,-0.42,-0.28,-0.14,0,0.14,0.28,0.42,0.56,0.7,0.84,0.98,1.12,1.26,1.4))
CX$density = CX$counts/sum(CX$counts)*100

y1 <- AX$density
y2 <- BX$density
y3 <- CX$density

x1 <- c()
x1[1] <- -1.4
for (i in 1:19){
  x1[i+1] = x1[i]+0.14
}


A <- c()

for (i in 1:20){
  A[i] = "WAV (N = 631)"
}
for (i in 21:40){
  A[i] = "SPL (N = 174)"
}
for (i in 41:60){
  A[i] = "WAV 2 (N = 173)"
}

B <- c()

for (i in 1:20){
  B[i] = x1[i]
}

for(i in 21:40){
  B[i] = x1[i-20]
}
for(i in 41:60){
  B[i] = x1[i-40]
}

C<- c()

for (i in 1:20){
  C[i] = y1[i]
}

for(i in 21:40){
  C[i] = y2[i-20]
}
for(i in 41:60){
  C[i] = y3[i-40]
}

data <- data.frame(L = A, x = B, y = C)

Resíduos <-  ggplot(data, aes(x = x, y = C, colour = L))+
  geom_line()+
  scale_colour_manual(values = c("red", "blue", "green")) +
  #stat_smooth(method = lm, formula = y ~ poly(x, 4),se = FALSE, fullrange = TRUE)+
  labs(x = "Resíduos",
       y = "Frequência Relativa (%)",
       colour = " ")+
  theme_classic()

png("32213.png", width = 1000, height = 450)

ggarrange(gráfico, Resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")

dev.off()

### População

#### Registros


gráfico <- ggplot(POP, aes(x = FR, y = R, color = L1))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
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

R1 <- resid (REW)
R2 <- resid (RES)
R3 <- resid (RE)

min(R1,R2,R3)
max(R1,R2,R3)

AX <- hist(R1, 
           plot = FALSE, 
           breaks = c(-1.7,-1.495,-1.29,-1.085,-0.88,-0.675,-0.47,-0.265,-0.06,0.145,0.35,0.555,0.76,0.965,1.17,1.375,1.58,1.785,1.99,2.195,2.4))
AX$density = AX$counts/sum(AX$counts)*100

BX <- hist(R2, 
           plot = FALSE, 
           breaks = c(-1.7,-1.495,-1.29,-1.085,-0.88,-0.675,-0.47,-0.265,-0.06,0.145,0.35,0.555,0.76,0.965,1.17,1.375,1.58,1.785,1.99,2.195,2.4))
BX$density = BX$counts/sum(BX$counts)*100

CX <- hist(R3, 
           plot = FALSE, 
           breaks = c(-1.7,-1.495,-1.29,-1.085,-0.88,-0.675,-0.47,-0.265,-0.06,0.145,0.35,0.555,0.76,0.965,1.17,1.375,1.58,1.785,1.99,2.195,2.4))
CX$density = CX$counts/sum(CX$counts)*100

y1 <- AX$density
y2 <- BX$density
y3 <- CX$density

x1 <- c()
x1[1] <- -1.7
for (i in 1:19){
  x1[i+1] = x1[i]+0.205
}


A <- c()

for (i in 1:20){
  A[i] = "WAV (N = 631)"
}
for (i in 21:40){
  A[i] = "SPL (N = 174)"
}
for (i in 41:60){
  A[i] = "WAV 2 (N = 173)"
}

B <- c()

for (i in 1:20){
  B[i] = x1[i]
}

for(i in 21:40){
  B[i] = x1[i-20]
}
for(i in 41:60){
  B[i] = x1[i-40]
}

C<- c()

for (i in 1:20){
  C[i] = y1[i]
}

for(i in 21:40){
  C[i] = y2[i-20]
}
for(i in 41:60){
  C[i] = y3[i-40]
}

data <- data.frame(L = A, x = B, y = C)

Resíduos <-  ggplot(data, aes(x = x, y = C, colour = L))+
  geom_line()+
  scale_colour_manual(values = c("red", "blue", "green")) +
  #stat_smooth(method = lm, formula = y ~ poly(x, 4),se = FALSE, fullrange = TRUE)+
  labs(x = "Resíduos",
       y = "Frequência Relativa (%)",
       colour = " ")+
  theme_classic()

png("31313.png", width = 1000, height = 450)

ggarrange(gráfico, Resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")

dev.off()

#### Espécies


gráfico <- ggplot(POP, aes(x = FE, y = E, color = L2))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
  stat_smooth(method = "lm",
              se = FALSE,
              fullrange = TRUE,
              size = 1)+
  labs( x = "População (Log10)",
        y = "Número de Espécies (Log10)",
        color = " ")+
  theme_classic()

REW <- lm(WAVRE~PWAVE, data = dados)
RES <- lm(SPLEP~PSPLE, data = dados)
RE <- lm(WAV2EP~PWAV2E, data = dados)

R1 <- resid (REW)
R2 <- resid (RES)
R3 <- resid (RE)

min(R1,R2,R3)
max(R1,R2,R3)

AX <- hist(R1, 
           plot = FALSE, 
           breaks = c(-1.2,-1.06,-0.92,-0.78,-0.74,-0.5,-0.36,-0.22,-0.08,0.06,0.2,0.34,0.48,0.62,0.76,0.9,1.04,1.18,1.32,1.46,1.6))
AX$density = AX$counts/sum(AX$counts)*100

BX <- hist(R2, 
           plot = FALSE, 
           breaks = c(-1.2,-1.06,-0.92,-0.78,-0.74,-0.5,-0.36,-0.22,-0.08,0.06,0.2,0.34,0.48,0.62,0.76,0.9,1.04,1.18,1.32,1.46,1.6))
BX$density = BX$counts/sum(BX$counts)*100

CX <- hist(R3, 
           plot = FALSE, 
           breaks = c(-1.2,-1.06,-0.92,-0.78,-0.74,-0.5,-0.36,-0.22,-0.08,0.06,0.2,0.34,0.48,0.62,0.76,0.9,1.04,1.18,1.32,1.46,1.6))
CX$density = CX$counts/sum(CX$counts)*100

y1 <- AX$density
y2 <- BX$density
y3 <- CX$density

x1 <- c()
x1[1] <- -1.2
for (i in 1:19){
  x1[i+1] = x1[i]+0.14
}


A <- c()

for (i in 1:20){
  A[i] = "WAV (N = 631)"
}
for (i in 21:40){
  A[i] = "SPL (N = 174)"
}
for (i in 41:60){
  A[i] = "WAV 2 (N = 173)"
}

B <- c()

for (i in 1:20){
  B[i] = x1[i]
}

for(i in 21:40){
  B[i] = x1[i-20]
}
for(i in 41:60){
  B[i] = x1[i-40]
}

C<- c()

for (i in 1:20){
  C[i] = y1[i]
}

for(i in 21:40){
  C[i] = y2[i-20]
}
for(i in 41:60){
  C[i] = y3[i-40]
}

data <- data.frame(L = A, x = B, y = C)

Resíduos <-  ggplot(data, aes(x = x, y = C, colour = L))+
  geom_line()+
  scale_colour_manual(values = c("red", "blue", "green")) +
  #stat_smooth(method = lm, formula = y ~ poly(x, 4),se = FALSE, fullrange = TRUE)+
  labs(x = "Resíduos",
       y = "Frequência Relativa (%)",
       colour = " ")+
  theme_classic()

png("32313.png", width = 1000, height = 450)

ggarrange(gráfico, Resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")

dev.off()

### Latitude

#### Registros


gráfico <- ggplot(LA, aes(x = FR, y = R, color = L1))+
  geom_point()+
  scale_colour_manual(values = c("blue","red", "green")) +
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

R1 <- resid (REW)
R2 <- resid (RES)
R3 <- resid (RE)

min(R1,R2,R3)
max(R1,R2,R3)

AX <- hist(R1, 
           plot = FALSE, 
           breaks = c(-2.2,-1.965,-1.73,-1.495,-1.26,-1.025,-0.79,-0.555,-0.32,-0.085,0.15,0.385,0.62,0.855,1.09,1.325,1.56,1.795,2.03,2.265,2.5))
AX$density = AX$counts/sum(AX$counts)*100

BX <- hist(R2, 
           plot = FALSE, 
           breaks = c(-2.2,-1.965,-1.73,-1.495,-1.26,-1.025,-0.79,-0.555,-0.32,-0.085,0.15,0.385,0.62,0.855,1.09,1.325,1.56,1.795,2.03,2.265,2.5))
BX$density = BX$counts/sum(BX$counts)*100

CX <- hist(R3, 
           plot = FALSE, 
           breaks = c(-2.2,-1.965,-1.73,-1.495,-1.26,-1.025,-0.79,-0.555,-0.32,-0.085,0.15,0.385,0.62,0.855,1.09,1.325,1.56,1.795,2.03,2.265,2.5))
CX$density = CX$counts/sum(CX$counts)*100

y1 <- AX$density
y2 <- BX$density
y3 <- CX$density

x1 <- c()
x1[1] <- -2.2
for (i in 1:19){
  x1[i+1] = x1[i]+0.235
}


A <- c()

for (i in 1:20){
  A[i] = "WAV (N = 631)"
}
for (i in 21:40){
  A[i] = "SPL (N = 174)"
}
for (i in 41:60){
  A[i] = "WAV 2 (N = 173)"
}

B <- c()

for (i in 1:20){
  B[i] = x1[i]
}

for(i in 21:40){
  B[i] = x1[i-20]
}
for(i in 41:60){
  B[i] = x1[i-40]
}

C<- c()

for (i in 1:20){
  C[i] = y1[i]
}

for(i in 21:40){
  C[i] = y2[i-20]
}
for(i in 41:60){
  C[i] = y3[i-40]
}

data <- data.frame(L = A, x = B, y = C)

Resíduos <-  ggplot(data, aes(x = x, y = C, colour = L))+
  geom_line()+
  scale_colour_manual(values = c("red", "blue", "green")) +
  #stat_smooth(method = lm, formula = y ~ poly(x, 4),se = FALSE, fullrange = TRUE)+
  labs(x = "Resíduos",
       y = "Frequência Relativa (%)",
       colour = " ")+
  theme_classic()

png("31413.png", width = 1000, height = 450)

ggarrange(gráfico, Resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")

dev.off()

#### Espécies


gráfico <- ggplot(LA, aes(x = FE, y = E, color = L2))+
  geom_point()+
  scale_colour_manual(values = c("blue","red", "green")) +
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

R1 <- resid (REW)
R2 <- resid (RES)
R3 <- resid (RE)

min(R1,R2,R3)
max(R1,R2,R3)

AX <- hist(R1, 
           plot = FALSE, 
           breaks = c(-1.4,-1.245,-1.09,-0.935,-0.78,-0.625,-0.47,-0.315,-0.16,-0.005,0.15,0.305,0.46,0.615,0.77,0.925,1.08,1.235,1.39,1.545,1.7))
AX$density = AX$counts/sum(AX$counts)*100

BX <- hist(R2, 
           plot = FALSE, 
           breaks = c(-1.4,-1.245,-1.09,-0.935,-0.78,-0.625,-0.47,-0.315,-0.16,-0.005,0.15,0.305,0.46,0.615,0.77,0.925,1.08,1.235,1.39,1.545,1.7))
BX$density = BX$counts/sum(BX$counts)*100

CX <- hist(R3, 
           plot = FALSE, 
           breaks = c(-1.4,-1.245,-1.09,-0.935,-0.78,-0.625,-0.47,-0.315,-0.16,-0.005,0.15,0.305,0.46,0.615,0.77,0.925,1.08,1.235,1.39,1.545,1.7))
CX$density = CX$counts/sum(CX$counts)*100

y1 <- AX$density
y2 <- BX$density
y3 <- CX$density

x1 <- c()
x1[1] <- -1.4
for (i in 1:19){
  x1[i+1] = x1[i]+0.155
}


A <- c()

for (i in 1:20){
  A[i] = "WAV (N = 631)"
}
for (i in 21:40){
  A[i] = "SPL (N = 174)"
}
for (i in 41:60){
  A[i] = "WAV 2 (N = 173)"
}

B <- c()

for (i in 1:20){
  B[i] = x1[i]
}

for(i in 21:40){
  B[i] = x1[i-20]
}
for(i in 41:60){
  B[i] = x1[i-40]
}

C<- c()

for (i in 1:20){
  C[i] = y1[i]
}

for(i in 21:40){
  C[i] = y2[i-20]
}
for(i in 41:60){
  C[i] = y3[i-40]
}

data <- data.frame(L = A, x = B, y = C)

Resíduos <-  ggplot(data, aes(x = x, y = C, colour = L))+
  geom_line()+
  scale_colour_manual(values = c("red", "blue", "green")) +
  #stat_smooth(method = lm, formula = y ~ poly(x, 4),se = FALSE, fullrange = TRUE)+
  labs(x = "Resíduos",
       y = "Frequência Relativa (%)",
       colour = " ")+
  theme_classic()

png("32413.png", width = 1000, height = 450)

ggarrange(gráfico, Resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")

dev.off()

### Longitude

#### Registros


gráfico <- ggplot(LO, aes(x = FR, y = R, color = L1))+
  geom_point()+
  scale_colour_manual(values = c("blue","red", "green")) +
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

R1 <- resid (REW)
R2 <- resid (RES)
R3 <- resid (RE)

min(R1,R2,R3)
max(R1,R2,R3)

AX <- hist(R1, 
           plot = FALSE, 
           breaks = c(-1.8,-1.59,-1.38,-1.17,-0.96,-0.75,-0.54,-0.33,-0.12,0.09,0.3,0.51,0.72,0.93,1.14,1.35,1.56,1.77,1.98,2.19,2.4))
AX$density = AX$counts/sum(AX$counts)*100

BX <- hist(R2, 
           plot = FALSE, 
           breaks = c(-1.8,-1.59,-1.38,-1.17,-0.96,-0.75,-0.54,-0.33,-0.12,0.09,0.3,0.51,0.72,0.93,1.14,1.35,1.56,1.77,1.98,2.19,2.4))
BX$density = BX$counts/sum(BX$counts)*100

CX <- hist(R3, 
           plot = FALSE, 
           breaks = c(-1.8,-1.59,-1.38,-1.17,-0.96,-0.75,-0.54,-0.33,-0.12,0.09,0.3,0.51,0.72,0.93,1.14,1.35,1.56,1.77,1.98,2.19,2.4))
CX$density = CX$counts/sum(CX$counts)*100

y1 <- AX$density
y2 <- BX$density
y3 <- CX$density

x1 <- c()
x1[1] <- -1.8
for (i in 1:19){
  x1[i+1] = x1[i]+0.21
}

A <- c()

for (i in 1:20){
  A[i] = "WAV (N = 631)"
}
for (i in 21:40){
  A[i] = "SPL (N = 174)"
}
for (i in 41:60){
  A[i] = "WAV 2 (N = 173)"
}

B <- c()

for (i in 1:20){
  B[i] = x1[i]
}

for(i in 21:40){
  B[i] = x1[i-20]
}
for(i in 41:60){
  B[i] = x1[i-40]
}

C<- c()

for (i in 1:20){
  C[i] = y1[i]
}

for(i in 21:40){
  C[i] = y2[i-20]
}
for(i in 41:60){
  C[i] = y3[i-40]
}

data <- data.frame(L = A, x = B, y = C)

Resíduos <-  ggplot(data, aes(x = x, y = C, colour = L))+
  geom_line()+
  scale_colour_manual(values = c("red", "blue", "green")) +
  #stat_smooth(method = lm, formula = y ~ poly(x, 4),se = FALSE, fullrange = TRUE)+
  labs(x = "Resíduos",
       y = "Frequência Relativa (%)",
       colour = " ")+
  theme_classic()

png("31513.png", width = 1000, height = 450)

ggarrange(gráfico, Resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")

dev.off()

#### Espécies


gráfico <- ggplot(LO, aes(x = FE, y = E, color = L2))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
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

R1 <- resid (REW)
R2 <- resid (RES)
R3 <- resid (RE)

min(R1,R2,R3)
max(R1,R2,R3)

AX <- hist(R1, 
           plot = FALSE, 
           breaks = c(-1.2,-1.065,-0.93,-0.795,-0.66,-0.525,-0.39,-0.255,-0.12,0.015,0.15,0.285,0.42,0.555,0.69,0.825,0.96,1.095,1.23,1.365,1.5))
AX$density = AX$counts/sum(AX$counts)*100

BX <- hist(R2, 
           plot = FALSE, 
           breaks = c(-1.2,-1.065,-0.93,-0.795,-0.66,-0.525,-0.39,-0.255,-0.12,0.015,0.15,0.285,0.42,0.555,0.69,0.825,0.96,1.095,1.23,1.365,1.5))
BX$density = BX$counts/sum(BX$counts)*100

CX <- hist(R3, 
           plot = FALSE, 
           breaks = c(-1.2,-1.065,-0.93,-0.795,-0.66,-0.525,-0.39,-0.255,-0.12,0.015,0.15,0.285,0.42,0.555,0.69,0.825,0.96,1.095,1.23,1.365,1.5))
CX$density = CX$counts/sum(CX$counts)*100

y1 <- AX$density
y2 <- BX$density
y3 <- CX$density

x1 <- c()
x1[1] <- -1.2
for (i in 1:19){
  x1[i+1] = x1[i]+0.135
}


A <- c()

for (i in 1:20){
  A[i] = "WAV (N = 631)"
}
for (i in 21:40){
  A[i] = "SPL (N = 174)"
}
for (i in 41:60){
  A[i] = "WAV 2 (N = 173)"
}

B <- c()

for (i in 1:20){
  B[i] = x1[i]
}

for(i in 21:40){
  B[i] = x1[i-20]
}
for(i in 41:60){
  B[i] = x1[i-40]
}

C<- c()

for (i in 1:20){
  C[i] = y1[i]
}

for(i in 21:40){
  C[i] = y2[i-20]
}
for(i in 41:60){
  C[i] = y3[i-40]
}

data <- data.frame(L = A, x = B, y = C)

Resíduos <-  ggplot(data, aes(x = x, y = C, colour = L))+
  geom_line()+
  scale_colour_manual(values = c("red", "blue", "green")) +
  #stat_smooth(method = lm, formula = y ~ poly(x, 4),se = FALSE, fullrange = TRUE)+
  labs(x = "Resíduos",
       y = "Frequência Relativa (%)",
       colour = " ")+
  theme_classic()

png("32513.png", width = 1000, height = 450)

ggarrange(gráfico, Resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
dev.off()

## Representação 2.1.

### Altitude

#### Registros


gráfico <- ggplot(AL, aes(x = FR, y = R, colour = L1, alpha =I(.5)))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
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

png("31121.png", width = 1000, height = 450)

ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
dev.off()

#### Espécies


gráfico <- ggplot(AL, aes(x = FE, y = E, color = L2, alpha =I(.5)))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
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

png("32121.png", width = 1000, height = 450)

ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
dev.off()

### Área

#### Registros


gráfico <- ggplot(AR, aes(x = FR, y = R, color = L1, alpha =I(.5)))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
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

png("31221.png", width = 1000, height = 450)

ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
dev.off()

#### Espécies


gráfico <- ggplot(AR, aes(x = FE, y = E, color = L2, alpha =I(.5)))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
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

png("32221.png", width = 1000, height = 450)

ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
dev.off()

### População

#### Registros


gráfico <- ggplot(POP, aes(x = FR, y = R, color = L1, alpha =I(.5)))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
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

png("31321.png", width = 1000, height = 450)

ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
dev.off()

#### Espécies


gráfico <- ggplot(POP, aes(x = FE, y = E, color = L2, alpha =I(.5)))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
  stat_smooth(method = "lm",
              se = FALSE,
              fullrange = TRUE,
              size = 1)+
  labs( x = "População (Log10)",
        y = "Número de Espécies (Log10)",
        colour = " ")+
  theme_classic()

REW <- lm(WAVRE~PWAVE, data = dados)
RES <- lm(SPLEP~PSPLE, data = dados)
RE <- lm(WAV2EP~PWAV2E, data = dados)

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

png("32321.png", width = 1000, height = 450)

ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
dev.off()

### Latitude

#### Registros


gráfico <- ggplot(LA, aes(x = FR, y = R, color = L1, alpha =I(.5)))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
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

png("31421.png", width = 1000, height = 450)

ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
dev.off()

#### Espécies


gráfico <- ggplot(LA, aes(x = FE, y = E, color = L2, alpha =I(.5)))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
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

png("32421.png", width = 1000, height = 450)

ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
dev.off()

### Longitude

#### Registros


gráfico <- ggplot(LO, aes(x = FR, y = R, color = L1, alpha =I(.5)))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
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

png("31521.png", width = 1000, height = 450)

ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
dev.off()

#### Espécies


gráfico <- ggplot(LO, aes(x = FE, y = E, color = L2, alpha =I(.5)))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
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

png("32521.png", width = 1000, height = 450)
ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
dev.off()

## Representação 2.2.

### Altitude

#### Registros


gráfico <- ggplot(AL, aes(x = FR, y = R, colour = L1, alpha =I(.5)))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
  stat_smooth(method = "lm",
              se = FALSE,
              fullrange = TRUE,
              size = 1)+
  labs( x ="Altitude (m)",
        y =  "Número de Registros (Log10)",
        colour = " ")+
  theme_classic()

REW <- lm(WAVRAL~ALWAVR, data = dados)
RES <- lm(SPLRAL~ALSPLR, data = dados)
RE <- lm(WAV2RAL~ALWAV2R, data = dados)

a <- qplot(fitted(REW), resid(REW))+ 
  geom_point(col = "blue")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

b <- qplot(fitted(RES), resid(RES))+ 
  geom_point(col = "Red")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

c <- qplot(fitted(RE), resid(RE))+ 
  geom_point(col = "Green")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

resíduos <- ggarrange(a,b,c, ncol = 1, nrow = 3)

png("31122.png", width = 1000, height = 450)

ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
dev.off()

#### Espécies


gráfico <- ggplot(AL, aes(x = FE, y = E, color = L2, alpha =I(.5)))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
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


a <- qplot(fitted(REW), resid(REW))+ 
  geom_point(col = "blue")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

b <- qplot(fitted(RES), resid(RES))+ 
  geom_point(col = "Red")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

c <- qplot(fitted(RE), resid(RE))+ 
  geom_point(col = "Green")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

resíduos <- ggarrange(a,b,c, ncol = 1, nrow = 3)

png("32122.png", width = 1000, height = 450)
ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
dev.off()

### Área

#### Registros



gráfico <- ggplot(AR, aes(x = FR, y = R, color = L1, alpha =I(.5)))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
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

a <- qplot(fitted(REW), resid(REW))+ 
  geom_point(col = "blue")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

b <- qplot(fitted(RES), resid(RES))+ 
  geom_point(col = "Red")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

c <- qplot(fitted(RE), resid(RE))+ 
  geom_point(col = "Green")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

resíduos <- ggarrange(a,b,c, ncol = 1, nrow = 3)

png("31222.png", width = 1000, height = 450)
ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
dev.off()

#### Espécies


gráfico <- ggplot(AR, aes(x = FE, y = E, color = L2, alpha =I(.5)))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
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

a <- qplot(fitted(REW), resid(REW))+ 
  geom_point(col = "blue")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

b <- qplot(fitted(RES), resid(RES))+ 
  geom_point(col = "Red")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

c <- qplot(fitted(RE), resid(RE))+ 
  geom_point(col = "Green")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

resíduos <- ggarrange(a,b,c, ncol = 1, nrow = 3)

png("32222.png", width = 1000, height = 450)
ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
dev.off()

### População

#### Registros


gráfico <- ggplot(POP, aes(x = FR, y = R, color = L1, alpha =I(.5)))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
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

a <- qplot(fitted(REW), resid(REW))+ 
  geom_point(col = "blue")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

b <- qplot(fitted(RES), resid(RES))+ 
  geom_point(col = "Red")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

c <- qplot(fitted(RE), resid(RE))+ 
  geom_point(col = "Green")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

resíduos <- ggarrange(a,b,c, ncol = 1, nrow = 3)

png("31322.png", width = 1000, height = 450)
ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
dev.off()

#### Espécies


gráfico <- ggplot(POP, aes(x = FE, y = E, color = L2, alpha =I(.5)))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
  stat_smooth(method = "lm",
              se = FALSE,
              fullrange = TRUE,
              size = 1)+
  labs( x = "População (Log10)",
        y = "Número de Espécies (Log10)",
        colour = " ")+
  theme_classic()

REW <- lm(WAVRE~PWAVE, data = dados)
RES <- lm(SPLEP~PSPLE, data = dados)
RE <- lm(WAV2EP~PWAV2E, data = dados)

a <- qplot(fitted(REW), resid(REW))+ 
  geom_point(col = "blue")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

b <- qplot(fitted(RES), resid(RES))+ 
  geom_point(col = "Red")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

c <- qplot(fitted(RE), resid(RE))+ 
  geom_point(col = "Green")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

resíduos <- ggarrange(a,b,c, ncol = 1, nrow = 3)
png("32322.png", width = 1000, height = 450)
ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
dev.off()

### Latitude

#### Registros


gráfico <- ggplot(LA, aes(x = FR, y = R, color = L1, alpha =I(.5)))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
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

a <- qplot(fitted(REW), resid(REW))+ 
  geom_point(col = "blue")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

b <- qplot(fitted(RES), resid(RES))+ 
  geom_point(col = "Red")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

c <- qplot(fitted(RE), resid(RE))+ 
  geom_point(col = "Green")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

resíduos <- ggarrange(a,b,c, ncol = 1, nrow = 3)

png("31422.png", width = 1000, height = 450)
ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
dev.off()

#### Espécies


gráfico <- ggplot(LA, aes(x = FE, y = E, color = L2, alpha =I(.5)))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
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

a <- qplot(fitted(REW), resid(REW))+ 
  geom_point(col = "blue")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

b <- qplot(fitted(RES), resid(RES))+ 
  geom_point(col = "Red")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

c <- qplot(fitted(RE), resid(RE))+ 
  geom_point(col = "Green")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

resíduos <- ggarrange(a,b,c, ncol = 1, nrow = 3)

png("32422.png", width = 1000, height = 450)
ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
dev.off()

### Longitude

#### Registros


gráfico <- ggplot(LO, aes(x = FR, y = R, color = L1, alpha =I(.5)))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
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

a <- qplot(fitted(REW), resid(REW))+ 
  geom_point(col = "blue")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

b <- qplot(fitted(RES), resid(RES))+ 
  geom_point(col = "Red")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

c <- qplot(fitted(RE), resid(RE))+ 
  geom_point(col = "Green")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

resíduos <- ggarrange(a,b,c, ncol = 1, nrow = 3)

png("31522.png", width = 1000, height = 450)
ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
dev.off()

#### Espécies


gráfico <- ggplot(LO, aes(x = FE, y = E, color = L2, alpha =I(.5)))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
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

a <- qplot(fitted(REW), resid(REW))+ 
  geom_point(col = "blue")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

b <- qplot(fitted(RES), resid(RES))+ 
  geom_point(col = "Red")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

c <- qplot(fitted(RE), resid(RE))+ 
  geom_point(col = "Green")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

resíduos <- ggarrange(a,b,c, ncol = 1, nrow = 3)

png("32522.png", width = 1000, height = 450)
ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
dev.off()

## Representação 2.3.

### Altitude

#### Registros


gráfico <- ggplot(AL, aes(x = FR, y = R, colour = L1, alpha =I(.5)))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
  stat_smooth(method = "lm",
              se = FALSE,
              fullrange = TRUE,
              size = 1)+
  labs( x ="Altitude (m)",
        y =  "Número de Registros (Log10)",
        colour = " ")+
  theme_classic()

REW <- lm(WAVRAL~ALWAVR, data = dados)
RES <- lm(SPLRAL~ALSPLR, data = dados)
RE <- lm(WAV2RAL~ALWAV2R, data = dados)


R1 <- resid (REW)
R2 <- resid (RES)
R3 <- resid (RE)

min(R1,R2,R3)
max(R1,R2,R3)

AX <- hist(R1, 
           plot = FALSE, 
           breaks = c(-2.1,-1.875,-1.65,-1.425,-1.2,-0.925,-0.75,-0.525,-0.3,-0.075,0.15,0.375,0.6,0.825,1.05,1.275,1.5,1.725,1.95,2.175,2.4))
AX$density = AX$counts/sum(AX$counts)*100

BX <- hist(R2, 
           plot = FALSE, 
           breaks = c(-2.1,-1.875,-1.65,-1.425,-1.2,-0.925,-0.75,-0.525,-0.3,-0.075,0.15,0.375,0.6,0.825,1.05,1.275,1.5,1.725,1.95,2.175,2.4))
BX$density = BX$counts/sum(BX$counts)*100

CX <- hist(R3, 
           plot = FALSE, 
           breaks = c(-2.1,-1.875,-1.65,-1.425,-1.2,-0.925,-0.75,-0.525,-0.3,-0.075,0.15,0.375,0.6,0.825,1.05,1.275,1.5,1.725,1.95,2.175,2.4))
CX$density = CX$counts/sum(CX$counts)*100

y1 <- AX$density
y2 <- BX$density
y3 <- CX$density

x1 <- c()
x1[1] <- -2.1
for (i in 1:19){
  x1[i+1] = x1[i]+0.225
}


A <- c()

for (i in 1:20){
  A[i] = "WAV (N = 631)"
}
for (i in 21:40){
  A[i] = "SPL (N = 174)"
}
for (i in 41:60){
  A[i] = "WAV 2 (N = 173)"
}

B <- c()

for (i in 1:20){
  B[i] = x1[i]
}

for(i in 21:40){
  B[i] = x1[i-20]
}
for(i in 41:60){
  B[i] = x1[i-40]
}

C<- c()

for (i in 1:20){
  C[i] = y1[i]
}

for(i in 21:40){
  C[i] = y2[i-20]
}
for(i in 41:60){
  C[i] = y3[i-40]
}

data <- data.frame(L = A, x = B, y = C)

Resíduos <-  ggplot(data, aes(x = x, y = C, colour = L))+
  geom_line()+
  scale_colour_manual(values = c("red", "blue", "green")) +
  #stat_smooth(method = lm, formula = y ~ poly(x, 4),se = FALSE, fullrange = TRUE)+
  labs(x = "Resíduos",
       y = "Frequência Relativa (%)",
       colour = " ")+
  theme_classic()

png("31123.png", width = 1000, height = 450)
ggarrange(gráfico, Resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
dev.off()

#### Espécies



gráfico <- ggplot(AL, aes(x = FE, y = E, color = L2, alpha =I(.5)))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
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


R1 <- resid (REW)
R2 <- resid (RES)
R3 <- resid (RE)

min(R1,R2,R3)
max(R1,R2,R3)

AX <- hist(R1, 
           plot = FALSE, 
           breaks = c(-1.3,-1.1,-0.9,-0.7,-0.5,-0.3,-0.1,0.1,0.3,0.5,0.7,0.9,1.1,1.3,1.5,1.7,1.9,2.1,2.3,2.5,2.7))
AX$density = AX$counts/sum(AX$counts)*100

BX <- hist(R2, 
           plot = FALSE, 
           breaks = c(-1.3,-1.1,-0.9,-0.7,-0.5,-0.3,-0.1,0.1,0.3,0.5,0.7,0.9,1.1,1.3,1.5,1.7,1.9,2.1,2.3,2.5,2.7))
BX$density = BX$counts/sum(BX$counts)*100

CX <- hist(R3, 
           plot = FALSE, 
           breaks = c(-1.3,-1.1,-0.9,-0.7,-0.5,-0.3,-0.1,0.1,0.3,0.5,0.7,0.9,1.1,1.3,1.5,1.7,1.9,2.1,2.3,2.5,2.7))
CX$density = CX$counts/sum(CX$counts)*100

y1 <- AX$density
y2 <- BX$density
y3 <- CX$density

x1 <- c()
x1[1] <- -1.3
for (i in 1:19){
  x1[i+1] = x1[i]+0.2
}


A <- c()

for (i in 1:20){
  A[i] = "WAV (N = 631)"
}
for (i in 21:40){
  A[i] = "SPL (N = 174)"
}
for (i in 41:60){
  A[i] = "WAV 2 (N = 173)"
}

B <- c()

for (i in 1:20){
  B[i] = x1[i]
}

for(i in 21:40){
  B[i] = x1[i-20]
}
for(i in 41:60){
  B[i] = x1[i-40]
}

C<- c()

for (i in 1:20){
  C[i] = y1[i]
}

for(i in 21:40){
  C[i] = y2[i-20]
}
for(i in 41:60){
  C[i] = y3[i-40]
}

data <- data.frame(L = A, x = B, y = C)

Resíduos <-  ggplot(data, aes(x = x, y = C, colour = L))+
  geom_line()+
  scale_colour_manual(values = c("red", "blue", "green")) +
  #stat_smooth(method = lm, formula = y ~ poly(x, 4),se = FALSE, fullrange = TRUE)+
  labs(x = "Resíduos",
       y = "Frequência Relativa (%)",
       colour = " ")+
  theme_classic()

png("32123.png", width = 1000, height = 450)
ggarrange(gráfico, Resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
dev.off()

### Área

#### Registros



gráfico <- ggplot(AR, aes(x = FR, y = R, color = L1, alpha =I(.5)))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
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

R1 <- resid (REW)
R2 <- resid (RES)
R3 <- resid (RE)

min(R1,R2,R3)
max(R1,R2,R3)

AX <- hist(R1, 
           plot = FALSE, 
           breaks = c(-2.4,-2.165,-1.93,-1.695,-1.46,-1.225,-0.99,-0.755,-0.52,-0.285,-0.05,0.185,0.42,0.655,0.89,1.125,1.36,1.595,1.83,2.065,2.3))
AX$density = AX$counts/sum(AX$counts)*100

BX <- hist(R2, 
           plot = FALSE, 
           breaks = c(-2.4,-2.165,-1.93,-1.695,-1.46,-1.225,-0.99,-0.755,-0.52,-0.285,-0.05,0.185,0.42,0.655,0.89,1.125,1.36,1.595,1.83,2.065,2.3))
BX$density = BX$counts/sum(BX$counts)*100

CX <- hist(R3, 
           plot = FALSE, 
           breaks = c(-2.4,-2.165,-1.93,-1.695,-1.46,-1.225,-0.99,-0.755,-0.52,-0.285,-0.05,0.185,0.42,0.655,0.89,1.125,1.36,1.595,1.83,2.065,2.3))
CX$density = CX$counts/sum(CX$counts)*100

y1 <- AX$density
y2 <- BX$density
y3 <- CX$density

x1 <- c()
x1[1] <- -2.4
for (i in 1:19){
  x1[i+1] = x1[i]+0.235
}


A <- c()

for (i in 1:20){
  A[i] = "WAV (N = 631)"
}
for (i in 21:40){
  A[i] = "SPL (N = 174)"
}
for (i in 41:60){
  A[i] = "WAV 2 (N = 173)"
}

B <- c()

for (i in 1:20){
  B[i] = x1[i]
}

for(i in 21:40){
  B[i] = x1[i-20]
}
for(i in 41:60){
  B[i] = x1[i-40]
}

C<- c()

for (i in 1:20){
  C[i] = y1[i]
}

for(i in 21:40){
  C[i] = y2[i-20]
}
for(i in 41:60){
  C[i] = y3[i-40]
}

data <- data.frame(L = A, x = B, y = C)

Resíduos <-  ggplot(data, aes(x = x, y = C, colour = L))+
  geom_line()+
  scale_colour_manual(values = c("red", "blue", "green")) +
  #stat_smooth(method = lm, formula = y ~ poly(x, 4),se = FALSE, fullrange = TRUE)+
  labs(x = "Resíduos",
       y = "Frequência Relativa (%)",
       colour = " ")+
  theme_classic()

png("31223.png", width = 1000, height = 450)
ggarrange(gráfico, Resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
dev.off()


#### Espécies


gráfico <- ggplot(AR, aes(x = FE, y = E, color = L2, alpha =I(.5)))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
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

R1 <- resid (REW)
R2 <- resid (RES)
R3 <- resid (RE)

min(R1,R2,R3)
max(R1,R2,R3)

AX <- hist(R1, 
           plot = FALSE, 
           breaks = c(-1.4,-1.26,-1.12,-0.98,-0.84,-0.7,-0.56,-0.42,-0.28,-0.14,0,0.14,0.28,0.42,0.56,0.7,0.84,0.98,1.12,1.26,1.4))
AX$density = AX$counts/sum(AX$counts)*100

BX <- hist(R2, 
           plot = FALSE, 
           breaks = c(-1.4,-1.26,-1.12,-0.98,-0.84,-0.7,-0.56,-0.42,-0.28,-0.14,0,0.14,0.28,0.42,0.56,0.7,0.84,0.98,1.12,1.26,1.4))
BX$density = BX$counts/sum(BX$counts)*100

CX <- hist(R3, 
           plot = FALSE, 
           breaks = c(-1.4,-1.26,-1.12,-0.98,-0.84,-0.7,-0.56,-0.42,-0.28,-0.14,0,0.14,0.28,0.42,0.56,0.7,0.84,0.98,1.12,1.26,1.4))
CX$density = CX$counts/sum(CX$counts)*100

y1 <- AX$density
y2 <- BX$density
y3 <- CX$density

x1 <- c()
x1[1] <- -1.4
for (i in 1:19){
  x1[i+1] = x1[i]+0.14
}


A <- c()

for (i in 1:20){
  A[i] = "WAV (N = 631)"
}
for (i in 21:40){
  A[i] = "SPL (N = 174)"
}
for (i in 41:60){
  A[i] = "WAV 2 (N = 173)"
}

B <- c()

for (i in 1:20){
  B[i] = x1[i]
}

for(i in 21:40){
  B[i] = x1[i-20]
}
for(i in 41:60){
  B[i] = x1[i-40]
}

C<- c()

for (i in 1:20){
  C[i] = y1[i]
}

for(i in 21:40){
  C[i] = y2[i-20]
}
for(i in 41:60){
  C[i] = y3[i-40]
}

data <- data.frame(L = A, x = B, y = C)

Resíduos <-  ggplot(data, aes(x = x, y = C, colour = L))+
  geom_line()+
  scale_colour_manual(values = c("red", "blue", "green")) +
  #stat_smooth(method = lm, formula = y ~ poly(x, 4),se = FALSE, fullrange = TRUE)+
  labs(x = "Resíduos",
       y = "Frequência Relativa (%)",
       colour = " ")+
  theme_classic()

png("32223.png", width = 1000, height = 450)
ggarrange(gráfico, Resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
dev.off()

### População

#### Registros


gráfico <- ggplot(POP, aes(x = FR, y = R, color = L1, alpha =I(.5)))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
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

R1 <- resid (REW)
R2 <- resid (RES)
R3 <- resid (RE)

min(R1,R2,R3)
max(R1,R2,R3)

AX <- hist(R1, 
           plot = FALSE, 
           breaks = c(-1.7,-1.495,-1.29,-1.085,-0.88,-0.675,-0.47,-0.265,-0.06,0.145,0.35,0.555,0.76,0.965,1.17,1.375,1.58,1.785,1.99,2.195,2.4))
AX$density = AX$counts/sum(AX$counts)*100

BX <- hist(R2, 
           plot = FALSE, 
           breaks = c(-1.7,-1.495,-1.29,-1.085,-0.88,-0.675,-0.47,-0.265,-0.06,0.145,0.35,0.555,0.76,0.965,1.17,1.375,1.58,1.785,1.99,2.195,2.4))
BX$density = BX$counts/sum(BX$counts)*100

CX <- hist(R3, 
           plot = FALSE, 
           breaks = c(-1.7,-1.495,-1.29,-1.085,-0.88,-0.675,-0.47,-0.265,-0.06,0.145,0.35,0.555,0.76,0.965,1.17,1.375,1.58,1.785,1.99,2.195,2.4))
CX$density = CX$counts/sum(CX$counts)*100

y1 <- AX$density
y2 <- BX$density
y3 <- CX$density

x1 <- c()
x1[1] <- -1.7
for (i in 1:19){
  x1[i+1] = x1[i]+0.205
}


A <- c()

for (i in 1:20){
  A[i] = "WAV (N = 631)"
}
for (i in 21:40){
  A[i] = "SPL (N = 174)"
}
for (i in 41:60){
  A[i] = "WAV 2 (N = 173)"
}

B <- c()

for (i in 1:20){
  B[i] = x1[i]
}

for(i in 21:40){
  B[i] = x1[i-20]
}
for(i in 41:60){
  B[i] = x1[i-40]
}

C<- c()

for (i in 1:20){
  C[i] = y1[i]
}

for(i in 21:40){
  C[i] = y2[i-20]
}
for(i in 41:60){
  C[i] = y3[i-40]
}

data <- data.frame(L = A, x = B, y = C)

Resíduos <-  ggplot(data, aes(x = x, y = C, colour = L))+
  geom_line()+
  scale_colour_manual(values = c("red", "blue", "green")) +
  #stat_smooth(method = lm, formula = y ~ poly(x, 4),se = FALSE, fullrange = TRUE)+
  labs(x = "Resíduos",
       y = "Frequência Relativa (%)",
       colour = " ")+
  theme_classic()

png("31323.png", width = 1000, height = 450)
ggarrange(gráfico, Resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
dev.off()

#### Espécies


gráfico <- ggplot(POP, aes(x = FE, y = E, color = L2, alpha =I(.5)))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
  stat_smooth(method = "lm",
              se = FALSE,
              fullrange = TRUE,
              size = 1)+
  labs( x = "População (Log10)",
        y = "Número de Espécies (Log10)",
        color = " ")+
  theme_classic()

REW <- lm(WAVRE~PWAVE, data = dados)
RES <- lm(SPLEP~PSPLE, data = dados)
RE <- lm(WAV2EP~PWAV2E, data = dados)

R1 <- resid (REW)
R2 <- resid (RES)
R3 <- resid (RE)

min(R1,R2,R3)
max(R1,R2,R3)

AX <- hist(R1, 
           plot = FALSE, 
           breaks = c(-1.2,-1.06,-0.92,-0.78,-0.74,-0.5,-0.36,-0.22,-0.08,0.06,0.2,0.34,0.48,0.62,0.76,0.9,1.04,1.18,1.32,1.46,1.6))
AX$density = AX$counts/sum(AX$counts)*100

BX <- hist(R2, 
           plot = FALSE, 
           breaks = c(-1.2,-1.06,-0.92,-0.78,-0.74,-0.5,-0.36,-0.22,-0.08,0.06,0.2,0.34,0.48,0.62,0.76,0.9,1.04,1.18,1.32,1.46,1.6))
BX$density = BX$counts/sum(BX$counts)*100

CX <- hist(R3, 
           plot = FALSE, 
           breaks = c(-1.2,-1.06,-0.92,-0.78,-0.74,-0.5,-0.36,-0.22,-0.08,0.06,0.2,0.34,0.48,0.62,0.76,0.9,1.04,1.18,1.32,1.46,1.6))
CX$density = CX$counts/sum(CX$counts)*100

y1 <- AX$density
y2 <- BX$density
y3 <- CX$density

x1 <- c()
x1[1] <- -1.2
for (i in 1:19){
  x1[i+1] = x1[i]+0.14
}


A <- c()

for (i in 1:20){
  A[i] = "WAV (N = 631)"
}
for (i in 21:40){
  A[i] = "SPL (N = 174)"
}
for (i in 41:60){
  A[i] = "WAV 2 (N = 173)"
}

B <- c()

for (i in 1:20){
  B[i] = x1[i]
}

for(i in 21:40){
  B[i] = x1[i-20]
}
for(i in 41:60){
  B[i] = x1[i-40]
}

C<- c()

for (i in 1:20){
  C[i] = y1[i]
}

for(i in 21:40){
  C[i] = y2[i-20]
}
for(i in 41:60){
  C[i] = y3[i-40]
}

data <- data.frame(L = A, x = B, y = C)

Resíduos <-  ggplot(data, aes(x = x, y = C, colour = L))+
  geom_line()+
  scale_colour_manual(values = c("red", "blue", "green")) +
  #stat_smooth(method = lm, formula = y ~ poly(x, 4),se = FALSE, fullrange = TRUE)+
  labs(x = "Resíduos",
       y = "Frequência Relativa (%)",
       colour = " ")+
  theme_classic()

png("32323.png", width = 1000, height = 450)
ggarrange(gráfico, Resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
dev.off()

### Latitude

#### Registros


gráfico <- ggplot(LA, aes(x = FR, y = R, color = L1, alpha =I(.5)))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
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

R1 <- resid (REW)
R2 <- resid (RES)
R3 <- resid (RE)

min(R1,R2,R3)
max(R1,R2,R3)

AX <- hist(R1, 
           plot = FALSE, 
           breaks = c(-2.2,-1.965,-1.73,-1.495,-1.26,-1.025,-0.79,-0.555,-0.32,-0.085,0.15,0.385,0.62,0.855,1.09,1.325,1.56,1.795,2.03,2.265,2.5))
AX$density = AX$counts/sum(AX$counts)*100

BX <- hist(R2, 
           plot = FALSE, 
           breaks = c(-2.2,-1.965,-1.73,-1.495,-1.26,-1.025,-0.79,-0.555,-0.32,-0.085,0.15,0.385,0.62,0.855,1.09,1.325,1.56,1.795,2.03,2.265,2.5))
BX$density = BX$counts/sum(BX$counts)*100

CX <- hist(R3, 
           plot = FALSE, 
           breaks = c(-2.2,-1.965,-1.73,-1.495,-1.26,-1.025,-0.79,-0.555,-0.32,-0.085,0.15,0.385,0.62,0.855,1.09,1.325,1.56,1.795,2.03,2.265,2.5))
CX$density = CX$counts/sum(CX$counts)*100

y1 <- AX$density
y2 <- BX$density
y3 <- CX$density

x1 <- c()
x1[1] <- -2.2
for (i in 1:19){
  x1[i+1] = x1[i]+0.235
}


A <- c()

for (i in 1:20){
  A[i] = "WAV (N = 631)"
}
for (i in 21:40){
  A[i] = "SPL (N = 174)"
}
for (i in 41:60){
  A[i] = "WAV 2 (N = 173)"
}

B <- c()

for (i in 1:20){
  B[i] = x1[i]
}

for(i in 21:40){
  B[i] = x1[i-20]
}
for(i in 41:60){
  B[i] = x1[i-40]
}

C<- c()

for (i in 1:20){
  C[i] = y1[i]
}

for(i in 21:40){
  C[i] = y2[i-20]
}
for(i in 41:60){
  C[i] = y3[i-40]
}

data <- data.frame(L = A, x = B, y = C)

Resíduos <-  ggplot(data, aes(x = x, y = C, colour = L))+
  geom_line()+
  scale_colour_manual(values = c("red", "blue", "green")) +
  #stat_smooth(method = lm, formula = y ~ poly(x, 4),se = FALSE, fullrange = TRUE)+
  labs(x = "Resíduos",
       y = "Frequência Relativa (%)",
       colour = " ")+
  theme_classic()

png("31423.png", width = 1000, height = 450)
ggarrange(gráfico, Resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
dev.off()

#### Espécies


gráfico <- ggplot(LA, aes(x = FE, y = E, color = L2, alpha =I(.5)))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
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

R1 <- resid (REW)
R2 <- resid (RES)
R3 <- resid (RE)

min(R1,R2,R3)
max(R1,R2,R3)

AX <- hist(R1, 
           plot = FALSE, 
           breaks = c(-1.4,-1.245,-1.09,-0.935,-0.78,-0.625,-0.47,-0.315,-0.16,-0.005,0.15,0.305,0.46,0.615,0.77,0.925,1.08,1.235,1.39,1.545,1.7))
AX$density = AX$counts/sum(AX$counts)*100

BX <- hist(R2, 
           plot = FALSE, 
           breaks = c(-1.4,-1.245,-1.09,-0.935,-0.78,-0.625,-0.47,-0.315,-0.16,-0.005,0.15,0.305,0.46,0.615,0.77,0.925,1.08,1.235,1.39,1.545,1.7))
BX$density = BX$counts/sum(BX$counts)*100

CX <- hist(R3, 
           plot = FALSE, 
           breaks = c(-1.4,-1.245,-1.09,-0.935,-0.78,-0.625,-0.47,-0.315,-0.16,-0.005,0.15,0.305,0.46,0.615,0.77,0.925,1.08,1.235,1.39,1.545,1.7))
CX$density = CX$counts/sum(CX$counts)*100

y1 <- AX$density
y2 <- BX$density
y3 <- CX$density

x1 <- c()
x1[1] <- -1.4
for (i in 1:19){
  x1[i+1] = x1[i]+0.155
}


A <- c()

for (i in 1:20){
  A[i] = "WAV (N = 631)"
}
for (i in 21:40){
  A[i] = "SPL (N = 174)"
}
for (i in 41:60){
  A[i] = "WAV 2 (N = 173)"
}

B <- c()

for (i in 1:20){
  B[i] = x1[i]
}

for(i in 21:40){
  B[i] = x1[i-20]
}
for(i in 41:60){
  B[i] = x1[i-40]
}

C<- c()

for (i in 1:20){
  C[i] = y1[i]
}

for(i in 21:40){
  C[i] = y2[i-20]
}
for(i in 41:60){
  C[i] = y3[i-40]
}

data <- data.frame(L = A, x = B, y = C)

Resíduos <-  ggplot(data, aes(x = x, y = C, colour = L))+
  geom_line()+
  scale_colour_manual(values = c("red", "blue", "green")) +
  #stat_smooth(method = lm, formula = y ~ poly(x, 4),se = FALSE, fullrange = TRUE)+
  labs(x = "Resíduos",
       y = "Frequência Relativa (%)",
       colour = " ")+
  theme_classic()

png("32423.png", width = 1000, height = 450)
ggarrange(gráfico, Resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
dev.off()

### Longitude

#### Registros


gráfico <- ggplot(LO, aes(x = FR, y = R, color = L1, alpha =I(.5)))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
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

R1 <- resid (REW)
R2 <- resid (RES)
R3 <- resid (RE)

min(R1,R2,R3)
max(R1,R2,R3)

AX <- hist(R1, 
           plot = FALSE, 
           breaks = c(-1.8,-1.59,-1.38,-1.17,-0.96,-0.75,-0.54,-0.33,-0.12,0.09,0.3,0.51,0.72,0.93,1.14,1.35,1.56,1.77,1.98,2.19,2.4))
AX$density = AX$counts/sum(AX$counts)*100

BX <- hist(R2, 
           plot = FALSE, 
           breaks = c(-1.8,-1.59,-1.38,-1.17,-0.96,-0.75,-0.54,-0.33,-0.12,0.09,0.3,0.51,0.72,0.93,1.14,1.35,1.56,1.77,1.98,2.19,2.4))
BX$density = BX$counts/sum(BX$counts)*100

CX <- hist(R3, 
           plot = FALSE, 
           breaks = c(-1.8,-1.59,-1.38,-1.17,-0.96,-0.75,-0.54,-0.33,-0.12,0.09,0.3,0.51,0.72,0.93,1.14,1.35,1.56,1.77,1.98,2.19,2.4))
CX$density = CX$counts/sum(CX$counts)*100

y1 <- AX$density
y2 <- BX$density
y3 <- CX$density

x1 <- c()
x1[1] <- -1.8
for (i in 1:19){
  x1[i+1] = x1[i]+0.21
}


A <- c()

for (i in 1:20){
  A[i] = "WAV (N = 631)"
}
for (i in 21:40){
  A[i] = "SPL (N = 174)"
}
for (i in 41:60){
  A[i] = "WAV 2 (N = 173)"
}

B <- c()

for (i in 1:20){
  B[i] = x1[i]
}

for(i in 21:40){
  B[i] = x1[i-20]
}
for(i in 41:60){
  B[i] = x1[i-40]
}

C<- c()

for (i in 1:20){
  C[i] = y1[i]
}

for(i in 21:40){
  C[i] = y2[i-20]
}
for(i in 41:60){
  C[i] = y3[i-40]
}

data <- data.frame(L = A, x = B, y = C)

Resíduos <-  ggplot(data, aes(x = x, y = C, colour = L))+
  geom_line()+
  scale_colour_manual(values = c("red", "blue", "green")) +
  #stat_smooth(method = lm, formula = y ~ poly(x, 4),se = FALSE, fullrange = TRUE)+
  labs(x = "Resíduos",
       y = "Frequência Relativa (%)",
       colour = " ")+
  theme_classic()

png("31523.png", width = 1000, height = 450)
ggarrange(gráfico, Resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
dev.off()

#### Espécies


gráfico <- ggplot(LO, aes(x = FE, y = E, color = L2, alpha =I(.5)))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
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

R1 <- resid (REW)
R2 <- resid (RES)
R3 <- resid (RE)

min(R1,R2,R3)
max(R1,R2,R3)

AX <- hist(R1, 
           plot = FALSE, 
           breaks = c(-1.2,-1.065,-0.93,-0.795,-0.66,-0.525,-0.39,-0.255,-0.12,0.015,0.15,0.285,0.42,0.555,0.69,0.825,0.96,1.095,1.23,1.365,1.5))
AX$density = AX$counts/sum(AX$counts)*100

BX <- hist(R2, 
           plot = FALSE, 
           breaks = c(-1.2,-1.065,-0.93,-0.795,-0.66,-0.525,-0.39,-0.255,-0.12,0.015,0.15,0.285,0.42,0.555,0.69,0.825,0.96,1.095,1.23,1.365,1.5))
BX$density = BX$counts/sum(BX$counts)*100

CX <- hist(R3, 
           plot = FALSE, 
           breaks = c(-1.2,-1.065,-0.93,-0.795,-0.66,-0.525,-0.39,-0.255,-0.12,0.015,0.15,0.285,0.42,0.555,0.69,0.825,0.96,1.095,1.23,1.365,1.5))
CX$density = CX$counts/sum(CX$counts)*100

y1 <- AX$density
y2 <- BX$density
y3 <- CX$density

x1 <- c()
x1[1] <- -1.2
for (i in 1:19){
  x1[i+1] = x1[i]+0.135
}


A <- c()

for (i in 1:20){
  A[i] = "WAV (N = 631)"
}
for (i in 21:40){
  A[i] = "SPL (N = 174)"
}
for (i in 41:60){
  A[i] = "WAV 2 (N = 173)"
}

B <- c()

for (i in 1:20){
  B[i] = x1[i]
}

for(i in 21:40){
  B[i] = x1[i-20]
}
for(i in 41:60){
  B[i] = x1[i-40]
}

C<- c()

for (i in 1:20){
  C[i] = y1[i]
}

for(i in 21:40){
  C[i] = y2[i-20]
}
for(i in 41:60){
  C[i] = y3[i-40]
}

data <- data.frame(L = A, x = B, y = C)

Resíduos <-  ggplot(data, aes(x = x, y = C, colour = L))+
  geom_line()+
  scale_colour_manual(values = c("red", "blue", "green")) +
  #stat_smooth(method = lm, formula = y ~ poly(x, 4),se = FALSE, fullrange = TRUE)+
  labs(x = "Resíduos",
       y = "Frequência Relativa (%)",
       colour = " ")+
  theme_classic()

png("32523.png", width = 1000, height = 450)
ggarrange(gráfico, Resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
dev.off()

## Representação 3.1.

### Altitude

#### Registros


gráfico <- ggplot(AL, aes(x = FR, y = R, colour = L1, shape = L1))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green"))+
  scale_shape_manual(values=c(15, 16, 17))+
  stat_smooth(method = "lm",
              se = FALSE,
              fullrange = TRUE,
              size = 1)+
  labs( x ="Altitude (m)",
        y =  "Número de Registros (Log10)",
        colour = " ",
        shape = " ")+
  #guides(colour=FALSE)+
  theme_classic()

REW <- lm(WAVRAL~ALWAVR, data = dados)
RES <- lm(SPLRAL~ALSPLR, data = dados)
RE <- lm(WAV2RAL~ALWAV2R, data = dados)

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

png("31131.png", width = 1000, height = 450)
ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
dev.off()

#### Espécies


gráfico <- ggplot(AL, aes(x = FE, y = E, color = L2, shape = L2))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
  scale_shape_manual(values=c(15, 16, 17))+
  stat_smooth(method = "lm",
              se = FALSE,
              fullrange = TRUE,
              size = 1)+
  labs( x = "Altitude (m)",
        y = "Número de Espécies (Log10)",
        colour = " ",
        shape = " ")+
  theme_classic()

REW <- lm(WAVEAL~ALWAVE, data = dados)
RES <- lm(SPLEAL~ALSPLE, data = dados)
RE <- lm(WAV2EAL~ALWAV2E, data = dados)

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

png("32131.png", width = 1000, height = 450)
ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
dev.off()

### Área

#### Registros


gráfico <- ggplot(AR, aes(x = FR, y = R, color = L1, shape = L1))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
  scale_shape_manual(values=c(15, 16, 17))+
  stat_smooth(method = "lm",
              se = FALSE,
              fullrange = TRUE,
              size = 1)+
  labs( x ="Área (Log10(Km^2))",
        y =  "Número de Registros (Log10)",
        colour = " ",
        shape = " ")+
  theme_classic()

REW <- lm(WAVRAR~ARWAVR, data = dados)
RES <- lm(SPLRAR~ARSPLR, data = dados)
RE <- lm(WAV2RAR~ARWAV2R, data = dados)

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

png("31231.png", width = 1000, height = 450)
ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
dev.off()

#### Espécies


gráfico <- ggplot(AR, aes(x = FE, y = E, color = L2, shape = L2))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
  scale_shape_manual(values=c(15, 16, 17))+
  stat_smooth(method = "lm",
              se = FALSE,
              fullrange = TRUE,
              size = 1)+
  labs( x = "Área (Log10(Km^2))",
        y = "Número de Espécies (Log10)",
        colour = " ",
        shape = " ")+
  theme_classic()

REW <- lm(WAVEAR~ARWAVE, data = dados)
RES <- lm(SPLEAR~ARSPLE, data = dados)
RE <- lm(WAV2EAR~ARWAV2E, data = dados)

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

png("32231.png", width = 1000, height = 450)
ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
dev.off()

### População

#### Registros


gráfico <- ggplot(POP, aes(x = FR, y = R, color = L1, shape = L1))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
  scale_shape_manual(values=c(15, 16, 17))+
  stat_smooth(method = "lm",
              se = FALSE,
              fullrange = TRUE,
              size = 1)+
  labs( x ="População (Log10)",
        y =  "Número de Registros (Log10)",
        colour = " ",
        shape = " ")+
  theme_classic()

REW <- lm(WAVRP~PWAVR, data = dados)
RES <- lm(SPLRP~PSPLR, data = dados)
RE <- lm(WAV2RP~PWAV2R, data = dados)

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

png("31331.png", width = 1000, height = 450)
ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
dev.off()

#### Espécies


gráfico <- ggplot(POP, aes(x = FE, y = E, color = L2, shape = L2))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
  scale_shape_manual(values=c(15, 16, 17))+
  stat_smooth(method = "lm",
              se = FALSE,
              fullrange = TRUE,
              size = 1)+
  labs( x = "População (Log10)",
        y = "Número de Espécies (Log10)",
        color = " ",
        shape = " ")+
  theme_classic()

REW <- lm(WAVRE~PWAVE, data = dados)
RES <- lm(SPLEP~PSPLE, data = dados)
RE <- lm(WAV2EP~PWAV2E, data = dados)

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

png("32331.png", width = 1000, height = 450)
ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
dev.off()

### Latitude

#### Registros


gráfico <- ggplot(LA, aes(x = FR, y = R, color = L1, shape = L1))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
  scale_shape_manual(values=c(15, 16, 17))+
  stat_smooth(method = "lm",
              se = FALSE,
              fullrange = TRUE,
              size = 1)+
  labs( x ="Latitude (º)",
        y =  "Número de Registros (Log10)",
        colour = " ",
        shape = " ")+
  theme_classic()

REW <- lm(WAVRLA~LAWAVR, data = dados)
RES <- lm(SPLRLA~LASPLR, data = dados)
RE <- lm(WAV2RLA~LAWAV2R, data = dados)

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

png("31431.png", width = 1000, height = 450)
ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
dev.off()

#### Espécies


gráfico <- ggplot(LA, aes(x = FE, y = E, color = L2, shape = L2))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
  scale_shape_manual(values=c(15, 16, 17))+
  stat_smooth(method = "lm",
              se = FALSE,
              fullrange = TRUE,
              size = 1)+
  labs( x ="Latitude (º)",
        y = "Número de Espécies (Log10)",
        colour = " ",
        shape = " ")+
  theme_classic()

REW <- lm(WAVELA~LAWAVE, data = dados)
RES <- lm(SPLELA~LASPLE, data = dados)
RE <- lm(WAV2ELA~LAWAV2E, data = dados)

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

png("32431.png", width = 1000, height = 450)
ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
dev.off()

### Longitude

#### Registros


gráfico <- ggplot(LO, aes(x = FR, y = R, color = L1, shape = L1))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
  scale_shape_manual(values=c(15, 16, 17))+
  stat_smooth(method = "lm",
              se = FALSE,
              fullrange = TRUE,
              size = 1)+
  labs( x ="Longitude (º)",
        y =  "Número de Registros (Log10)",
        colour = " ",
        shape = " ")+
  theme_classic()

REW <- lm(WAVRLO~LOWAVR, data = dados)
RES <- lm(SPLRLO~LOSPLR, data = dados)
RE <- lm(WAV2RLO~LOWAV2R, data = dados)

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

png("31531.png", width = 1000, height = 450)
ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
dev.off()

#### Espécies


gráfico <- ggplot(LO, aes(x = FE, y = E, color = L2, shape = L2))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
  scale_shape_manual(values=c(15, 16, 17))+
  stat_smooth(method = "lm",
              se = FALSE,
              fullrange = TRUE,
              size = 1)+
  labs( x ="Longitude (º)",
        y = "Número de Espécies (Log10)",
        colour = " ",
        shape = " ")+
  theme_classic()

REW <- lm(WAVELO~LOWAVE, data = dados)
RES <- lm(SPLELO~LOSPLE, data = dados)
RE <- lm(WAV2ELO~LOWAV2E, data = dados)

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

png("32531.png", width = 1000, height = 450)
ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
dev.off()

## Representação 3.2.

### Altitude

#### Registros


gráfico <- ggplot(AL, aes(x = FR, y = R, colour = L1, shape = L1))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
  scale_shape_manual(values=c(15, 16, 17))+
  stat_smooth(method = "lm",
              se = FALSE,
              fullrange = TRUE,
              size = 1)+
  labs( x ="Altitude (m)",
        y =  "Número de Registros (Log10)",
        colour = " ",
        shape = " ")+
  theme_classic()

REW <- lm(WAVRAL~ALWAVR, data = dados)
RES <- lm(SPLRAL~ALSPLR, data = dados)
RE <- lm(WAV2RAL~ALWAV2R, data = dados)

a <- qplot(fitted(REW), resid(REW))+ 
  geom_point(col = "blue")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

b <- qplot(fitted(RES), resid(RES))+ 
  geom_point(col = "Red")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

c <- qplot(fitted(RE), resid(RE))+ 
  geom_point(col = "Green")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

resíduos <- ggarrange(a,b,c, ncol = 1, nrow = 3)

png("31132.png", width = 1000, height = 450)

ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
dev.off()

#### Espécies


gráfico <- ggplot(AL, aes(x = FE, y = E, color = L2, shape = L2))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
  scale_shape_manual(values=c(15, 16, 17))+
  stat_smooth(method = "lm",
              se = FALSE,
              fullrange = TRUE,
              size = 1)+
  labs( x = "Altitude (m)",
        y = "Número de Espécies (Log10)",
        colour = " ",         
        shape = " ")+
  theme_classic()

REW <- lm(WAVEAL~ALWAVE, data = dados)
RES <- lm(SPLEAL~ALSPLE, data = dados)
RE <- lm(WAV2EAL~ALWAV2E, data = dados)


a <- qplot(fitted(REW), resid(REW))+ 
  geom_point(col = "blue")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

b <- qplot(fitted(RES), resid(RES))+ 
  geom_point(col = "Red")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

c <- qplot(fitted(RE), resid(RE))+ 
  geom_point(col = "Green")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

resíduos <- ggarrange(a,b,c, ncol = 1, nrow = 3)

png("32132.png", width = 1000, height = 450)

ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
dev.off()

### Área

#### Registros



gráfico <- ggplot(AR, aes(x = FR, y = R, color = L1, shape = L1))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
  scale_shape_manual(values=c(15, 16, 17))+
  stat_smooth(method = "lm",
              se = FALSE,
              fullrange = TRUE,
              size = 1)+
  labs( x ="Área (Log10(Km^2))",
        y =  "Número de Registros (Log10)",
        colour = " ",  
        shape = " ")+
  theme_classic()

REW <- lm(WAVRAR~ARWAVR, data = dados)
RES <- lm(SPLRAR~ARSPLR, data = dados)
RE <- lm(WAV2RAR~ARWAV2R, data = dados)

a <- qplot(fitted(REW), resid(REW))+ 
  geom_point(col = "blue")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

b <- qplot(fitted(RES), resid(RES))+ 
  geom_point(col = "Red")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

c <- qplot(fitted(RE), resid(RE))+ 
  geom_point(col = "Green")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

resíduos <- ggarrange(a,b,c, ncol = 1, nrow = 3)

png("31232.png", width = 1000, height = 450)
ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
dev.off()

#### Espécies


gráfico <- ggplot(AR, aes(x = FE, y = E, color = L2, shape = L2))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
  scale_shape_manual(values=c(15, 16, 17))+
  stat_smooth(method = "lm",
              se = FALSE,
              fullrange = TRUE,
              size = 1)+
  labs( x = "Área (Log10(Km^2))",
        y = "Número de Espécies (Log10)",
        colour = " ",
        shape = " ")+
  theme_classic()

REW <- lm(WAVEAR~ARWAVE, data = dados)
RES <- lm(SPLEAR~ARSPLE, data = dados)
RE <- lm(WAV2EAR~ARWAV2E, data = dados)

a <- qplot(fitted(REW), resid(REW))+ 
  geom_point(col = "blue")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

b <- qplot(fitted(RES), resid(RES))+ 
  geom_point(col = "Red")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

c <- qplot(fitted(RE), resid(RE))+ 
  geom_point(col = "Green")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

resíduos <- ggarrange(a,b,c, ncol = 1, nrow = 3)

png("32232.png", width = 1000, height = 450)
ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
dev.off()

### População

#### Registros


gráfico <- ggplot(POP, aes(x = FR, y = R, color = L1, shape = L1))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
  scale_shape_manual(values=c(15, 16, 17))+
  stat_smooth(method = "lm",
              se = FALSE,
              fullrange = TRUE,
              size = 1)+
  labs( x ="População (Log10)",
        y =  "Número de Registros (Log10)",
        colour = " ", 
        shape = " ")+
  theme_classic()

REW <- lm(WAVRP~PWAVR, data = dados)
RES <- lm(SPLRP~PSPLR, data = dados)
RE <- lm(WAV2RP~PWAV2R, data = dados)

a <- qplot(fitted(REW), resid(REW))+ 
  geom_point(col = "blue")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

b <- qplot(fitted(RES), resid(RES))+ 
  geom_point(col = "Red")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

c <- qplot(fitted(RE), resid(RE))+ 
  geom_point(col = "Green")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

resíduos <- ggarrange(a,b,c, ncol = 1, nrow = 3)

png("31332.png", width = 1000, height = 450)
ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
dev.off()

#### Espécies


gráfico <- ggplot(POP, aes(x = FE, y = E, color = L2, shape = L2))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
  scale_shape_manual(values=c(15, 16, 17))+
  stat_smooth(method = "lm",
              se = FALSE,
              fullrange = TRUE,
              size = 1)+
  labs( x = "População (Log10)",
        y = "Número de Espécies (Log10)",
        colour = " ",
        shape = " ")+
  theme_classic()

REW <- lm(WAVRE~PWAVE, data = dados)
RES <- lm(SPLEP~PSPLE, data = dados)
RE <- lm(WAV2EP~PWAV2E, data = dados)

a <- qplot(fitted(REW), resid(REW))+ 
  geom_point(col = "blue")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

b <- qplot(fitted(RES), resid(RES))+ 
  geom_point(col = "Red")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

c <- qplot(fitted(RE), resid(RE))+ 
  geom_point(col = "Green")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

resíduos <- ggarrange(a,b,c, ncol = 1, nrow = 3)

png("32332.png", width = 1000, height = 450)
ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
dev.off()

### Latitude

#### Registros


gráfico <- ggplot(LA, aes(x = FR, y = R, color = L1, shape = L1))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
  scale_shape_manual(values=c(15, 16, 17))+
  stat_smooth(method = "lm",
              se = FALSE,
              fullrange = TRUE,
              size = 1)+
  labs( x ="Latitude (º)",
        y =  "Número de Registros (Log10)",
        colour = " ", 
        shape = " ")+
  theme_classic()

REW <- lm(WAVRLA~LAWAVR, data = dados)
RES <- lm(SPLRLA~LASPLR, data = dados)
RE <- lm(WAV2RLA~LAWAV2R, data = dados)

a <- qplot(fitted(REW), resid(REW))+ 
  geom_point(col = "blue")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

b <- qplot(fitted(RES), resid(RES))+ 
  geom_point(col = "Red")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

c <- qplot(fitted(RE), resid(RE))+ 
  geom_point(col = "Green")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

resíduos <- ggarrange(a,b,c, ncol = 1, nrow = 3)

png("31432.png", width = 1000, height = 450)
ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
dev.off()

#### Espécies


gráfico <- ggplot(LA, aes(x = FE, y = E, color = L2, shape = L2))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
  scale_shape_manual(values=c(15, 16, 17))+
  stat_smooth(method = "lm",
              se = FALSE,
              fullrange = TRUE,
              size = 1)+
  labs( x ="Latitude (º)",
        y = "Número de Espécies (Log10)",
        colour = " ", 
        shape = " ")+
  theme_classic()

REW <- lm(WAVELA~LAWAVE, data = dados)
RES <- lm(SPLELA~LASPLE, data = dados)
RE <- lm(WAV2ELA~LAWAV2E, data = dados)

a <- qplot(fitted(REW), resid(REW))+ 
  geom_point(col = "blue")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

b <- qplot(fitted(RES), resid(RES))+ 
  geom_point(col = "Red")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

c <- qplot(fitted(RE), resid(RE))+ 
  geom_point(col = "Green")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

resíduos <- ggarrange(a,b,c, ncol = 1, nrow = 3)

png("32432.png", width = 1000, height = 450)
ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
dev.off()

### Longitude

#### Registros


gráfico <- ggplot(LO, aes(x = FR, y = R, color = L1, shape = L1))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
  scale_shape_manual(values=c(15, 16, 17))+
  stat_smooth(method = "lm",
              se = FALSE,
              fullrange = TRUE,
              size = 1)+
  labs( x ="Longitude (º)",
        y =  "Número de Registros (Log10)",
        colour = " ", 
        shape = " ")+
  theme_classic()

REW <- lm(WAVRLO~LOWAVR, data = dados)
RES <- lm(SPLRLO~LOSPLR, data = dados)
RE <- lm(WAV2RLO~LOWAV2R, data = dados)

a <- qplot(fitted(REW), resid(REW))+ 
  geom_point(col = "blue")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

b <- qplot(fitted(RES), resid(RES))+ 
  geom_point(col = "Red")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

c <- qplot(fitted(RE), resid(RE))+ 
  geom_point(col = "Green")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

resíduos <- ggarrange(a,b,c, ncol = 1, nrow = 3)

png("31532.png", width = 1000, height = 450)
ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
dev.off()

#### Espécies


gráfico <- ggplot(LO, aes(x = FE, y = E, color = L2, shape = L2))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
  scale_shape_manual(values=c(15, 16, 17))+
  stat_smooth(method = "lm",
              se = FALSE,
              fullrange = TRUE,
              size = 1)+
  labs( x ="Longitude (º)",
        y = "Número de Espécies (Log10)",
        colour = " ",     
        shape = " ")+
  theme_classic()

REW <- lm(WAVELO~LOWAVE, data = dados)
RES <- lm(SPLELO~LOSPLE, data = dados)
RE <- lm(WAV2ELO~LOWAV2E, data = dados)

a <- qplot(fitted(REW), resid(REW))+ 
  geom_point(col = "blue")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

b <- qplot(fitted(RES), resid(RES))+ 
  geom_point(col = "Red")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

c <- qplot(fitted(RE), resid(RE))+ 
  geom_point(col = "Green")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

resíduos <- ggarrange(a,b,c, ncol = 1, nrow = 3)

png("32532.png", width = 1000, height = 450)
ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
dev.off()

## Representação 3.3.

### Altitude

#### Registros


gráfico <- ggplot(AL, aes(x = FR, y = R, colour = L1, shape = L1))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green"))+
  scale_shape_manual(values=c(15, 16, 17))+
  stat_smooth(method = "lm",
              se = FALSE,
              fullrange = TRUE,
              size = 1)+
  labs( x ="Altitude (m)",
        y =  "Número de Registros (Log10)",
        colour = " ",
        shape = " ")+
  #guides(colour=FALSE)+
  theme_classic()

REW <- lm(WAVRAL~ALWAVR, data = dados)
RES <- lm(SPLRAL~ALSPLR, data = dados)
RE <- lm(WAV2RAL~ALWAV2R, data = dados)

R1 <- resid (REW)
R2 <- resid (RES)
R3 <- resid (RE)

min(R1,R2,R3)
max(R1,R2,R3)

AX <- hist(R1, 
           plot = FALSE, 
           breaks = c(-2.1,-1.875,-1.65,-1.425,-1.2,-0.925,-0.75,-0.525,-0.3,-0.075,0.15,0.375,0.6,0.825,1.05,1.275,1.5,1.725,1.95,2.175,2.4))
AX$density = AX$counts/sum(AX$counts)*100

BX <- hist(R2, 
           plot = FALSE, 
           breaks = c(-2.1,-1.875,-1.65,-1.425,-1.2,-0.925,-0.75,-0.525,-0.3,-0.075,0.15,0.375,0.6,0.825,1.05,1.275,1.5,1.725,1.95,2.175,2.4))
BX$density = BX$counts/sum(BX$counts)*100

CX <- hist(R3, 
           plot = FALSE, 
           breaks = c(-2.1,-1.875,-1.65,-1.425,-1.2,-0.925,-0.75,-0.525,-0.3,-0.075,0.15,0.375,0.6,0.825,1.05,1.275,1.5,1.725,1.95,2.175,2.4))
CX$density = CX$counts/sum(CX$counts)*100

y1 <- AX$density
y2 <- BX$density
y3 <- CX$density

x1 <- c()
x1[1] <- -2.1
for (i in 1:19){
  x1[i+1] = x1[i]+0.225
}
  
  
  A <- c()
  
  for (i in 1:20){
    A[i] = "WAV (N = 631)"
  }
  for (i in 21:40){
    A[i] = "SPL (N = 174)"
  }
  for (i in 41:60){
    A[i] = "WAV 2 (N = 173)"
  }
  
  B <- c()
  
  for (i in 1:20){
    B[i] = x1[i]
  }
  
  for(i in 21:40){
    B[i] = x1[i-20]
  }
  for(i in 41:60){
    B[i] = x1[i-40]
  }
  
  C<- c()
  
  for (i in 1:20){
    C[i] = y1[i]
  }
  
  for(i in 21:40){
    C[i] = y2[i-20]
  }
  for(i in 41:60){
    C[i] = y3[i-40]
  }
  
  data <- data.frame(L = A, x = B, y = C)
  
  Resíduos <-  ggplot(data, aes(x = x, y = C, colour = L))+
    geom_line()+
    scale_colour_manual(values = c("red", "blue", "green")) +
    #stat_smooth(method = lm, formula = y ~ poly(x, 4),se = FALSE, fullrange = TRUE)+
    labs(x = "Resíduos",
         y = "Frequência Relativa (%)",
         colour = " ")+
    theme_classic()
  
  png("31133.png", width = 1000, height = 450)
  ggarrange(gráfico, Resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
  dev.off()
  
  #### Espécies
  
  
  gráfico <- ggplot(AL, aes(x = FE, y = E, color = L2, shape = L2))+
    geom_point()+
    scale_colour_manual(values = c("blue", "red", "green")) +
    scale_shape_manual(values=c(15, 16, 17))+
    stat_smooth(method = "lm",
                se = FALSE,
                fullrange = TRUE,
                size = 1)+
    labs( x = "Altitude (m)",
          y = "Número de Espécies (Log10)",
          colour = " ",
          shape = " ")+
    theme_classic()
  
  REW <- lm(WAVEAL~ALWAVE, data = dados)
  RES <- lm(SPLEAL~ALSPLE, data = dados)
  RE <- lm(WAV2EAL~ALWAV2E, data = dados)
  
  R1 <- resid (REW)
  R2 <- resid (RES)
  R3 <- resid (RE)
  
  min(R1,R2,R3)
  max(R1,R2,R3)
  
  AX <- hist(R1, 
             plot = FALSE, 
             breaks = c(-1.3,-1.1,-0.9,-0.7,-0.5,-0.3,-0.1,0.1,0.3,0.5,0.7,0.9,1.1,1.3,1.5,1.7,1.9,2.1,2.3,2.5,2.7))
  AX$density = AX$counts/sum(AX$counts)*100
  
  BX <- hist(R2, 
             plot = FALSE, 
             breaks = c(-1.3,-1.1,-0.9,-0.7,-0.5,-0.3,-0.1,0.1,0.3,0.5,0.7,0.9,1.1,1.3,1.5,1.7,1.9,2.1,2.3,2.5,2.7))
  BX$density = BX$counts/sum(BX$counts)*100
  
  CX <- hist(R3, 
             plot = FALSE, 
             breaks = c(-1.3,-1.1,-0.9,-0.7,-0.5,-0.3,-0.1,0.1,0.3,0.5,0.7,0.9,1.1,1.3,1.5,1.7,1.9,2.1,2.3,2.5,2.7))
  CX$density = CX$counts/sum(CX$counts)*100
  
  y1 <- AX$density
  y2 <- BX$density
  y3 <- CX$density
  
  x1 <- c()
  x1[1] <- -1.3
  for (i in 1:19){
    x1[i+1] = x1[i]+0.2
  }
  
  
  A <- c()
  
  for (i in 1:20){
    A[i] = "WAV (N = 631)"
  }
  for (i in 21:40){
    A[i] = "SPL (N = 174)"
  }
  for (i in 41:60){
    A[i] = "WAV 2 (N = 173)"
  }
  
  B <- c()
  
  for (i in 1:20){
    B[i] = x1[i]
  }
  
  for(i in 21:40){
    B[i] = x1[i-20]
  }
  for(i in 41:60){
    B[i] = x1[i-40]
  }
  
  C<- c()
  
  for (i in 1:20){
    C[i] = y1[i]
  }
  
  for(i in 21:40){
    C[i] = y2[i-20]
  }
  for(i in 41:60){
    C[i] = y3[i-40]
  }
  
  data <- data.frame(L = A, x = B, y = C)
  
  Resíduos <-  ggplot(data, aes(x = x, y = C, colour = L))+
    geom_line()+
    scale_colour_manual(values = c("red", "blue", "green")) +
    #stat_smooth(method = lm, formula = y ~ poly(x, 4),se = FALSE, fullrange = TRUE)+
    labs(x = "Resíduos",
         y = "Frequência Relativa (%)",
         colour = " ")+
    theme_classic()
  
  png("32133.png", width = 1000, height = 450) 
  ggarrange(gráfico, Resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
dev.off
  
  ### Área
  
  #### Registros
  
  
  gráfico <- ggplot(AR, aes(x = FR, y = R, color = L1, shape = L1))+
    geom_point()+
    scale_colour_manual(values = c("blue", "red", "green")) +
    scale_shape_manual(values=c(15, 16, 17))+
    stat_smooth(method = "lm",
                se = FALSE,
                fullrange = TRUE,
                size = 1)+
    labs( x ="Área (Log10(Km^2))",
          y =  "Número de Registros (Log10)",
          colour = " ",
          shape = " ")+
    theme_classic()
  
  REW <- lm(WAVRAR~ARWAVR, data = dados)
  RES <- lm(SPLRAR~ARSPLR, data = dados)
  RE <- lm(WAV2RAR~ARWAV2R, data = dados)
  
  R1 <- resid (REW)
  R2 <- resid (RES)
  R3 <- resid (RE)
  
  min(R1,R2,R3)
  max(R1,R2,R3)
  
  AX <- hist(R1, 
             plot = FALSE, 
             breaks = c(-2.4,-2.165,-1.93,-1.695,-1.46,-1.225,-0.99,-0.755,-0.52,-0.285,-0.05,0.185,0.42,0.655,0.89,1.125,1.36,1.595,1.83,2.065,2.3))
  AX$density = AX$counts/sum(AX$counts)*100
  
  BX <- hist(R2, 
             plot = FALSE, 
             breaks = c(-2.4,-2.165,-1.93,-1.695,-1.46,-1.225,-0.99,-0.755,-0.52,-0.285,-0.05,0.185,0.42,0.655,0.89,1.125,1.36,1.595,1.83,2.065,2.3))
  BX$density = BX$counts/sum(BX$counts)*100
  
  CX <- hist(R3, 
             plot = FALSE, 
             breaks = c(-2.4,-2.165,-1.93,-1.695,-1.46,-1.225,-0.99,-0.755,-0.52,-0.285,-0.05,0.185,0.42,0.655,0.89,1.125,1.36,1.595,1.83,2.065,2.3))
  CX$density = CX$counts/sum(CX$counts)*100
  
  y1 <- AX$density
  y2 <- BX$density
  y3 <- CX$density
  
  x1 <- c()
  x1[1] <- -2.4
  for (i in 1:19){
    x1[i+1] = x1[i]+0.235
  }
  
  
  A <- c()
  
  for (i in 1:20){
    A[i] = "WAV (N = 631)"
  }
  for (i in 21:40){
    A[i] = "SPL (N = 174)"
  }
  for (i in 41:60){
    A[i] = "WAV 2 (N = 173)"
  }
  
  B <- c()
  
  for (i in 1:20){
    B[i] = x1[i]
  }
  
  for(i in 21:40){
    B[i] = x1[i-20]
  }
  for(i in 41:60){
    B[i] = x1[i-40]
  }
  
  C<- c()
  
  for (i in 1:20){
    C[i] = y1[i]
  }
  
  for(i in 21:40){
    C[i] = y2[i-20]
  }
  for(i in 41:60){
    C[i] = y3[i-40]
  }
  
  data <- data.frame(L = A, x = B, y = C)
  
  Resíduos <-  ggplot(data, aes(x = x, y = C, colour = L))+
    geom_line()+
    scale_colour_manual(values = c("red", "blue", "green")) +
    #stat_smooth(method = lm, formula = y ~ poly(x, 4),se = FALSE, fullrange = TRUE)+
    labs(x = "Resíduos",
         y = "Frequência Relativa (%)",
         colour = " ")+
    theme_classic()
  
  
  png("31233.png", width = 1000, height = 450)
  ggarrange(gráfico, Resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
  dev.off()

  
  #### Espécies
  
  
  gráfico <- ggplot(AR, aes(x = FE, y = E, color = L2, shape = L2))+
    geom_point()+
    scale_colour_manual(values = c("blue", "red", "green")) +
    scale_shape_manual(values=c(15, 16, 17))+
    stat_smooth(method = "lm",
                se = FALSE,
                fullrange = TRUE,
                size = 1)+
    labs( x = "Área (Log10(Km^2))",
          y = "Número de Espécies (Log10)",
          colour = " ",
          shape = " ")+
    theme_classic()
  
  REW <- lm(WAVEAR~ARWAVE, data = dados)
  RES <- lm(SPLEAR~ARSPLE, data = dados)
  RE <- lm(WAV2EAR~ARWAV2E, data = dados)
  
  R1 <- resid (REW)
  R2 <- resid (RES)
  R3 <- resid (RE)
  
  min(R1,R2,R3)
  max(R1,R2,R3)
  
  AX <- hist(R1, 
             plot = FALSE, 
             breaks = c(-1.4,-1.26,-1.12,-0.98,-0.84,-0.7,-0.56,-0.42,-0.28,-0.14,0,0.14,0.28,0.42,0.56,0.7,0.84,0.98,1.12,1.26,1.4))
  AX$density = AX$counts/sum(AX$counts)*100
  
  BX <- hist(R2, 
             plot = FALSE, 
             breaks = c(-1.4,-1.26,-1.12,-0.98,-0.84,-0.7,-0.56,-0.42,-0.28,-0.14,0,0.14,0.28,0.42,0.56,0.7,0.84,0.98,1.12,1.26,1.4))
  BX$density = BX$counts/sum(BX$counts)*100
  
  CX <- hist(R3, 
             plot = FALSE, 
             breaks = c(-1.4,-1.26,-1.12,-0.98,-0.84,-0.7,-0.56,-0.42,-0.28,-0.14,0,0.14,0.28,0.42,0.56,0.7,0.84,0.98,1.12,1.26,1.4))
  CX$density = CX$counts/sum(CX$counts)*100
  
  y1 <- AX$density
  y2 <- BX$density
  y3 <- CX$density
  
  x1 <- c()
  x1[1] <- -1.4
  for (i in 1:19){
    x1[i+1] = x1[i]+0.14
  }
  
  
  A <- c()
  
  for (i in 1:20){
    A[i] = "WAV (N = 631)"
  }
  for (i in 21:40){
    A[i] = "SPL (N = 174)"
  }
  for (i in 41:60){
    A[i] = "WAV 2 (N = 173)"
  }
  
  B <- c()
  
  for (i in 1:20){
    B[i] = x1[i]
  }
  
  for(i in 21:40){
    B[i] = x1[i-20]
  }
  for(i in 41:60){
    B[i] = x1[i-40]
  }
  
  C<- c()
  
  for (i in 1:20){
    C[i] = y1[i]
  }
  
  for(i in 21:40){
    C[i] = y2[i-20]
  }
  for(i in 41:60){
    C[i] = y3[i-40]
  }
  
  data <- data.frame(L = A, x = B, y = C)
  
  Resíduos <-  ggplot(data, aes(x = x, y = C, colour = L))+
    geom_line()+
    scale_colour_manual(values = c("red", "blue", "green")) +
    #stat_smooth(method = lm, formula = y ~ poly(x, 4),se = FALSE, fullrange = TRUE)+
    labs(x = "Resíduos",
         y = "Frequência Relativa (%)",
         colour = " ")+
    theme_classic()
  
  
  png("32233.png", width = 1000, height = 450)
  ggarrange(gráfico, Resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
  dev.off()

  
  ### População
  
  #### Registros
  
  
  gráfico <- ggplot(POP, aes(x = FR, y = R, color = L1, shape = L1))+
    geom_point()+
    scale_colour_manual(values = c("blue", "red", "green")) +
    scale_shape_manual(values=c(15, 16, 17))+
    stat_smooth(method = "lm",
                se = FALSE,
                fullrange = TRUE,
                size = 1)+
    labs( x ="População (Log10)",
          y =  "Número de Registros (Log10)",
          colour = " ",
          shape = " ")+
    theme_classic()
  
  REW <- lm(WAVRP~PWAVR, data = dados)
  RES <- lm(SPLRP~PSPLR, data = dados)
  RE <- lm(WAV2RP~PWAV2R, data = dados)
  
  R1 <- resid (REW)
  R2 <- resid (RES)
  R3 <- resid (RE)
  
  min(R1,R2,R3)
  max(R1,R2,R3)
  
  AX <- hist(R1, 
             plot = FALSE, 
             breaks = c(-1.7,-1.495,-1.29,-1.085,-0.88,-0.675,-0.47,-0.265,-0.06,0.145,0.35,0.555,0.76,0.965,1.17,1.375,1.58,1.785,1.99,2.195,2.4))
  AX$density = AX$counts/sum(AX$counts)*100
  
  BX <- hist(R2, 
             plot = FALSE, 
             breaks = c(-1.7,-1.495,-1.29,-1.085,-0.88,-0.675,-0.47,-0.265,-0.06,0.145,0.35,0.555,0.76,0.965,1.17,1.375,1.58,1.785,1.99,2.195,2.4))
  BX$density = BX$counts/sum(BX$counts)*100
  
  CX <- hist(R3, 
             plot = FALSE, 
             breaks = c(-1.7,-1.495,-1.29,-1.085,-0.88,-0.675,-0.47,-0.265,-0.06,0.145,0.35,0.555,0.76,0.965,1.17,1.375,1.58,1.785,1.99,2.195,2.4))
  CX$density = CX$counts/sum(CX$counts)*100
  
  y1 <- AX$density
  y2 <- BX$density
  y3 <- CX$density
  
  x1 <- c()
  x1[1] <- -1.7
  for (i in 1:19){
    x1[i+1] = x1[i]+0.205
  }
  
  
  A <- c()
  
  for (i in 1:20){
    A[i] = "WAV (N = 631)"
  }
  for (i in 21:40){
    A[i] = "SPL (N = 174)"
  }
  for (i in 41:60){
    A[i] = "WAV 2 (N = 173)"
  }
  
  B <- c()
  
  for (i in 1:20){
    B[i] = x1[i]
  }
  
  for(i in 21:40){
    B[i] = x1[i-20]
  }
  for(i in 41:60){
    B[i] = x1[i-40]
  }
  
  C<- c()
  
  for (i in 1:20){
    C[i] = y1[i]
  }
  
  for(i in 21:40){
    C[i] = y2[i-20]
  }
  for(i in 41:60){
    C[i] = y3[i-40]
  }
  
  data <- data.frame(L = A, x = B, y = C)
  
  Resíduos <-  ggplot(data, aes(x = x, y = C, colour = L))+
    geom_line()+
    scale_colour_manual(values = c("red", "blue", "green")) +
    #stat_smooth(method = lm, formula = y ~ poly(x, 4),se = FALSE, fullrange = TRUE)+
    labs(x = "Resíduos",
         y = "Frequência Relativa (%)",
         colour = " ")+
    theme_classic()
  
  
  png("31333.png", width = 1000, height = 450)
  ggarrange(gráfico, Resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
  dev.off()

  
  #### Espécies
  
  
  gráfico <- ggplot(POP, aes(x = FE, y = E, color = L2, shape = L2))+
    geom_point()+
    scale_colour_manual(values = c("blue", "red", "green")) +
    scale_shape_manual(values=c(15, 16, 17))+
    stat_smooth(method = "lm",
                se = FALSE,
                fullrange = TRUE,
                size = 1)+
    labs( x = "População (Log10)",
          y = "Número de Espécies (Log10)",
          color = " ",
          shape = " ")+
    theme_classic()
  
  REW <- lm(WAVRE~PWAVE, data = dados)
  RES <- lm(SPLEP~PSPLE, data = dados)
  RE <- lm(WAV2EP~PWAV2E, data = dados)
  
  R1 <- resid (REW)
  R2 <- resid (RES)
  R3 <- resid (RE)
  
  min(R1,R2,R3)
  max(R1,R2,R3)
  
  AX <- hist(R1, 
             plot = FALSE, 
             breaks = c(-1.2,-1.06,-0.92,-0.78,-0.74,-0.5,-0.36,-0.22,-0.08,0.06,0.2,0.34,0.48,0.62,0.76,0.9,1.04,1.18,1.32,1.46,1.6))
  AX$density = AX$counts/sum(AX$counts)*100
  
  BX <- hist(R2, 
             plot = FALSE, 
             breaks = c(-1.2,-1.06,-0.92,-0.78,-0.74,-0.5,-0.36,-0.22,-0.08,0.06,0.2,0.34,0.48,0.62,0.76,0.9,1.04,1.18,1.32,1.46,1.6))
  BX$density = BX$counts/sum(BX$counts)*100
  
  CX <- hist(R3, 
             plot = FALSE, 
             breaks = c(-1.2,-1.06,-0.92,-0.78,-0.74,-0.5,-0.36,-0.22,-0.08,0.06,0.2,0.34,0.48,0.62,0.76,0.9,1.04,1.18,1.32,1.46,1.6))
  CX$density = CX$counts/sum(CX$counts)*100
  
  y1 <- AX$density
  y2 <- BX$density
  y3 <- CX$density
  
  x1 <- c()
  x1[1] <- -1.2
  for (i in 1:19){
    x1[i+1] = x1[i]+0.14
  }
  
  
  A <- c()
  
  for (i in 1:20){
    A[i] = "WAV (N = 631)"
  }
  for (i in 21:40){
    A[i] = "SPL (N = 174)"
  }
  for (i in 41:60){
    A[i] = "WAV 2 (N = 173)"
  }
  
  B <- c()
  
  for (i in 1:20){
    B[i] = x1[i]
  }
  
  for(i in 21:40){
    B[i] = x1[i-20]
  }
  for(i in 41:60){
    B[i] = x1[i-40]
  }
  
  C<- c()
  
  for (i in 1:20){
    C[i] = y1[i]
  }
  
  for(i in 21:40){
    C[i] = y2[i-20]
  }
  for(i in 41:60){
    C[i] = y3[i-40]
  }
  
  data <- data.frame(L = A, x = B, y = C)
  
  Resíduos <-  ggplot(data, aes(x = x, y = C, colour = L))+
    geom_line()+
    scale_colour_manual(values = c("red", "blue", "green")) +
    #stat_smooth(method = lm, formula = y ~ poly(x, 4),se = FALSE, fullrange = TRUE)+
    labs(x = "Resíduos",
         y = "Frequência Relativa (%)",
         colour = " ")+
    theme_classic()
  
  
  png("32333.png", width = 1000, height = 450)
  ggarrange(gráfico, Resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
  dev.off()

  
  ### Latitude
  
  #### Registros
  
  
  gráfico <- ggplot(LA, aes(x = FR, y = R, color = L1, shape = L1))+
    geom_point()+
    scale_colour_manual(values = c("blue", "red", "green")) +
    scale_shape_manual(values=c(15, 16, 17))+
    stat_smooth(method = "lm",
                se = FALSE,
                fullrange = TRUE,
                size = 1)+
    labs( x ="Latitude (º)",
          y =  "Número de Registros (Log10)",
          colour = " ",
          shape = " ")+
    theme_classic()
  
  REW <- lm(WAVRLA~LAWAVR, data = dados)
  RES <- lm(SPLRLA~LASPLR, data = dados)
  RE <- lm(WAV2RLA~LAWAV2R, data = dados)
  
  R1 <- resid (REW)
  R2 <- resid (RES)
  R3 <- resid (RE)
  
  min(R1,R2,R3)
  max(R1,R2,R3)
  
  
  AX <- hist(R1, 
             plot = FALSE, 
             breaks = c(-2.2,-1.965,-1.73,-1.495,-1.26,-1.025,-0.79,-0.555,-0.32,-0.085,0.15,0.385,0.62,0.855,1.09,1.325,1.56,1.795,2.03,2.265,2.5))
  AX$density = AX$counts/sum(AX$counts)*100
  
  BX <- hist(R2, 
             plot = FALSE, 
             breaks = c(-2.2,-1.965,-1.73,-1.495,-1.26,-1.025,-0.79,-0.555,-0.32,-0.085,0.15,0.385,0.62,0.855,1.09,1.325,1.56,1.795,2.03,2.265,2.5))
  BX$density = BX$counts/sum(BX$counts)*100
  
  CX <- hist(R3, 
             plot = FALSE, 
             breaks = c(-2.2,-1.965,-1.73,-1.495,-1.26,-1.025,-0.79,-0.555,-0.32,-0.085,0.15,0.385,0.62,0.855,1.09,1.325,1.56,1.795,2.03,2.265,2.5))
  CX$density = CX$counts/sum(CX$counts)*100
  
  y1 <- AX$density
  y2 <- BX$density
  y3 <- CX$density
  
  x1 <- c()
  x1[1] <- -2.2
  for (i in 1:19){
    x1[i+1] = x1[i]+0.235
  }
  
  A <- c()
  
  for (i in 1:20){
    A[i] = "WAV (N = 631)"
  }
  for (i in 21:40){
    A[i] = "SPL (N = 174)"
  }
  for (i in 41:60){
    A[i] = "WAV 2 (N = 173)"
  }
  
  B <- c()
  
  for (i in 1:20){
    B[i] = x1[i]
  }
  
  for(i in 21:40){
    B[i] = x1[i-20]
  }
  for(i in 41:60){
    B[i] = x1[i-40]
  }
  
  C<- c()
  
  for (i in 1:20){
    C[i] = y1[i]
  }
  
  for(i in 21:40){
    C[i] = y2[i-20]
  }
  for(i in 41:60){
    C[i] = y3[i-40]
  }
  
  data <- data.frame(L = A, x = B, y = C)
  
  Resíduos <-  ggplot(data, aes(x = x, y = C, colour = L))+
    geom_line()+
    scale_colour_manual(values = c("red", "blue", "green")) +
    #stat_smooth(method = lm, formula = y ~ poly(x, 4),se = FALSE, fullrange = TRUE)+
    labs(x = "Resíduos",
         y = "Frequência Relativa (%)",
         colour = " ")+
    theme_classic()
  
  
  png("31433.png", width = 1000, height = 450)
  ggarrange(gráfico, Resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
  dev.off()

  
  #### Espécies
  
  
  gráfico <- ggplot(LA, aes(x = FE, y = E, color = L2, shape = L2))+
    geom_point()+
    scale_colour_manual(values = c("blue", "red", "green")) +
    scale_shape_manual(values=c(15, 16, 17))+
    stat_smooth(method = "lm",
                se = FALSE,
                fullrange = TRUE,
                size = 1)+
    labs( x ="Latitude (º)",
          y = "Número de Espécies (Log10)",
          colour = " ",
          shape = " ")+
    theme_classic()
  
  REW <- lm(WAVELA~LAWAVE, data = dados)
  RES <- lm(SPLELA~LASPLE, data = dados)
  RE <- lm(WAV2ELA~LAWAV2E, data = dados)
  
  R1 <- resid (REW)
  R2 <- resid (RES)
  R3 <- resid (RE)
  
  min(R1,R2,R3)
  max(R1,R2,R3)
  
  
  AX <- hist(R1, 
             plot = FALSE, 
             breaks = c(-1.4,-1.245,-1.09,-0.935,-0.78,-0.625,-0.47,-0.315,-0.16,-0.005,0.15,0.305,0.46,0.615,0.77,0.925,1.08,1.235,1.39,1.545,1.7))
  AX$density = AX$counts/sum(AX$counts)*100
  
  BX <- hist(R2, 
             plot = FALSE, 
             breaks = c(-1.4,-1.245,-1.09,-0.935,-0.78,-0.625,-0.47,-0.315,-0.16,-0.005,0.15,0.305,0.46,0.615,0.77,0.925,1.08,1.235,1.39,1.545,1.7))
  BX$density = BX$counts/sum(BX$counts)*100
  
  CX <- hist(R3, 
             plot = FALSE, 
             breaks = c(-1.4,-1.245,-1.09,-0.935,-0.78,-0.625,-0.47,-0.315,-0.16,-0.005,0.15,0.305,0.46,0.615,0.77,0.925,1.08,1.235,1.39,1.545,1.7))
  CX$density = CX$counts/sum(CX$counts)*100
  
  y1 <- AX$density
  y2 <- BX$density
  y3 <- CX$density
  
  x1 <- c()
  x1[1] <- -1.4
  for (i in 1:19){
    x1[i+1] = x1[i]+0.155
  }
  
  
  A <- c()
  
  for (i in 1:20){
    A[i] = "WAV (N = 631)"
  }
  for (i in 21:40){
    A[i] = "SPL (N = 174)"
  }
  for (i in 41:60){
    A[i] = "WAV 2 (N = 173)"
  }
  
  B <- c()
  
  for (i in 1:20){
    B[i] = x1[i]
  }
  
  for(i in 21:40){
    B[i] = x1[i-20]
  }
  for(i in 41:60){
    B[i] = x1[i-40]
  }
  
  C<- c()
  
  for (i in 1:20){
    C[i] = y1[i]
  }
  
  for(i in 21:40){
    C[i] = y2[i-20]
  }
  for(i in 41:60){
    C[i] = y3[i-40]
  }
  
  data <- data.frame(L = A, x = B, y = C)
  
  Resíduos <-  ggplot(data, aes(x = x, y = C, colour = L))+
    geom_line()+
    scale_colour_manual(values = c("red", "blue", "green")) +
    #stat_smooth(method = lm, formula = y ~ poly(x, 4),se = FALSE, fullrange = TRUE)+
    labs(x = "Resíduos",
         y = "Frequência Relativa (%)",
         colour = " ")+
    theme_classic()
  
  
  png("32433.png", width = 1000, height = 450)
  ggarrange(gráfico, Resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
  dev.off()

  
  ### Longitude
  
  #### Registros
  
  
  gráfico <- ggplot(LO, aes(x = FR, y = R, color = L1, shape = L1))+
    geom_point()+
    scale_colour_manual(values = c("blue", "red", "green")) +
    scale_shape_manual(values=c(15, 16, 17))+
    stat_smooth(method = "lm",
                se = FALSE,
                fullrange = TRUE,
                size = 1)+
    labs( x ="Longitude (º)",
          y =  "Número de Registros (Log10)",
          colour = " ",
          shape = " ")+
    theme_classic()
  
  REW <- lm(WAVRLO~LOWAVR, data = dados)
  RES <- lm(SPLRLO~LOSPLR, data = dados)
  RE <- lm(WAV2RLO~LOWAV2R, data = dados)
  
  R1 <- resid (REW)
  R2 <- resid (RES)
  R3 <- resid (RE)
  
  min(R1,R2,R3)
  max(R1,R2,R3)
  
  AX <- hist(R1, 
             plot = FALSE, 
             breaks = c(-1.8,-1.59,-1.38,-1.17,-0.96,-0.75,-0.54,-0.33,-0.12,0.09,0.3,0.51,0.72,0.93,1.14,1.35,1.56,1.77,1.98,2.19,2.4))
  AX$density = AX$counts/sum(AX$counts)*100
  
  BX <- hist(R2, 
             plot = FALSE, 
             breaks = c(-1.8,-1.59,-1.38,-1.17,-0.96,-0.75,-0.54,-0.33,-0.12,0.09,0.3,0.51,0.72,0.93,1.14,1.35,1.56,1.77,1.98,2.19,2.4))
  BX$density = BX$counts/sum(BX$counts)*100
  
  CX <- hist(R3, 
             plot = FALSE, 
             breaks = c(-1.8,-1.59,-1.38,-1.17,-0.96,-0.75,-0.54,-0.33,-0.12,0.09,0.3,0.51,0.72,0.93,1.14,1.35,1.56,1.77,1.98,2.19,2.4))
  CX$density = CX$counts/sum(CX$counts)*100
  
  y1 <- AX$density
  y2 <- BX$density
  y3 <- CX$density
  
  x1 <- c()
  x1[1] <- -1.8
  for (i in 1:19){
    x1[i+1] = x1[i]+0.21
  }
  
  
  A <- c()
  
  for (i in 1:20){
    A[i] = "WAV (N = 631)"
  }
  for (i in 21:40){
    A[i] = "SPL (N = 174)"
  }
  for (i in 41:60){
    A[i] = "WAV 2 (N = 173)"
  }
  
  B <- c()
  
  for (i in 1:20){
    B[i] = x1[i]
  }
  
  for(i in 21:40){
    B[i] = x1[i-20]
  }
  for(i in 41:60){
    B[i] = x1[i-40]
  }
  
  C<- c()
  
  for (i in 1:20){
    C[i] = y1[i]
  }
  
  for(i in 21:40){
    C[i] = y2[i-20]
  }
  for(i in 41:60){
    C[i] = y3[i-40]
  }
  
  data <- data.frame(L = A, x = B, y = C)
  
  Resíduos <-  ggplot(data, aes(x = x, y = C, colour = L))+
    geom_line()+
    scale_colour_manual(values = c("red", "blue", "green")) +
    #stat_smooth(method = lm, formula = y ~ poly(x, 4),se = FALSE, fullrange = TRUE)+
    labs(x = "Resíduos",
         y = "Frequência Relativa (%)",
         colour = " ")+
    theme_classic()
  
  
  png("31533.png", width = 1000, height = 450)
  ggarrange(gráfico, Resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
  dev.off()

  
  #### Espécies
  
  
  gráfico <- ggplot(LO, aes(x = FE, y = E, color = L2, shape = L2))+
    geom_point()+
    scale_colour_manual(values = c("blue", "red", "green")) +
    scale_shape_manual(values=c(15, 16, 17))+
    stat_smooth(method = "lm",
                se = FALSE,
                fullrange = TRUE,
                size = 1)+
    labs( x ="Longitude (º)",
          y = "Número de Espécies (Log10)",
          colour = " ",
          shape = " ")+
    theme_classic()
  
  REW <- lm(WAVELO~LOWAVE, data = dados)
  RES <- lm(SPLELO~LOSPLE, data = dados)
  RE <- lm(WAV2ELO~LOWAV2E, data = dados)
  
  R1 <- resid (REW)
  R2 <- resid (RES)
  R3 <- resid (RE)
  
  min(R1,R2,R3)
  max(R1,R2,R3)
  
  AX <- hist(R1, 
             plot = FALSE, 
             breaks = c(-1.2,-1.065,-0.93,-0.795,-0.66,-0.525,-0.39,-0.255,-0.12,0.015,0.15,0.285,0.42,0.555,0.69,0.825,0.96,1.095,1.23,1.365,1.5))
  AX$density = AX$counts/sum(AX$counts)*100
  
  BX <- hist(R2, 
             plot = FALSE, 
             breaks = c(-1.2,-1.065,-0.93,-0.795,-0.66,-0.525,-0.39,-0.255,-0.12,0.015,0.15,0.285,0.42,0.555,0.69,0.825,0.96,1.095,1.23,1.365,1.5))
  BX$density = BX$counts/sum(BX$counts)*100
  
  CX <- hist(R3, 
             plot = FALSE, 
             breaks = c(-1.2,-1.065,-0.93,-0.795,-0.66,-0.525,-0.39,-0.255,-0.12,0.015,0.15,0.285,0.42,0.555,0.69,0.825,0.96,1.095,1.23,1.365,1.5))
  CX$density = CX$counts/sum(CX$counts)*100
  
  y1 <- AX$density
  y2 <- BX$density
  y3 <- CX$density
  
  x1 <- c()
  x1[1] <- -1.2
  for (i in 1:19){
    x1[i+1] = x1[i]+0.135
  }
  
  
  A <- c()
  
  for (i in 1:20){
    A[i] = "WAV (N = 631)"
  }
  for (i in 21:40){
    A[i] = "SPL (N = 174)"
  }
  for (i in 41:60){
    A[i] = "WAV 2 (N = 173)"
  }
  
  B <- c()
  
  for (i in 1:20){
    B[i] = x1[i]
  }
  
  for(i in 21:40){
    B[i] = x1[i-20]
  }
  for(i in 41:60){
    B[i] = x1[i-40]
  }
  
  C<- c()
  
  for (i in 1:20){
    C[i] = y1[i]
  }
  
  for(i in 21:40){
    C[i] = y2[i-20]
  }
  for(i in 41:60){
    C[i] = y3[i-40]
  }
  
  data <- data.frame(L = A, x = B, y = C)
  
  Resíduos <-  ggplot(data, aes(x = x, y = C, colour = L))+
    geom_line()+
    scale_colour_manual(values = c("red", "blue", "green")) +
    #stat_smooth(method = lm, formula = y ~ poly(x, 4),se = FALSE, fullrange = TRUE)+
    labs(x = "Resíduos",
         y = "Frequência Relativa (%)",
         colour = " ")+
    theme_classic()
  
  
  png("32533.png", width = 1000, height = 450)
  ggarrange(gráfico, Resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
  dev.off()
  
  
rm(list=ls())

  
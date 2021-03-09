rm(list=ls())

library("openxlsx")
library(ggplot2)
library(esquisse)
library(sf)
library(RColorBrewer)
library(gtable)
library(gridExtra)

WS <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/R/S1/WAVxSPL.xlsx")
WAV <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/R/S1/WAV.xlsx")
SPL <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/R/S1/SPL.xlsx")
ESP <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/R/S1/Espécies.xlsx")
FE <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/R/S2/FE.xlsx")
AL <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/R/S3/Altitude.xlsx")
AR <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/R/S3/Area.xlsx")
POP <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/R/S3/Populacao.xlsx")
LA <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/R/S3/Latitude.xlsx")
LO <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/R/S3/Longitude.xlsx")
dados <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/R/S3/Auxiliar.xlsx")
S4 <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/R/S4/secao4.xlsx") 


##Seção 1:

A1 <- hist(WAV$WAVR, breaks = 25)
B1 <- hist(SPL$SPLR, breaks = 25)
C1 <- hist (WS$WAV2R, breaks = 25)
A2 <- hist(WAV$WAVE, breaks = 25)
B2 <- hist(SPL$SPLE, breaks = 25)
C2 <- hist(WS$WAV2E, breaks = 25)
A3 <- hist(ESP$WAV, breaks = 25)
B3 <- hist(ESP$SPL, breaks = 25)
C3 <- hist(ESP$WAV2, breaks = 25)


#Registros por Muicipio:

plot(A1, col=rgb(0,0,1,1/2),
     main = " ",
     xlab = "Número de Registros (Log10)",
     ylab = "Frequência Relativa (%)",
     freq = FALSE,
     ylim = c(0,1),
     xlim=c(0,5))
plot(B1, col=rgb(1,0,0,1/2), freq = FALSE, add=T) 
plot(C1, col=rgb(0,1,0,1/2), freq = FALSE, add=T)

#Espécies por Municipio:

plot(A2, col=rgb(0,0,1,1/2),
     main = " ",
     xlab = "Número de Espécies (Log10)",
     ylab = "Frequência Relativa (%)",
     freq = FALSE,
     ylim = c(0,3),
     xlim=c(0,3))
plot(B2, col=rgb(1,0,0,1/2), freq = FALSE, add=T) 
plot(C2, col=rgb(0,1,0,1/2), freq = FALSE, add=T)
#plot(C2, col=rgb(1,1,0,1/2), freq = FALSE, add=T)

#Registros por espécies:

plot(A3, col=rgb(0,0,1,1/2),
     main = " ",
     xlab = "Número de Espécies (Log10)",
     ylab = "Frequência Relativa (%)",
     freq = FALSE,
     ylim = c(0,1),
     xlim=c(0,4))
plot(B3, col=rgb(1,0,0,1/2), freq = FALSE, add=T) 
plot(C3, col=rgb(0,1,0,1/2), freq = FALSE, add=T)

##Seção 2

WAL <- hist(FE$WAVAL, breaks = 25)
SAL <- hist(FE$SPLAL, breaks = 25)
WAR <- hist(FE$WAVAR, breaks = 25)
SAR <- hist(FE$SPLAR, breaks = 25)
WPOP <- hist(FE$WAVPOP, breaks = 25)
SPOP <- hist(FE$SPLPOP, breaks = 25)
WLA <- hist(FE$WAVLA, breaks = 25)
SLA <- hist(FE$SPLLA, breaks = 25)
WLO <- hist(FE$WAVLO, breaks = 25)
SLO <- hist(FE$SPLLO, breaks = 25)

#Altitude

plot(WAL, col=rgb(0,0,1,1/2),
     main = " ",
     xlab = "Altitude (m)",
     ylab = "Frequência Relativa (%)",
     freq = FALSE,
     #ylim = c(0,1),
     xlim=c(0,1800))
plot(SAL, col=rgb(1,0,0,1/2), freq = FALSE, add=T) 

#Area

plot(WAR, col=rgb(0,0,1,1/2),
     main = " ",
     xlab = "Área (Log10(Km^2))",
     ylab = "Frequência Relativa (%)",
     freq = FALSE,
     ylim = c(0,1.3),
     xlim=c(0,4))
plot(SAR, col=rgb(1,0,0,1/2), freq = FALSE, add=T) 

#População

plot(WPOP, col=rgb(0,0,1,1/2),
     main = " ",
     xlab = "População (Log10)",
     ylab = "Frequência Relativa (%)",
     freq = FALSE,
     ylim = c(0,0.8),
     xlim=c(2,7.5))
plot(SPOP, col=rgb(1,0,0,1/2), freq = FALSE, add=T) 

#Latitude

plot(WLA, col=rgb(0,0,1,1/2),
     main = " ",
     xlab = "Latitude (°)",
     ylab = "Frequência Relativa (%)",
     freq = FALSE,
     ylim = c(0,0.5),
     xlim=c(-25.5,-19.5))
plot(SLA, col=rgb(1,0,0,1/2), freq = FALSE, add=T)

#Longitude

plot(WLO, col=rgb(0,0,1,1/2),
     main = " ",
     xlab = "Longitude (°)",
     ylab = "Frequência Relativa (%)",
     freq = FALSE,
     ylim = c(0,0.4),
     xlim=c(-55,-42))
plot(SLO, col=rgb(1,0,0,1/2), freq = FALSE, add=T) 


##Seção 3

#Altitude

ggplot(AL, aes(x = FR, y = R, color = L1))+
        geom_point()+
        scale_colour_manual(values = c("blue", "red", "green")) +
        stat_smooth(method = "lm",
                    se = FALSE,
                    fullrange = TRUE,
                    size = 1)+
        labs( x ="Altitude (m)",
              y =  "Número de Registros (Log10)")+
        theme_minimal()

REW <- lm(ALWAVR~WAVRAL, data = dados)
RES <- lm(ALSPLR~SPLRAL, data = dados)
RE <- lm(ALWAV2R~WAV2RAL, data = dados)
R1 <- resid(REW)
R2 <- resid(RES)
R3 <- resid(RE)

plot(fitted(REW), R1, 
     pch = 20, 
     bty = "n",
     col = "blue",
     xlab = "Resíduos",
     ylab = "Frequência")
points(fitted(RES), R2, 
     pch = 20, 
     bty = "n",
     col = "Red")
points(fitted(RE), R3, 
     pch = 20, 
     bty = "n",
     col = "green")
abline(0,0)

H1 <- hist(R1, breaks = 25)
H2 <- hist(R2, breaks = 25)
H3 <- hist(R3, breaks = 25)

plot(H1, col=rgb(0,0,1,1/2),
     main = " ",
     xlab = "Resíduos",
     ylab = "Frequência",
     freq = TRUE,
     ylim = c(0,100),
     #xlim=c(-25.5,-19.5))
)
plot(H2, col=rgb(1,0,0,1/2), freq = TRUE, add=T) 
plot(H3, col=rgb(0,1,0,1/2), freq = TRUE, add=T)


ggplot(AL, aes(x = FE, y = E, color = L2))+
        geom_point()+
        scale_colour_manual(values = c("blue", "red", "green")) +
        stat_smooth(method = "lm",
                    se = FALSE,
                    fullrange = TRUE,
                    size = 1)+
        labs( x = "Altitude (m)",
              y = "Número de Espécies (Log10)")+
        theme_minimal()
REW <- lm(ALWAVE~WAVEAL, data = dados)
RES <- lm(ALSPLE~SPLEAL, data = dados)
RE <- lm(ALWAV2E~WAV2EAL, data = dados)
R1 <- resid(REW)
R2 <- resid(RES)
R3 <- resid(RE)

plot(fitted(REW), R1, 
     pch = 20, 
     bty = "n",
     col = "blue",
     xlab = "Resíduos",
     ylab = "Frequência")
points(fitted(RES), R2, 
       pch = 20, 
       bty = "n",
       col = "Red")
points(fitted(RE), R3, 
       pch = 20, 
       bty = "n",
       col = "green")
abline(0,0)

H1 <- hist(R1, breaks = 25)
H2 <- hist(R2, breaks = 25)
H3 <- hist(R3, breaks = 25)

plot(H1, col=rgb(0,0,1,1/2),
     main = " ",
     xlab = "Resíduos",
     ylab = "Frequência",
     freq = TRUE,
     ylim = c(0,100),
     #xlim=c(-25.5,-19.5))
)
plot(H2, col=rgb(1,0,0,1/2), freq = TRUE, add=T) 
plot(H3, col=rgb(0,1,0,1/2), freq = TRUE, add=T)


#Area

ggplot(AR, aes(x = FR, y = R, color = L1))+
        geom_point()+
        scale_colour_manual(values = c("blue", "red", "green")) +
        stat_smooth(method = "lm",
                    se = FALSE,
                    fullrange = TRUE,
                    size = 1)+
        labs( x ="Área (Log10(Km^2))",
              y =  "Número de Registros (Log10)")+
        theme_minimal()

REW <- lm(ARWAVR~WAVRAR, data = dados)
RES <- lm(ARSPLR~SPLRAR, data = dados)
RE <- lm(ARWAV2R~WAV2RAR, data = dados)
R1 <- resid(REW)
R2 <- resid(RES)
R3 <- resid(RE)

plot(fitted(REW), R1, 
     pch = 20, 
     bty = "n",
     col = "blue",
     xlab = "Resíduos",
     ylab = "Frequência")
points(fitted(RES), R2, 
       pch = 20, 
       bty = "n",
       col = "Red")
points(fitted(RE), R3, 
       pch = 20, 
       bty = "n",
       col = "green")
abline(0,0)

H1 <- hist(R1, breaks = 25)
H2 <- hist(R2, breaks = 25)
H3 <- hist(R3, breaks = 25)

plot(H1, col=rgb(0,0,1,1/2),
     main = " ",
     xlab = "Resíduos",
     ylab = "Frequência",
     freq = TRUE,
     ylim = c(0,100),
     #xlim=c(-25.5,-19.5))
)
plot(H2, col=rgb(1,0,0,1/2), freq = TRUE, add=T) 
plot(H3, col=rgb(0,1,0,1/2), freq = TRUE, add=T)



ggplot(AR, aes(x = FE, y = E, color = L2))+
        geom_point()+
        scale_colour_manual(values = c("blue", "red", "green")) +
        stat_smooth(method = "lm",
                    se = FALSE,
                    fullrange = TRUE,
                    size = 1)+
        labs( x = "Área (Log10(Km^2))",
              y = "Número de Espécies (Log10)")+
        theme_minimal()

REW <- lm(ARWAVE~WAVEAR, data = dados)
RES <- lm(ARSPLE~SPLEAR, data = dados)
RE <- lm(ARWAV2E~WAVERAR, data = dados)
R1 <- resid(REW)
R2 <- resid(RES)
R3 <- resid(RE)

plot(fitted(REW), R1, 
     pch = 20, 
     bty = "n",
     col = "blue",
     xlab = "Resíduos",
     ylab = "Frequência")
points(fitted(RES), R2, 
       pch = 20, 
       bty = "n",
       col = "Red")
points(fitted(RE), R3, 
       pch = 20, 
       bty = "n",
       col = "green")
abline(0,0)

H1 <- hist(R1, breaks = 25)
H2 <- hist(R2, breaks = 25)
H3 <- hist(R3, breaks = 25)

plot(H1, col=rgb(0,0,1,1/2),
     main = " ",
     xlab = "Resíduos",
     ylab = "Frequência",
     freq = TRUE,
     ylim = c(0,100),
     #xlim=c(-25.5,-19.5))
)
plot(H2, col=rgb(1,0,0,1/2), freq = TRUE, add=T) 
plot(H3, col=rgb(0,1,0,1/2), freq = TRUE, add=T)

#População

ggplot(POP, aes(x = FR, y = R, color = L1))+
        geom_point()+
        scale_colour_manual(values = c("blue", "red", "green")) +
        stat_smooth(method = "lm",
                    se = FALSE,
                    fullrange = TRUE,
                    size = 1)+
        labs( x ="População (Log10)",
              y =  "Número de Registros (Log10)")+
        theme_minimal()

REW <- lm(PWAVR~WAVRP, data = dados)
RES <- lm(PSPLR~SPLRP, data = dados)
RE <- lm(PWAV2R~WAV2RP, data = dados)
R1 <- resid(REW)
R2 <- resid(RES)
R3 <- resid(RE)

plot(fitted(REW), R1, 
     pch = 20, 
     bty = "n",
     col = "blue",
     xlab = "Resíduos",
     ylab = "Frequência")
points(fitted(RES), R2, 
       pch = 20, 
       bty = "n",
       col = "Red")
points(fitted(RE), R3, 
       pch = 20, 
       bty = "n",
       col = "green")
abline(0,0)

H1 <- hist(R1, breaks = 25)
H2 <- hist(R2, breaks = 25)
H3 <- hist(R3, breaks = 25)

plot(H1, col=rgb(0,0,1,1/2),
     main = " ",
     xlab = "Resíduos",
     ylab = "Frequência",
     freq = TRUE,
     ylim = c(0,100),
     #xlim=c(-25.5,-19.5))
)
plot(H2, col=rgb(1,0,0,1/2), freq = TRUE, add=T) 
plot(H3, col=rgb(0,1,0,1/2), freq = TRUE, add=T)

ggplot(POP, aes(x = FE, y = E, color = L2))+
        geom_point()+
        scale_colour_manual(values = c("blue", "red", "green")) +
        stat_smooth(method = "lm",
                    se = FALSE,
                    fullrange = TRUE,
                    size = 1)+
        labs( x = "População (Log10)",
              y = "Número de Espécies (Log10)")+
        theme_minimal()

REW <- lm(PWAVE~WAVEP, data = dados)
RES <- lm(PSPLE~SPLEP, data = dados)
RE <- lm(PWAV2E~WAV2EP, data = dados)
R1 <- resid(REW)
R2 <- resid(RES)
R3 <- resid(RE)

plot(fitted(REW), R1, 
     pch = 20, 
     bty = "n",
     col = "blue",
     xlab = "Resíduos",
     ylab = "Frequência")
points(fitted(RES), R2, 
       pch = 20, 
       bty = "n",
       col = "Red")
points(fitted(RE), R3, 
       pch = 20, 
       bty = "n",
       col = "green")
abline(0,0)

H1 <- hist(R1, breaks = 25)
H2 <- hist(R2, breaks = 25)
H3 <- hist(R3, breaks = 25)

plot(H1, col=rgb(0,0,1,1/2),
     main = " ",
     xlab = "Resíduos",
     ylab = "Frequência",
     freq = TRUE,
     ylim = c(0,100),
     #xlim=c(-25.5,-19.5))
)
plot(H2, col=rgb(1,0,0,1/2), freq = TRUE, add=T) 
plot(H3, col=rgb(0,1,0,1/2), freq = TRUE, add=T)

#Latitude

ggplot(LA, aes(x = FR, y = R, color = L1))+
        geom_point()+
        scale_colour_manual(values = c("blue", "red", "green")) +
        stat_smooth(method = "lm",
                    se = FALSE,
                    fullrange = TRUE,
                    size = 1)+
        labs( x ="Latitude (º)",
              y =  "Número de Registros (Log10)")+
        theme_minimal()

REW <- lm(LAWAVR~WAVRLA, data = dados)
RES <- lm(LASPLR~SPLRLA, data = dados)
RE <- lm(LAWAV2R~WAV2RLA, data = dados)
R1 <- resid(REW)
R2 <- resid(RES)
R3 <- resid(RE)

plot(fitted(REW), R1, 
     pch = 20, 
     bty = "n",
     col = "blue",
     xlab = "Resíduos",
     ylab = "Frequência")
points(fitted(RES), R2, 
       pch = 20, 
       bty = "n",
       col = "Red")
points(fitted(RE), R3, 
       pch = 20, 
       bty = "n",
       col = "green")
abline(0,0)

H1 <- hist(R1, breaks = 25)
H2 <- hist(R2, breaks = 25)
H3 <- hist(R3, breaks = 25)

plot(H1, col=rgb(0,0,1,1/2),
     main = " ",
     xlab = "Resíduos",
     ylab = "Frequência",
     freq = TRUE,
     ylim = c(0,100),
     #xlim=c(-25.5,-19.5))
)
plot(H2, col=rgb(1,0,0,1/2), freq = TRUE, add=T) 
plot(H3, col=rgb(0,1,0,1/2), freq = TRUE, add=T)


ggplot(LA, aes(x = FE, y = E, color = L2))+
        geom_point()+
        scale_colour_manual(values = c("blue", "red", "green")) +
        stat_smooth(method = "lm",
                    se = FALSE,
                    fullrange = TRUE,
                    size = 1)+
        labs( x ="Latitude (º)",
              y = "Número de Espécies (Log10)")+
        theme_minimal()

REW <- lm(LAWAVE~WAVELA, data = dados)
RES <- lm(LASPLE~SPLELA, data = dados)
RE <- lm(LAWAV2E~WAV2ELA, data = dados)
R1 <- resid(REW)
R2 <- resid(RES)
R3 <- resid(RE)

plot(fitted(REW), R1, 
     pch = 20, 
     bty = "n",
     col = "blue",
     xlab = "Resíduos",
     ylab = "Frequência")
points(fitted(RES), R2, 
       pch = 20, 
       bty = "n",
       col = "Red")
points(fitted(RE), R3, 
       pch = 20, 
       bty = "n",
       col = "green")
abline(0,0)

H1 <- hist(R1, breaks = 25)
H2 <- hist(R2, breaks = 25)
H3 <- hist(R3, breaks = 25)

plot(H1, col=rgb(0,0,1,1/2),
     main = " ",
     xlab = "Resíduos",
     ylab = "Frequência",
     freq = TRUE,
     ylim = c(0,100),
     #xlim=c(-25.5,-19.5))
)
plot(H2, col=rgb(1,0,0,1/2), freq = TRUE, add=T) 
plot(H3, col=rgb(0,1,0,1/2), freq = TRUE, add=T)

#Longitude


ggplot(LO, aes(x = FR, y = R, color = L1))+
        geom_point()+
        scale_colour_manual(values = c("blue", "red", "green")) +
        stat_smooth(method = "lm",
                    se = FALSE,
                    fullrange = TRUE,
                    size = 1)+
        labs( x ="Longitude (º)",
              y =  "Número de Registros (Log10)")+
        theme_minimal()

REW <- lm(LOWAVR~WAVRLO, data = dados)
RES <- lm(LOSPLR~SPLRLO, data = dados)
RE <- lm(LOWAV2R~WAV2RLO, data = dados)
R1 <- resid(REW)
R2 <- resid(RES)
R3 <- resid(RE)

plot(fitted(REW), R1, 
     pch = 20, 
     bty = "n",
     col = "blue",
     xlab = "Resíduos",
     ylab = "Frequência")
points(fitted(RES), R2, 
       pch = 20, 
       bty = "n",
       col = "Red")
points(fitted(RE), R3, 
       pch = 20, 
       bty = "n",
       col = "green")
abline(0,0)

H1 <- hist(R1, breaks = 25)
H2 <- hist(R2, breaks = 25)
H3 <- hist(R3, breaks = 25)

plot(H1, col=rgb(0,0,1,1/2),
     main = " ",
     xlab = "Resíduos",
     ylab = "Frequência",
     freq = TRUE,
     ylim = c(0,100),
     #xlim=c(-25.5,-19.5))
)
plot(H2, col=rgb(1,0,0,1/2), freq = TRUE, add=T) 
plot(H3, col=rgb(0,1,0,1/2), freq = TRUE, add=T)


ggplot(LO, aes(x = FE, y = E, color = L2))+
        geom_point()+
        scale_colour_manual(values = c("blue", "red", "green")) +
        stat_smooth(method = "lm",
                    se = FALSE,
                    fullrange = TRUE,
                    size = 1)+
        labs( x ="Longitude (º)",
              y = "Número de Espécies (Log10)")+
        theme_minimal()

REW <- lm(LOWAVE~WAVELO, data = dados)
RES <- lm(LOSPLE~SPLELO, data = dados)
RE <- lm(LOWAV2E~WAV2ELO, data = dados)
R1 <- resid(REW)
R2 <- resid(RES)
R3 <- resid(RE)

plot(fitted(REW), R1, 
     pch = 20, 
     bty = "n",
     col = "blue",
     xlab = "Resíduos",
     ylab = "Frequência") 
points(fitted(RES), R2,  
       pch = 20, 
       bty = "n",
       col = "Red")
points(fitted(RE), R3, 
       pch = 20, 
       bty = "n",
       col = "green")
abline(0,0)


# plot(fitted(REW), R1, 
 #    pch = 20, 
  #   bty = "n",
   #  col = "blue") + abline(0,0)
# plot(fitted(RES), R2,  
 #      pch = 20, 
  #     bty = "n",
   #    col = "Red") + abline(0,0)
# plot(fitted(RE), R3, 
 #      pch = 20, 
  #     bty = "n",
   #    col = "green") + abline(0,0)



H1 <- hist(R1, breaks = 20)
H2 <- hist(R2, breaks = 20)
H3 <- hist(R3, breaks = 20)

plot(H1, col=rgb(0,0,1,1/2),
     main = " ",
     xlab = "Resíduos",
     ylab = "Frequência",
     freq = TRUE,
     ylim = c(0,100),
     #xlim=c(-25.5,-19.5))
)
plot(H2, col=rgb(1,0,0,1/2), freq = TRUE, add=T) 
plot(H3, col=rgb(0,1,0,1/2), freq = TRUE, add=T)





##Seção 4

##Graficos com Outliers Bivariados:

ggplot(WS, aes(SPL2R,WAV2R ))+
        geom_point()+
        stat_smooth(method = "lm",
                    se = FALSE,
                    fullrange = TRUE,
                    size = 1,
                    colour = "red")+
        labs( x ="Número de Registros SpeciesLink (Log10)",
              y = "Número de Registros Wikiaves (Log10)")+
        theme_minimal()

ggplot(WS, aes(SPL2E,WAV2E ))+
        geom_point()+
        stat_smooth(method = "lm",
                    se = FALSE,
                    fullrange = TRUE,
                    size = 1,
                    colour = "red")+
        labs( x ="Número de Espécies SpeciesLink (Log10)",
              y = "Número de Espécies Wikiaves (Log10)")+
        theme_minimal()


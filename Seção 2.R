#Importando bibliotecas e dados

library("openxlsx") 
library(ggplot2)
library(RColorBrewer)

FE <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/R/S2/FE.xlsx")
dados <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/R/S2/Dados.xlsx")

#Histogramas individuais:

WAL <- hist(FE$WAVAL, 
            plot = FALSE, 
            breaks = c(0,72,144,216,288,360,432,504,576,648,720,792,864,936,1008,1080,1152,1224,1296,1368,1440,1512,1584,1656,1782,1800))
WAL$density = WAL$counts/sum(WAL$counts)*100

SAL <- hist(FE$SPLAL, 
            plot = FALSE, 
            breaks = c(0,72,144,216,288,360,432,504,576,648,720,792,864,936,1008,1080,1152,1224,1296,1368,1440,1512,1584,1656,1782,1800))
SAL$density = SAL$counts/sum(SAL$counts)*100

WAR <- hist(FE$WAVAR, 
            plot = FALSE, 
            breaks = c(0,0.16,0.32,0.48,0.64,0.8,0.96,1.12,1.28,1.44,1.60,1.76,1.92,2.08,2.24,2.40,2.56,2.72,2.88,3.04,3.20,3.36,3.52,3.68,3.84,4.00))
WAR$density = WAR$counts/sum(WAR$counts)*100

SAR <- hist(FE$SPLAR, 
            plot = FALSE, 
            breaks = c(0,0.16,0.32,0.48,0.64,0.8,0.96,1.12,1.28,1.44,1.60,1.76,1.92,2.08,2.24,2.40,2.56,2.72,2.88,3.04,3.20,3.36,3.52,3.68,3.84,4.00))
SAR$density = SAR$counts/sum(SAR$counts)*100

WPOP <- hist(FE$WAVPOP, 
             plot = FALSE, 
             breaks = c(2,2.22,2.44,2.66,2.88,3.1,3.32,3.54,3.76,3.98,4.2,4.42,4.64,4.86,5.08,5.3,5.52,5.74,5.96,6.18,6.4,6.62,6.84,7.06,7.28,7.5))
WPOP$density = WPOP$counts/sum(WPOP$counts)*100

SPOP <- hist(FE$SPLPOP, 
             plot = FALSE, 
             breaks = c(2,2.22,2.44,2.66,2.88,3.1,3.32,3.54,3.76,3.98,4.2,4.42,4.64,4.86,5.08,5.3,5.52,5.74,5.96,6.18,6.4,6.62,6.84,7.06,7.28,7.5))
SPOP$density = SPOP$counts/sum(SPOP$counts)*100

WLA <- hist(FE$WAVLA, 
            plot = FALSE, 
            breaks = c(-25.5,-25.26,-25.02,-24.78,-24.54,-24.3,-24.06,-23.82,-23.58,-23.34,-23.1,-22.86,-22.62,-22.38,-22.14,-21.9,-21.66,-21.42,-21.18,-20.94,-20.7,-20.46,-20.22,-19.98,-19.74,-19,5))
WLA$density = WLA$counts/sum(WLA$counts)*100

SLA <- hist(FE$SPLLA, 
            plot = FALSE, 
            breaks = c(-25.5,-25.26,-25.02,-24.78,-24.54,-24.3,-24.06,-23.82,-23.58,-23.34,-23.1,-22.86,-22.62,-22.38,-22.14,-21.9,-21.66,-21.42,-21.18,-20.94,-20.7,-20.46,-20.22,-19.98,-19.74,-19,5))
SLA$density = SLA$counts/sum(SLA$counts)*100

WLO <- hist(FE$WAVLO, 
            plot = FALSE, 
            breaks = c(-55,-54.48,-53.96,-53.44,-52.92,-52.4,-51.88,-51.36,-50.84,-50.32,-49.8,-49.28,-48.76,-48.24,-47.72,-47.2,-46.68,-46.16,-45.64,-45.12,-44.6,-44.08,-43.56,-43.04,-42.52,-42))
WLO$density = WLO$counts/sum(WLO$counts)*100

SLO <- hist(FE$SPLLO, 
            plot = FALSE, 
            breaks = c(-55,-54.48,-53.96,-53.44,-52.92,-52.4,-51.88,-51.36,-50.84,-50.32,-49.8,-49.28,-48.76,-48.24,-47.72,-47.2,-46.68,-46.16,-45.64,-45.12,-44.6,-44.08,-43.56,-43.04,-42.52,-42))
SLO$density = SLO$counts/sum(SLO$counts)*100

## Representação 1

### Altitude


plot(WAL, col=rgb(0,0,1,1/2),
     main = " ",
     xlab = "Altitude (m)",
     ylab = "Frequência Relativa (%)",
     freq = FALSE,
     #ylim = c(0,1),
     xlim=c(0,1800))
plot(SAL, col=rgb(1,0,0,1/2), freq = FALSE, add=T) 
legend("topright", c("Wikiaves", "SpeciesLink"), col=c(rgb(0,0,1,1/2),col=rgb(1,0,0,1/2) ), lwd=5, box.col = "white")


### Área


plot(WAR, col=rgb(0,0,1,1/2),
     main = " ",
     xlab = "Área (Log10(Km^2))",
     ylab = "Frequência Relativa (%)",
     freq = FALSE,
     ylim = c(0,20),
     xlim=c(0,4))
plot(SAR, col=rgb(1,0,0,1/2), freq = FALSE, add=T) 
legend("topright", c("Wikiaves", "SpeciesLink"), col=c(rgb(0,0,1,1/2),col=rgb(1,0,0,1/2) ), lwd=5, box.col = "white")


### População


plot(WPOP, col=rgb(0,0,1,1/2),
     main = " ",
     xlab = "População (Log10)",
     ylab = "Frequência Relativa (%)",
     freq = FALSE,
     ylim = c(0,20),
     xlim=c(2,7.5))
plot(SPOP, col=rgb(1,0,0,1/2), freq = FALSE, add=T)
legend("topright", c("Wikiaves", "SpeciesLink"), col=c(rgb(0,0,1,1/2),col=rgb(1,0,0,1/2) ), lwd=5, box.col = "white")


### Latitude


plot(WLA, col=rgb(0,0,1,1/2),
     main = " ",
     xlab = "Latitude (°)",
     ylab = "Frequência Relativa (%)",
     freq = FALSE,
     ylim = c(0,15),
     xlim=c(-25.5,-19.5))
plot(SLA, col=rgb(1,0,0,1/2), freq = FALSE, add=T)
legend("topright", c("Wikiaves", "SpeciesLink"), col=c(rgb(0,0,1,1/2),col=rgb(1,0,0,1/2) ), lwd=5, box.col = "white")


### Longitude


plot(WLO, col=rgb(0,0,1,1/2),
     main = " ",
     xlab = "Longitude (°)",
     ylab = "Frequência Relativa (%)",
     freq = FALSE,
     ylim = c(0,20),
     xlim=c(-55,-42))
plot(SLO, col=rgb(1,0,0,1/2), freq = FALSE, add=T)
legend("topright", c("Wikiaves", "SpeciesLink"), col=c(rgb(0,0,1,1/2),col=rgb(1,0,0,1/2) ), lwd=5, box.col = "white")


## Representação 2

### Altitude


dados$L <- factor(dados$L, levels = c("WAV", "SPL"), labels = c("WAV (N = 631)", "SLI (N = 174)"))

ggplot(dados, aes(x = Altitude, fill = L, colour = L))+
  scale_colour_manual(values = c("blue", "red")) +
  scale_fill_manual(values = c("blue", "red")) +
  geom_density(alpha = 0.2)+
  labs(x = "Altitude (m)",
       y = "Densidade",
       colour  = "",
       fill = "")+
  theme_classic()


### Área


ggplot(dados, aes(x = Área, fill = L, colour = L))+
  scale_colour_manual(values = c("blue", "red")) +
  scale_fill_manual(values = c("blue", "red")) +
  geom_density(alpha = 0.2)+
  labs(x = "Área (Log10(km^2))",
       y = "Densidade",
       colour  = "",
       fill = "")+
  theme_classic()


### População


ggplot(dados, aes(x = População, fill = L, colour = L))+
  scale_colour_manual(values = c("blue", "red")) +
  scale_fill_manual(values = c("blue", "red")) +
  geom_density(alpha = 0.2)+
  labs(x = "População (log10)",
       y = "Densidade",
       colour  = "",
       fill = "")+
  theme_classic()


### Latitude


ggplot(dados, aes(x = Latitude, fill = L, colour = L))+
  scale_colour_manual(values = c("blue", "red")) +
  scale_fill_manual(values = c("blue", "red")) +
  geom_density(alpha = 0.2)+
  labs(x = "Latitude (º)",
       y = "Densidade",
       colour  = "",
       fill = "")+
  theme_classic()


### Longitude


ggplot(dados, aes(x = Longitude, fill = L, colour = L))+
  scale_colour_manual(values = c("blue", "red")) +
  scale_fill_manual(values = c("blue", "red")) +
  geom_density(alpha = 0.2)+
  labs(x = "Longitude (º)",
       y = "Densidade",
       colour  = "",
       fill = "")+
  theme_classic()



## Representação 3

### Altitude


y1 <- WAL$density
x1 <- c()
x1[1] <- 36
for (i in 1:24){
  x1[i+1] = x1[i]+72
}

y2 <- SAL$density
x2 <- c()
x2[1] <- 36
for (i in 1:24){
  x2[i+1] = x2[i]+72
}

A <- c()

for (i in 1:25){
  A[i] = "WAV (N = 631)"
}
for (i in 26:50){
  A[i] = "SPL (N = 174)"
}

B <- c()

for (i in 1:25){
  B[i] = x1[i]
}

for(i in 26:50){
  B[i] = x2[i-25]
}

C<- c()

for (i in 1:25){
  C[i] = y1[i]
}

for(i in 26:50){
  C[i] = y2[i-25]
}

dados <- data.frame(Legenda = A, x = B, y = C)
dados$Legenda <- factor(dados$Legenda, levels = c("WAV (N = 631)", "SPL (N = 174)"))

ggplot(dados, aes(x = x, y = C, group = Legenda, colour = Legenda))+
  scale_colour_manual(values = c("blue", "red")) +
  stat_smooth(method = lm, formula = y ~ poly(x, 15),se = FALSE, fullrange = TRUE)+
  labs(x = "Altitude (m)",
       y = "Frequência Relativa %",
       colour = " ")+
  theme_classic()


### Área


y1 <- WAR$density
x1 <- c()
x1[1] <- 0.08
for (i in 1:24){
  x1[i+1] = x1[i]+0.16
}

y2 <- SAR$density
x2 <- c()
x2[1] <- 0.08
for (i in 1:24){
  x2[i+1] = x2[i]+0.16
}

A <- c()

for (i in 1:25){
  A[i] = "WAV (N = 631)"
}
for (i in 26:50){
  A[i] = "SPL (N = 174)"
}

B <- c()

for (i in 1:25){
  B[i] = x1[i]
}

for(i in 26:50){
  B[i] = x2[i-25]
}

C<- c()

for (i in 1:25){
  C[i] = y1[i]
}

for(i in 26:50){
  C[i] = y2[i-25]
}

dados <- data.frame(Legenda = A, x = B, y = C)
dados$Legenda <- factor(dados$Legenda, levels = c("WAV (N = 631)", "SPL (N = 174)"))

ggplot(dados, aes(x = x, y = C, group = Legenda, colour = Legenda))+
  scale_colour_manual(values = c("blue", "red")) +
  stat_smooth(method = lm, formula = y ~ poly(x, 15),se = FALSE, fullrange = TRUE)+
  labs(x = "Área (Log10(km^2))",
       y = "Frequência Relativa (%)",
       colour = " ")+
  theme_classic()


### População


y1 <- WPOP$density
x1 <- c()
x1[1] <- 2.11
for (i in 1:24){
  x1[i+1] = x1[i]+0.22
}

y2 <- SPOP$density
x2 <- c()
x2[1] <- 2.11
for (i in 1:24){
  x2[i+1] = x2[i]+0.22
}

A <- c()

for (i in 1:25){
  A[i] = "WAV (N = 631)"
}
for (i in 26:50){
  A[i] = "SPL (N = 174)"
}

B <- c()

for (i in 1:25){
  B[i] = x1[i]
}

for(i in 26:50){
  B[i] = x2[i-25]
}

C<- c()

for (i in 1:25){
  C[i] = y1[i]
}

for(i in 26:50){
  C[i] = y2[i-25]
}

dados <- data.frame(Legenda = A, x = B, y = C)
dados$Legenda <- factor(dados$Legenda, levels = c("WAV (N = 631)", "SPL (N = 174)"))

ggplot(dados, aes(x = x, y = C, group = Legenda, colour = Legenda))+
  scale_colour_manual(values = c("blue", "red")) +
  stat_smooth(method = lm, formula = y ~ poly(x, 15),se = FALSE, fullrange = TRUE)+
  labs(x = "Número de habitantes (Log10)",
       y = "Frequência Relativa (%)",
       colour = " ")+
  theme_classic()


### Latitude


y1 <- WLA$density
x1 <- c()
x1[1] <- -25.38
for (i in 1:24){
  x1[i+1] = x1[i]+0.24
}

y2 <- SLA$density
x2 <- c()
x2[1] <- -25.38
for (i in 1:24){
  x2[i+1] = x2[i]+0.24
}

A <- c()

for (i in 1:25){
  A[i] = "WAV (N = 631)"
}
for (i in 26:50){
  A[i] = "SPL (N = 174)"
}

B <- c()

for (i in 1:25){
  B[i] = x1[i]
}

for(i in 26:50){
  B[i] = x2[i-25]
}

C<- c()

for (i in 1:25){
  C[i] = y1[i]
}

for(i in 26:50){
  C[i] = y2[i-25]
}

dados <- data.frame(Legenda = A, x = B, y = C)
dados$Legenda <- factor(dados$Legenda, levels = c("WAV (N = 631)", "SPL (N = 174)"))

ggplot(dados, aes(x = x, y = C, group = Legenda, colour = Legenda))+
  scale_colour_manual(values = c("blue", "red")) +
  stat_smooth(method = lm, formula = y ~ poly(x, 15),se = FALSE, fullrange = TRUE)+
  labs(x = "Latitude (º)",
       y = "Frequência Relativa (%)",
       colour = " ")+
  theme_classic()


### Longitude


y1 <- WLO$density
x1 <- c()
x1[1] <- -54.64
for (i in 1:24){
  x1[i+1] = x1[i]+0.72
}

y2 <- SLO$density
x2 <- c()
x2[1] <- -54.64
for (i in 1:24){
  x2[i+1] = x2[i]+0.72
}

A <- c()

for (i in 1:25){
  A[i] = "WAV (N = 631)"
}
for (i in 26:50){
  A[i] = "SPL (N = 174)"
}

B <- c()

for (i in 1:25){
  B[i] = x1[i]
}

for(i in 26:50){
  B[i] = x2[i-25]
}

C<- c()

for (i in 1:25){
  C[i] = y1[i]
}

for(i in 26:50){
  C[i] = y2[i-25]
}

dados <- data.frame(Legenda = A, x = B, y = C)
dados$Legenda <- factor(dados$Legenda, levels = c("WAV (N = 631)", "SPL (N = 174)"))

ggplot(dados, aes(x = x, y = C, group = Legenda, colour = Legenda))+
  scale_colour_manual(values = c("blue", "red")) +
  stat_smooth(method = lm, formula = y ~ poly(x, 15),se = FALSE)+
  labs(x = "Longitude (º)",
       y = "Frequência Relativa (%)",
       colour = " ")+
  theme_classic()

rm(list=ls())
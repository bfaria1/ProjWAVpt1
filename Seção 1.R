#Importando bibliotecas e dados

library("openxlsx") 
library(ggplot2)
library(RColorBrewer)
library(esquisse)
library(usethis)
library(moments)

WS <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/R/S1/WAVxSPL.xlsx") 
WAV <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/R/S1/WAV.xlsx") 
SPL <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/R/S1/SPL.xlsx")
ESP <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/R/S1/Espécies.xlsx")
dados <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/R/S1/dados.xlsx")

skewness(WAV$WAVR)
kurtosis(WAV$WAVR)

skewness(SPL$SPLR)
kurtosis(SPL$SPLR)

skewness(WS$WAV2R)
kurtosis(WS$WAV2R)

skewness(WAV$WAVE)
kurtosis(WAV$WAVE)

skewness(SPL$SPLE)
kurtosis(SPL$SPLE)

skewness(WS$WAV2E)
kurtosis(WS$WAV2E)

A1 <- hist(WAV$WAVR, 
           plot = FALSE, 
           breaks = c(0,0.2,0.4,0.6,0.8,1.0,1.2,1.4,1.6,1.8,2.0,2.2,2.4,2.6,2.8,3.0,3.2,3.4,3.6,3.8,4.0,4.2,4.4,4.6,4.8,5.0))
A1$density = A1$counts/sum(A1$counts)*100

B1 <- hist(SPL$SPLR, 
           plot = FALSE, 
           breaks = c(0,0.2,0.4,0.6,0.8,1.0,1.2,1.4,1.6,1.8,2.0,2.2,2.4,2.6,2.8,3.0,3.2,3.4,3.6,3.8,4.0,4.2,4.4,4.6,4.8,5.0))
B1$density = B1$counts/sum(B1$counts)*100

C1 <- hist (WS$WAV2R, 
            plot = FALSE, 
            breaks = c(0,0.2,0.4,0.6,0.8,1.0,1.2,1.4,1.6,1.8,2.0,2.2,2.4,2.6,2.8,3.0,3.2,3.4,3.6,3.8,4.0,4.2,4.4,4.6,4.8,5.0))
C1$density = C1$counts/sum(C1$counts)*100

A2 <- hist(WAV$WAVE, 
           plot = FALSE, 
           breaks = c(0,0.12,0.24,0.36,0.48,0.60,0.72,0.84,0.96,1.08,1.20,1.32,1.44,1.56,1.68,1.80,1.92,2.04,2.16,2.28,2.40,2.52,2.64,2.76,2.88,3.00)) 
A2$density = A2$counts/sum(A2$counts)*100

B2 <- hist(SPL$SPLE, 
           plot = FALSE, 
           breaks = c(0,0.12,0.24,0.36,0.48,0.60,0.72,0.84,0.96,1.08,1.20,1.32,1.44,1.56,1.68,1.80,1.92,2.04,2.16,2.28,2.40,2.52,2.64,2.76,2.88,3.00))
B2$density = B2$counts/sum(B2$counts)*100

C2 <- hist(WS$WAV2E, 
           plot = FALSE,
           breaks = c(0,0.12,0.24,0.36,0.48,0.60,0.72,0.84,0.96,1.08,1.20,1.32,1.44,1.56,1.68,1.80,1.92,2.04,2.16,2.28,2.40,2.52,2.64,2.76,2.88,3.00)) 
C2$density = C2$counts/sum(C2$counts)*100

A3 <- hist(ESP$WAV, 
           plot = FALSE, 
           breaks = c(0,0.16,0.32,0.48,0.64,0.8,0.96,1.12,1.28,1.44,1.60,1.76,1.92,2.08,2.24,2.40,2.56,2.72,2.88,3.04,3.20,3.36,3.52,3.68,3.84,4.00))
A3$density = A3$counts/sum(A3$counts)*100

B3 <- hist(ESP$SPL, 
           plot = FALSE, 
           breaks = c(0,0.16,0.32,0.48,0.64,0.8,0.96,1.12,1.28,1.44,1.60,1.76,1.92,2.08,2.24,2.40,2.56,2.72,2.88,3.04,3.20,3.36,3.52,3.68,3.84,4.00))
B3$density = B3$counts/sum(B3$counts)*100

C3 <- hist(ESP$WAV2, 
           plot = FALSE,
           breaks = c(0,0.16,0.32,0.48,0.64,0.8,0.96,1.12,1.28,1.44,1.60,1.76,1.92,2.08,2.24,2.40,2.56,2.72,2.88,3.04,3.20,3.36,3.52,3.68,3.84,4.00))
C3$density = C3$counts/sum(C3$counts)*100

## Representação 1


png("111.png", width = 550, height = 530)

plot(A1, col=rgb(0,0,1,1/2),
     main = " ",
     xlab = "Número de Registros (Log10)",
     ylab = "Frequência Relativa (%)",
     freq = FALSE,
     ylim = c(0,20),
     xlim=c(0,5))
plot(B1, col=rgb(1,0,0,1/2), freq = FALSE, add=T) 
plot(C1, col=rgb(0,1,0,1/2), freq = FALSE, add=T)
legend("topright", c("WAV (N = 631)", "SLI (N = 174)","WAV 2 (N = 173)"), col=c(rgb(0,0,1,1/2),col=rgb(1,0,0,1/2) ,rgb(0,1,0,1/2) ), lwd=5, box.col = "white")

dev.off()

png("121.png", width = 550, height = 530)

plot(A2, col=rgb(0,0,1,1/2),
     main = " ",
     xlab = "Número de Espécies (Log10)",
     ylab = "Frequência Relativa (%)",
     freq = FALSE,
     ylim = c(0,25),
     xlim=c(0,3))
plot(B2, col=rgb(1,0,0,1/2), freq = FALSE, add=T) 
plot(C2, col=rgb(0,1,0,1/2), freq = FALSE, add=T)
legend("top", c("WAV (N = 631)", "SLI (N = 174)","WAV 2 (N = 173)"), col=c(rgb(0,0,1,1/2),col=rgb(1,0,0,1/2) ,rgb(0,1,0,1/2) ), lwd=5, box.col = "white")

dev.off()

png("131.png", width = 550, height = 530)

plot(A3, col=rgb(0,0,1,1/2),
     main = " ",
     xlab = "Número de Registros por Espécie (Log10)",
     ylab = "Frequência Relativa (%)",
     freq = FALSE,
     ylim = c(0,20),
     xlim=c(0,4))
plot(B3, col=rgb(1,0,0,1/2), freq = FALSE, add=T) 
plot(C3, col=rgb(0,1,0,1/2), freq = FALSE, add=T)
legend("topright", c("WAV (N = 631)", "SLI (N = 174)","WAV 2 (N = 173)"), col=c(rgb(0,0,1,1/2),col=rgb(1,0,0,1/2) ,rgb(0,1,0,1/2) ), lwd=5, box.col = "white")

dev.off()

## Representação 2


dados$L <- factor(dados$L, levels = c("WAV", "SPL", "WAV 2"), labels = c("WAV (N = 631)", "SLI (N = 174)", "WAV 2 (N = 173)"))

png("112.png", width = 550, height = 530)

ggplot(dados, aes(x = R, fill = L, colour = L))+
  scale_colour_manual(values = c("blue", "red", "green")) +
  scale_fill_manual(values = c("blue", "red", "green")) +
  geom_density(alpha = 0.2)+
  labs(x = "Quantidade de Registros (Log10)",
       y = "Densidade",
       colour  = "",
       fill = "")+
  theme_classic()

dev.off()

png("122.png", width = 550, height = 530)

ggplot(dados, aes(x = E, fill = L, colour = L))+
  scale_colour_manual(values = c("blue", "red", "green")) +
  scale_fill_manual(values = c("blue", "red", "green")) +
  geom_density(alpha = 0.2)+
  labs(x = "Quantidade de Espécies (Log10)",
       y = "Densidade",
       colour  = "",
       fill = "")+
  theme_classic()

dev.off()

dados$L2 <- factor(dados$L2, levels = c("WAV", "SPL", "WAV 2"), labels = c("WAV (S = 790)", "SLI (S = 661)", "WAV 2 (S = 779)"))

png("132.png", width = 550, height = 530)

ggplot(dados, aes(x = RE, fill = L2, colour = L2))+
  scale_colour_manual(values = c("blue", "red", "green")) +
  scale_fill_manual(values = c("blue", "red", "green")) +
  geom_density(alpha = 0.2)+
  labs(x = "Quantidade de Registros (Log10)",
       y = "Densidade",
       colour  = "",
       fill = "")+
  theme_classic()

dev.off()

## Representação 3



y1 <- A1$density
x1 <- c()
x1[1] <- 0.1
for (i in 1:24){
  x1[i+1] = x1[i]+0.2
}


y2 <- B1$density
x2 <- c()
x2[1] <- 0.1
for (i in 1:24){
  x2[i+1] = x2[i]+0.2
}


y3 <- C1$density
x3 <- c()
x3[1] <- 0.1
for (i in 1:24){
  x3[i+1] = x3[i]+0.2
}

A <- c()

for (i in 1:25){
  A[i] = "WAV (N = 631)"
}
for (i in 26:50){
  A[i] = "SLI (N = 174)"
}
for (i in 51:75){
  A[i] = "WAV 2 (N = 173)"
}

B <- c()

for (i in 1:25){
  B[i] = x1[i]
}

for(i in 26:50){
  B[i] = x2[i-25]
}
for(i  in 51:75){
  B[i] = x3[i-50]
}

C<- c()

for (i in 1:25){
  C[i] = y1[i]
}

for(i in 26:50){
  C[i] = y2[i-25]
}
for(i  in 51:75){
  C[i] = y3[i-50]
}

dados <- data.frame(L = A, x = B, y = C)
dados$L <- factor(dados$L, levels = c("WAV (N = 631)", "SLI (N = 174)", "WAV 2 (N = 173)"))

png("113.png", width = 550, height = 530)

ggplot(dados, aes(x = x, y = C, group = L, colour = L))+
  geom_line()+
  scale_colour_manual(values = c("blue", "red", "green")) +
  labs(x = "Número de Registros (Log10)",
       y = "Frequência Relativa (%)",
       colour = " ")+
  theme_classic()

dev.off()


y1 <- A2$density
x1 <- c()
x1[1] <- 0.1
for (i in 1:24){
  x1[i+1] = x1[i]+0.12
}

y2 <- B2$density
x2 <- c()
x2[1] <- 0.1
for (i in 1:24){
  x2[i+1] = x2[i]+0.12
}

y3 <- C2$density
x3 <- c()
x3[1] <- 0.1
for (i in 1:24){
  x3[i+1] = x3[i]+0.12
}

A <- c()

for (i in 1:25){
  A[i] = "WAV (N = 631)"
}
for (i in 26:50){
  A[i] = "SLI (N = 174)"
}
for (i in 51:75){
  A[i] = "WAV 2 (N = 173)"
}

B <- c()

for (i in 1:25){
  B[i] = x1[i]
}

for(i in 26:50){
  B[i] = x2[i-25]
}
for(i  in 51:75){
  B[i] = x3[i-50]
}

C<- c()

for (i in 1:25){
  C[i] = y1[i]
}

for(i in 26:50){
  C[i] = y2[i-25]
}
for(i  in 51:75){
  C[i] = y3[i-50]
}

dados <- data.frame(L = A, x = B, y = C)
dados$L <- factor(dados$L, levels = c("WAV (N = 631)", "SLI (N = 174)", "WAV 2 (N = 173)"))

png("123.png", width = 550, height = 530)

ggplot(dados, aes(x = x, y = C, group = L, colour = L))+
  geom_line()+
  scale_colour_manual(values = c("blue", "red", "green")) +
  labs(x = "Número de Espécies (Log10)",
       y = "Frequência Relativa (%)",
       colour = " ")+
  theme_classic()

dev.off()


y1 <- A3$density
x1 <- c()
x1[1] <- 0.1
for (i in 1:24){
  x1[i+1] = x1[i]+0.16
}

y2 <- B3$density
x2 <- c()
x2[1] <- 0.1
for (i in 1:24){
  x2[i+1] = x2[i]+0.16
}

y3 <- C3$density
x3 <- c()
x3[1] <- 0.1
for (i in 1:24){
  x3[i+1] = x3[i]+0.16
}


A <- c()

for (i in 1:25){
  A[i] = "WAV (S = 790)"
}
for (i in 26:50){
  A[i] = "SLI (S = 661)"
}
for (i in 51:75){
  A[i] = "WAV 2 (S = 779)"
}

B <- c()

for (i in 1:25){
  B[i] = x1[i]
}

for(i in 26:50){
  B[i] = x2[i-25]
}
for(i  in 51:75){
  B[i] = x3[i-50]
}

C<- c()

for (i in 1:25){
  C[i] = y1[i]
}

for(i in 26:50){
  C[i] = y2[i-25]
}
for(i  in 51:75){
  C[i] = y3[i-50]
}

dados <- data.frame(L = A, x = B, y = C)
dados$L <- factor(dados$L, levels = c("WAV (S = 790)", "SLI (S = 661)", "WAV 2 (S = 779)"))

png("133.png", width = 550, height = 530)

ggplot(dados, aes(x = x, y = C, group = L, colour = L))+
  geom_line()+
  scale_colour_manual(values = c("blue", "red", "green")) +
  labs(x = "Número de Registros por Espécie (Log10)",
       y = "Frequência Relativa (%)",
       colour = " ")+
  theme_classic()

dev.off()

rm(list=ls())



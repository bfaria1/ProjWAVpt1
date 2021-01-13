#Importando bibliotecas e dados

library("openxlsx") 
library(ggplot2)
library(RColorBrewer)
library(esquisse)

WS <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/R/S1/WAVxSPL.xlsx") 
WAV <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/R/S1/WAV.xlsx") 
SPL <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/R/S1/SPL.xlsx")
ESP <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/R/S1/Espécies.xlsx")
dados <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/R/S1/dados.xlsx")

#Histogramas individuais:

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

#Registros por Muicipio:

plot(A1, col=rgb(0,0,1,1/2),
     main = " ",
     xlab = "Número de Registros (Log10)",
     ylab = "Frequência Relativa (%)",
     freq = FALSE,
     ylim = c(0,20),
     xlim=c(0,5))
plot(B1, col=rgb(1,0,0,1/2), freq = FALSE, add=T) 
plot(C1, col=rgb(0,1,0,1/2), freq = FALSE, add=T)
legend("topright", c("Wikiaves", "SpeciesLink","Wikiaves 2"), col=c(rgb(0,0,1,1/2),col=rgb(1,0,0,1/2) ,rgb(0,1,0,1/2) ), lwd=5, box.col = "white")

#Espécies por Municipio:

plot(A2, col=rgb(0,0,1,1/2),
     main = " ",
     xlab = "Número de Espécies (Log10)",
     ylab = "Frequência Relativa (%)",
     freq = FALSE,
     ylim = c(0,25),
     xlim=c(0,3))
plot(B2, col=rgb(1,0,0,1/2), freq = FALSE, add=T) 
plot(C2, col=rgb(0,1,0,1/2), freq = FALSE, add=T)
legend("top", c("Wikiaves", "SpeciesLink","Wikiaves 2"), col=c(rgb(0,0,1,1/2),col=rgb(1,0,0,1/2) ,rgb(0,1,0,1/2) ), lwd=5, box.col = "white")

#Registros por espécies:

plot(A3, col=rgb(0,0,1,1/2),
     main = " ",
     xlab = "Número de Registros por Espécie (Log10)",
     ylab = "Frequência Relativa (%)",
     freq = FALSE,
     ylim = c(0,20),
     xlim=c(0,4))
plot(B3, col=rgb(1,0,0,1/2), freq = FALSE, add=T) 
plot(C3, col=rgb(0,1,0,1/2), freq = FALSE, add=T)
legend("topright", c("Wikiaves", "SpeciesLink","Wikiaves 2"), col=c(rgb(0,0,1,1/2),col=rgb(1,0,0,1/2) ,rgb(0,1,0,1/2) ), lwd=5, box.col = "white")

#Plots de Desidade

#Registros

ggplot(dados, aes(x = R, fill = L, colour = L))+
  scale_colour_manual(values = c("red", "blue", "green")) +
  scale_fill_manual(values = c("red", "blue", "green")) +
  geom_density(alpha = 0.2)+
  labs(x = "Quantidade de Registros (Log10)",
       y = "Densidade")+
  theme_classic()

#Espécies

ggplot(dados, aes(x = E, fill = L, colour = L))+
  scale_colour_manual(values = c("red", "blue", "green")) +
  scale_fill_manual(values = c("red", "blue", "green")) +
  geom_density(alpha = 0.2)+
  labs(x = "Quantidade de Espécies (Log10)",
       y = "Densidade")+
  theme_classic()

#Registros por Espécies

ggplot(dados, aes(x = RE, fill = L2, colour = L2))+
  scale_colour_manual(values = c("red", "blue", "green")) +
  scale_fill_manual(values = c("red", "blue", "green")) +
  geom_density(alpha = 0.2)+
  labs(x = "Quantidade de Registros (Log10)",
       y = "Densidade")+
  theme_classic()

rm(list=ls())

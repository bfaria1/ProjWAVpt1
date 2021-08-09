# Relatório final

library(pacman)

pacman::p_load(ggthemes, dplyr, car, rstatix, lmtest, ggpubr, QuantPsyc, psych, ggplot2, openxlsx, permute, lattice, vegan,geosphere,tidyverse, cluster, gclus, factoextra, dendextend, sf,geobr, ade4,multcomp, emmeans, RVAideMemoire, DescTools,RColorBrewer, tidyr, hrbrthemes,  FD, ggcorrplot, moments,  usethis)

WS <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S1/WAVxSPL.xlsx") 
WAV <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S1/WAV.xlsx") 
SPL <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S1/SPL.xlsx")
ESP <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S1/Espécies.xlsx")
dados <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S1/dados.xlsx")

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

dados <- data.frame(Li = A, xi = B, yi = C)
dados$L <- factor(dados$L, levels = c("WAV (N = 631)", "SLI (N = 174)", "WAV 2 (N = 173)"))


G1 <- ggplot(dados, aes(x = xi, y = yi, group = Li, colour = Li))+
  geom_line()+
  scale_colour_manual(values = c("blue", "red", "green")) +
  labs(title = "(B)",
       x = "Número de Registros (Log10)",
       y = "Frequência Relativa (%)",
       colour = " ")+
  theme_classic()


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

dados <- data.frame(Lj = A, xj = B, yj = C)
dados$L <- factor(dados$L, levels = c("WAV (N = 631)", "SLI (N = 174)", "WAV 2 (N = 173)"))

G2 <- ggplot(dados, aes(x = xj, y = yj, group = Lj, colour = Lj))+
  geom_line()+
  scale_colour_manual(values = c("blue", "red", "green")) +
  labs(title= "(A)",
       x = "Número de Espécies (Log10)",
       y = "Frequência Relativa (%)",
       colour = " ")+
  theme_classic()


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

dados <- data.frame(Lk = A, xk = B, yk = C)
dados$L <- factor(dados$L, levels = c("WAV (S = 790)", "SLI (S = 661)", "WAV 2 (S = 779)"))

G3 <- ggplot(dados, aes(x = xk, y = yk, group = Lk, colour = Lk))+
  geom_line()+
  scale_colour_manual(values = c("blue", "red", "green")) +
  labs(title = "(C)",
       x = "Número de Registros por Espécie (Log10)",
       y = "Frequência Relativa (%)",
       colour = " ")+
  theme_classic()

ggarrange(G2,G1,G3, ncol = 2, nrow = 2)

rm(list=ls())

# Figura 2

Dados <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S5/Mapas.xlsx")

mc <- read_municipality(code_muni= "SP")
cid <- merge(mc,Dados)

cid$WEI <- cut(cid$WAVE, breaks = c(-1,0,100,200,300,400,Inf), 
               labels=c("0","1 a 100","101 a 200","201 a 300","301 a 400","401+"))

cid$SEI <- cut(cid$SPLE, breaks = c(-1,0,50,100,150,200,Inf),
               labels= c("0","1 a 50","51 a 100","101 a 150","151 a 200","201+"))

cid$WRI <- cut(cid$WAVR, breaks = c(-1,0,5000,10000,15000,20000,25000,30000,35000, Inf), 
               labels=c("0","1 a 5000","5001 a 10000","10001 a 15000","15001 a 20000","20001 a 25000","25001 a 30000", "30001 a 35000","35000+"))

cid$SRI <- cut(cid$SPLR, breaks = c(-1,0,800,1600,2400,3200,4000,4800,5600,Inf),
               labels= c("0","1 a 800","801 a 1600","1601 a 2400","2401 a 3200","3201 a 4000","4001 a 4800","4801 a 5600","5601+"))

cid$LWEI <- cut(cid$LWAVE, breaks = c(-2,-1,0,1.00,1.5,2.0,2.5,Inf),
                labels = c("Sem registro","0.00","0.01 a 1.00","1.01 a 1.50","1.51 a 2.00","2.01 a 2.50","2.51 a 3.00"))

cid$LSEI <- cut(cid$LSPLE, breaks = c(-2,-1,0,1.00,1.5,2.0,2.5,Inf),
                labels = c("Sem registro","0.00","0.01 a 1.00","1.01 a 1.50","1.51 a 2.00","2.01 a 2.50","2.51 a 3.00"))

cid$LWRI <- cut(cid$LWAVR, breaks = c(-2,-1,1.00,1.5,2.0,2.5,3.00,3.5,4,Inf),
                labels = c("Sem registro","0.00 a 1.00","1.01 a 1.50","1.51 a 2.00","2.01 a 2.50","2.51 a 3.00","3.01 a 3.50","3.51 a 4.00","4.01+"))

cid$LSRI <- cut(cid$LSPLR, breaks = c(-2,-1,1.00,1.5,2.0,2.5,3.00,3.5,4,Inf),
                labels = c("Sem registro","0.00 a 1.00","1.01 a 1.50","1.51 a 2.00","2.01 a 2.50","2.51 a 3.00","3.01 a 3.50","3.51 a 4.00","4.01+"))

cid$LW2EI <- cut(cid$LWAV2E, breaks = c(-2,-1,0,1.00,1.5,2.0,2.5,Inf),
                 labels = c("Sem registro","0.00","0.01 a 1.00","1.01 a 1.50","1.51 a 2.00","2.01 a 2.50","2.51 a 3.00"))

cid$LW2RI <- cut(cid$LWAV2R, breaks = c(-2,-1,1.00,1.5,2.0,2.5,3.00,3.5,4,Inf),
                 labels = c("Sem registro","0.00 a 1.00","1.01 a 1.50","1.51 a 2.00","2.01 a 2.50","2.51 a 3.00","3.01 a 3.50","3.51 a 4.00","4.01+"))

A <- ggplot(cid) +
  geom_sf(aes(fill=LWEI), color = NA) +
  labs(subtitle="Wikiaves", size=8)+ 
  scale_fill_manual (values = c('#f0f0f0','#fdd49e','#fdbb84','#feb24c','#fc8d59','#ef6548','#d7301f'))+
  labs(fill = "Espécies (Log10) ")+
  theme_minimal()

B <- ggplot(cid) +
  geom_sf(aes(fill=LW2EI), color = NA) +
  labs(subtitle="Wikiaves 2", size=8)+ 
  scale_fill_manual (values = c('#f0f0f0','#fdd49e','#fdbb84','#feb24c','#fc8d59','#ef6548','#d7301f'))+
  labs(fill = "Espécies (Log10) ")+
  theme_minimal()


C <- ggplot(cid) +
  geom_sf(aes(fill=LSEI), color = NA) +
  labs(subtitle="SpeciesLink ", size=8)+ 
  scale_fill_manual (values = c('#f0f0f0','#fdd49e','#fdbb84','#feb24c','#fc8d59','#ef6548','#d7301f'))+
  labs(fill = "Espécies (Log10) ")+
  theme_minimal()


D <- ggplot(cid) +
  geom_sf(aes(fill=LSRI), color = NA) +
  labs(subtitle="SpeciesLink", size=8)+ 
  scale_fill_manual (values = c('#f0f0f0','#fdd49e','#fdbb84','#feb24c','#fc8d59','#ef6548','#d7301f','#b30000','#7f0000'))+
  labs(fill = "Registros (Log10) ")+
  theme_minimal()

E <- ggplot(cid) +
  geom_sf(aes(fill=LWRI), color = NA) +
  labs(subtitle="Wikiaves", size=8)+
  scale_fill_manual (values = c('#f0f0f0','#fdd49e','#fdbb84','#feb24c','#fc8d59','#ef6548','#d7301f','#b30000','#7f0000'))+
  labs(fill = "Registros (Log10) ")+
  theme_minimal()

G <- ggplot(cid) +
  geom_sf(aes(fill=LW2RI), color = NA) +
  labs(subtitle="Wikiaves 2", size=8)+
  scale_fill_manual (values = c('#f0f0f0','#fdd49e','#fdbb84','#feb24c','#fc8d59','#ef6548','#d7301f','#b30000','#7f0000'))+
  labs(fill = "Registros (Log10) ")+
  theme_minimal()

ggarrange(A,E,C,D,B,G, ncol = 2, nrow = 3)

rm(list=ls())

# Figura 3

dados <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S4/WS.xlsx")
RW <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S4/RW.xlsx")
RS <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S4/RS.xlsx")
RW2 <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S4/RW2.xlsx")

grafico1 <- ggplot(dados, aes(x = A2, y = A1))+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
  expand_limits(x=0,y=0)+
  geom_point()+
  stat_smooth(method = "lm",
              se = FALSE,
              fullrange = TRUE,
              col = "red",
              size = 1)+
  labs( title = "(B)",
        x ="Número de Registros SpeciesLink (Log10)",
        y =  "Número de Registros Wikiaves (Log10)")+
  theme_classic()


grafico2 <- ggplot(dados, aes(x = B2, y = B1))+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
  expand_limits(x=0,y=0)+
  geom_point()+
  stat_smooth(method = "lm",
              se = FALSE,
              fullrange = TRUE,
              col = "red",
              size = 1)+
  labs( title = "(C)",
        x ="Número de Espécies SpeciesLink (Log10)",
        y =  "Número de Espécies Wikiaves (Log10)")+
  theme_classic()




dados$L1 <- factor(dados$L1, levels=c("Wikiaves", "SpeciesLink", "Wikiaves 2"), labels = c("WAV (N = 620)","SLI (N = 143)","WAV 2 (N = 171)"))

grafico3 <- ggplot(dados, aes(x = C1, y = C2, color = L1))+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
  expand_limits(x=0,y=0)+
  geom_point()+
  scale_colour_manual(values = c("blue","red", "green")) +
  stat_smooth(method = "lm",
              formula = y~poly(x,2),
              se = FALSE,
              fullrange = TRUE,
              size = 1)+
  labs( title = "(A)",
        x ="Número de Registros (Log10)",
        y =  "Número de Espécies (Log10)",
        colour = " ")+
  theme_classic()

graf <- ggarrange(grafico1,grafico2, ncol = 2, nrow = 1)

ggarrange(grafico3,graf,  ncol = 1, nrow = 2, legend = "right")

rm(list=ls())

# Figura 4

FE <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S2/FE.xlsx")
dados <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S2/Dados.xlsx")

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

A1 <- c()

for (i in 1:25){
  A1[i] = "WAV (N = 631)"
}
for (i in 26:50){
  A1[i] = "SLI (N = 174)"
}

B1 <- c()

for (i in 1:25){
  B1[i] = x1[i]
}

for(i in 26:50){
  B1[i] = x2[i-25]
}

C1<- c()

for (i in 1:25){
  C1[i] = y1[i]
}

for(i in 26:50){
  C1[i] = y2[i-25]
}

dados1 <- data.frame(Legenda = A1, x = B1, y = C1)
dados1$Legenda <- factor(dados1$Legenda, levels = c("WAV (N = 631)", "SLI (N = 174)"))

A <- ggplot(dados1, aes(x = x, y = C1, group = Legenda, colour = Legenda))+
  geom_line()+
  scale_colour_manual(values = c("blue", "red")) +
  labs(title = "(A)",
       x = "Altitude (m)",
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

A2 <- c()

for (i in 1:25){
  A2[i] = "WAV (N = 631)"
}
for (i in 26:50){
  A2[i] = "SLI (N = 174)"
}

B2 <- c()

for (i in 1:25){
  B2[i] = x1[i]
}

for(i in 26:50){
  B2[i] = x2[i-25]
}

C2<- c()

for (i in 1:25){
  C2[i] = y1[i]
}

for(i in 26:50){
  C2[i] = y2[i-25]
}

dados2 <- data.frame(Legenda = A2, x = B2, y = C2)
dados2$Legenda <- factor(dados2$Legenda, levels = c("WAV (N = 631)", "SLI (N = 174)"))

B <- ggplot(dados2, aes(x = x, y = C2, group = Legenda, colour = Legenda))+
  geom_line()+
  scale_colour_manual(values = c("blue", "red")) +
  labs(title = "(B)",
       x = "Área (Log10(km^2))",
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

A3 <- c()

for (i in 1:25){
  A3[i] = "WAV (N = 631)"
}
for (i in 26:50){
  A3[i] = "SLI (N = 174)"
}

B3 <- c()

for (i in 1:25){
  B3[i] = x1[i]
}

for(i in 26:50){
  B3[i] = x2[i-25]
}

C3<- c()

for (i in 1:25){
  C3[i] = y1[i]
}

for(i in 26:50){
  C3[i] = y2[i-25]
}

dados3 <- data.frame(Legenda = A3, x = B3, y = C3)
dados3$Legenda <- factor(dados3$Legenda, levels = c("WAV (N = 631)", "SLI (N = 174)"))

C <- ggplot(dados3, aes(x = x, y = C3, group = Legenda, colour = Legenda))+
  geom_line()+
  scale_colour_manual(values = c("blue", "red")) +
  labs(title = "(C)",
       x = "Número de habitantes (Log10)",
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

A4 <- c()

for (i in 1:25){
  A4[i] = "WAV (N = 631)"
}
for (i in 26:50){
  A4[i] = "SLI (N = 174)"
}

B4 <- c()

for (i in 1:25){
  B4[i] = x1[i]
}

for(i in 26:50){
  B4[i] = x2[i-25]
}

C4<- c()

for (i in 1:25){
  C4[i] = y1[i]
}

for(i in 26:50){
  C4[i] = y2[i-25]
}

dados4 <- data.frame(Legenda = A4, x = B4, y = C4)
dados4$Legenda <- factor(dados4$Legenda, levels = c("WAV (N = 631)", "SLI (N = 174)"))
D <- ggplot(dados4, aes(x = x, y = C4, group = Legenda, colour = Legenda))+
  geom_line()+
  scale_colour_manual(values = c("blue", "red")) +
  labs(title = "(D)",
       x = "Latitude (º)",
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

A5 <- c()

for (i in 1:25){
  A5[i] = "WAV (N = 631)"
}
for (i in 26:50){
  A5[i] = "SLI (N = 174)"
}

B5 <- c()

for (i in 1:25){
  B5[i] = x1[i]
}

for(i in 26:50){
  B5[i] = x2[i-25]
}

C5<- c()

for (i in 1:25){
  C5[i] = y1[i]
}

for(i in 26:50){
  C5[i] = y2[i-25]
}

dados5 <- data.frame(Legenda = A5, x = B5, y = C5)
dados5$Legenda <- factor(dados5$Legenda, levels = c("WAV (N = 631)", "SLI (N = 174)"))
E <- ggplot(dados5, aes(x = x, y = C5, group = Legenda, colour = Legenda))+
  geom_line()+
  scale_colour_manual(values = c("blue", "red")) +
  labs(title = "(E)",
       x = "Longitude (º)",
       y = "Frequência Relativa (%)",
       colour = " ")+
  theme_classic()


ggarrange(A,B,C,D,E, ncol = 2, nrow = 3, common.legend = TRUE, legend = "top")

rm(list=ls())

# Figura 5

FE <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S5/Mapas2.xlsx")
mc <- read_municipality(code_muni= "SP")
FAT <- merge(mc,FE)

FAT$AL <- cut(FAT$Altitude, breaks = c(0,200,400,600,800,1000,1200,1400,1600,Inf),
              labels = c("0-200","200-400","400-600","600-800","800-1000","1000-1200","1200-1400","1400-1600","1600+"))

FAT$LAR <- cut(FAT$AreaL, breaks = c(0,0.5,1,1.5,2,2.5,3,3.5,4),
               labels = c("0 - 0.5","0.5 - 1", "1 - 1.5", "1.5 - 2", "2 - 2.5", "2.5 - 3", "3 - 3.5","3.5 - 4"))

FAT$LPOP <- cut (FAT$PopulacaoL, breaks = c(2,3,4,5,6,7,8), 
                 labels = c("2 - 3", "3 - 4", "4 - 5", "5 - 6", "6 - 7", "7+"))


## Altitude


A <- ggplot(FAT) +  
  geom_sf(aes(fill=AL), color = NA) + 
  labs(subtitle="", size=8)+ 
  scale_fill_manual (values = brewer.pal(9,'OrRd'))+  
  labs(fill = "Altitude (m) ")+  
  theme_minimal()


## Área


B <- ggplot(FAT) +
  geom_sf(aes(fill=LAR), color = NA) +
  labs(subtitle=" ", size=8)+  
  scale_fill_manual (values = brewer.pal(7,'OrRd'))+  
  labs(fill = "Área (Log10(km^2))")+  
  theme_minimal()


## População


C <- ggplot(FAT) + 
  geom_sf(aes(fill=LPOP), color = NA) + 
  labs(subtitle="", size=8)+   
  scale_fill_manual (values = brewer.pal(9,'OrRd'))+  
  labs(fill = "População (Log10)")+ 
  theme_minimal()

ggarrange(A,B,C, ncol = 2, nrow = 2)

rm(list=ls())

# Figura 6

AL <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S3/Altitude.xlsx")
AR <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S3/Area.xlsx")
POP <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S3/Populacao.xlsx")
LA <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S3/Latitude.xlsx")
LO <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S3/Longitude.xlsx")
dados <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S3/Auxiliar.xlsx")


AL$L1 <- factor(AL$L1, levels=c("WAV (N = 598)", "SLI (N = 145)", "WAV 2 (N = 141)"))

A1 <- ggplot(AL, aes(x = FR, y = R, colour = L1))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
  stat_smooth(method = "lm",
              se = FALSE,
              fullrange = TRUE,
              size = 1)+
  labs( title = "(B1)",
        x ="Altitude (m)",
        y =  "Número de Registros (Log10)",
        colour = " ")+
  theme_classic()


#### Espécies


AL$L2 <- factor(AL$L2, levels=c("WAV (N = 557)", "SLI (N = 147)", "WAV 2 (N = 136)"))

A2 <- ggplot(AL, aes(x = FE, y = E, color = L2))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
  stat_smooth(method = "lm",
              se = FALSE,
              fullrange = TRUE,
              size = 1)+
  labs( title = "(A1)",
        x = "Altitude (m)",
        y = "Número de Espécies (Log10)",
        colour = " ")+
  theme_classic()

### Área

#### Registros


AR$L1 <- factor(AR$L1, levels=c("WAV (N = 624)", "SLI (N = 171)", "WAV 2 (N = 168)"))

B1 <- ggplot(AR, aes(x = FR, y = R, color = L1))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
  stat_smooth(method = "lm",
              se = FALSE,
              fullrange = TRUE,
              size = 1)+
  labs( title = "(B2)",
        x ="Área (Log10(Km^2))",
        y =  "Número de Registros (Log10)",
        colour = " ")+
  theme_classic()

#### Espécies


AR$L2 <- factor(AR$L2, levels=c("WAV (N = 605)", "SLI (N = 171)", "WAV 2 (N = 161)"))

B2 <- ggplot(AR, aes(x = FE, y = E, color = L2))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
  stat_smooth(method = "lm",
              se = FALSE,
              fullrange = TRUE,
              size = 1)+
  labs( title = "(A2)",
        x = "Área (Log10(Km^2))",
        y = "Número de Espécies (Log10)",
        colour = " ")+
  theme_classic()

### População

#### Registros



POP$L1 <- factor(POP$L1, levels=c("WAV (N = 619)", "SpeciesLink", "WAV 2 (N = 167)") ,labels = c("WAV (N = 619)", "SLI (N = 173)", "WAV 2 (N = 167)"))

C1 <- ggplot(POP, aes(x = FR, y = R, color = L1))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
  stat_smooth(method = "lm",
              se = FALSE,
              fullrange = TRUE,
              size = 1)+
  labs( title = "(B3)",
        x ="População (Log10)",
        y =  "Número de Registros (Log10)",
        colour = " ")+
  theme_classic()


#### Espécies


POP$L2 <- factor(POP$L2, levels=c("WAV (N = 611)", "SLI (N = 173)", "WAV 2 (N = 158)"))

C2 <- ggplot(POP, aes(x = FE, y = E, color = L2))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
  stat_smooth(method = "lm",
              se = FALSE,
              fullrange = TRUE,
              size = 1)+
  labs( title = "(A3)",
        x = "População (Log10)",
        y = "Número de Espécies (Log10)",
        color = " ")+
  theme_classic()

### Latitude

#### Registros



LA$L1 <- factor(LA$L1, levels=c("WAV (N = 629)", "SLI (N = 174)", "WAV 2 (N = 170)"))

D1 <- ggplot(LA, aes(x = FR, y = R, color = L1))+
  geom_point()+
  scale_colour_manual(values = c("blue","red", "green")) +
  stat_smooth(method = "lm",
              se = FALSE,
              fullrange = TRUE,
              size = 1)+
  labs( title = "(B4)",
        x ="Latitude (º)",
        y =  "Número de Registros (Log10)",
        colour = " ")+
  theme_classic()


#### Espécies


LA$L2 <- factor(LA$L2, levels=c("WAV (N = 605)", "SLI (N = 174)", "WAV 2 (N = 164)"))

D2 <- ggplot(LA, aes(x = FE, y = E, color = L2))+
  geom_point()+
  scale_colour_manual(values = c("blue","red", "green")) +
  stat_smooth(method = "lm",
              se = FALSE,
              fullrange = TRUE,
              size = 1)+
  labs( title = "(A4)",
        x ="Latitude (º)",
        y = "Número de Espécies (Log10)",
        colour = " ")+
  theme_classic()


### Longitude

#### Registros


LO$L1 <- factor(LO$L1, levels=c("WAV (N = 627)", "SLI (N = 174)", "WAV 2 (N = 170)"))

E1 <- ggplot(LO, aes(x = FR, y = R, color = L1))+
  geom_point()+
  scale_colour_manual(values = c("blue","red", "green")) +
  stat_smooth(method = "lm",
              se = FALSE,
              fullrange = TRUE,
              size = 1)+
  labs( title = "(B5)",
        x ="Longitude (º)",
        y =  "Número de Registros (Log10)",
        colour = " ")+
  theme_classic()

#### Espécies


LO$L2 <- factor(LO$L2, levels=c("WAV (N = 602)", "SLI (N = 174)", "WAV 2 (N = 167)"))

E2 <- ggplot(LO, aes(x = FE, y = E, color = L2))+
  geom_point()+
  scale_colour_manual(values = c("blue", "red", "green")) +
  stat_smooth(method = "lm",
              se = FALSE,
              fullrange = TRUE,
              size = 1)+
  labs( title = "(A5)",
        x ="Longitude (º)",
        y = "Número de Espécies (Log10)",
        colour = " ")+
  theme_classic()


ggarrange(A2,A1,B2,B1,C2,C1,D2,D1,E2,E1,  ncol = 2, nrow = 5, legend = "right")


rm(list=ls())

# Figura 7

WAV1 <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S6/Dados2.xlsx", sheet = "WAV1")
WAV1 <- t(WAV1)

SLI1 <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S6/Dados2.xlsx", sheet = "SLI1")
SLI1 <- t(SLI1)

A1 <- vegdist(WAV1, method = "jaccard", binary = TRUE)
B1 <- vegdist(SLI1, method = "jaccard", binary = TRUE)

CA1 <- hclust(A1, method = "ward.D2")
CB1 <- hclust(B1, method = "ward.D2")


dendA1 <- as.dendrogram (CA1)
dendB1 <- as.dendrogram (CB1)

tanglegram(dendA1, dendB1,
           highlight_distinct_edges = FALSE, 
           common_subtrees_color_lines = TRUE, 
           common_subtrees_color_branches = FALSE,
           main_left = "Wikiaves",
           main_right = "SpeciesLink",
           lwd = .5,
           k_branches = 5)

# Figura 8

WAV2 <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S6/Dados2.xlsx", sheet = "WAV2")
WAV2 <- t(WAV2)

SLI2 <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S6/Dados2.xlsx", sheet = "SLI2")
SLI2 <- t(SLI2)

A2 <- vegdist(WAV2, method = "jaccard", binary = TRUE)
B2 <- vegdist(SLI2, method = "jaccard", binary = TRUE)

CA2 <- hclust(A2, method = "ward.D2")
CB2 <- hclust(B2, method = "ward.D2")

dendA2 <- as.dendrogram (CA2)
dendB2 <- as.dendrogram (CB2)

tanglegram(dendA2, dendB2,
           highlight_distinct_edges = FALSE, 
           common_subtrees_color_lines = TRUE, 
           common_subtrees_color_branches = FALSE,
           main_left = "Wikiaves",
           main_right = "SpeciesLink",
           lwd = .5,
           k_branches = 5)


# Figura 9

mapas<-read.xlsx("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S6/Dados.xlsx", sheet = "Mapas")
mc <- read_municipality(code_muni= "SP")
mapas <- merge(mc,mapas)

mapas$N1WK2 <- factor(mapas$N1WK2, levels=c("Sem Registro", "Grupo 1", "Grupo 2"), labels = c("Sem Registro", "Grupo 1 (N = 34)", "Grupo 2 (N = 37)"))
mapas$N1SK2 <- factor(mapas$N1SK2, levels=c("Sem Registro", "Grupo 1", "Grupo 2"), labels = c("Sem Registro", "Grupo 1 (N = 34)", "Grupo 2 (N = 37)"))
mapas$N1WK3 <- factor(mapas$N1WK3, levels=c("Sem Registro", "Grupo 1", "Grupo 2","Grupo 3"), labels = c("Sem Registro", "Grupo 1 (N = 28)", "Grupo 2 (N = 37)", "Grupo 3 (N = 6)"))
mapas$N1SK3 <- factor(mapas$N1SK3, levels=c("Sem Registro", "Grupo 1", "Grupo 2","Grupo 3"), labels = c("Sem Registro", "Grupo 1 (N = 24)", "Grupo 2 (N = 36)", "Grupo 3 (N = 11)"))
mapas$N1WK4 <- factor(mapas$N1WK4, levels=c("Sem Registro", "Grupo 1", "Grupo 2","Grupo 3", "Grupo 4"), labels = c("Sem Registro", "Grupo 1 (N = 28)", "Grupo 2 (N = 17)", "Grupo 3 (N = 20)", "Grupo 4 (N = 6)"))
mapas$N1SK4 <- factor(mapas$N1SK4, levels=c("Sem Registro", "Grupo 1", "Grupo 2","Grupo 3", "Grupo 4"), labels = c("Sem Registro", "Grupo 1 (N = 24)", "Grupo 2 (N = 18)", "Grupo 3 (N = 21)", "Grupo 4 (N = 11)"))
mapas$N1WK5 <- factor(mapas$N1WK5, levels=c("Sem Registro", "Grupo 1", "Grupo 2","Grupo 3", "Grupo 4", "Grupo 5"), labels = c("Sem Registro", "Grupo 1 (N = 17)", "Grupo 2 (N = 11)","Grupo 3 (N = 17)", "Grupo 4 (N = 20)", "Grupo 5 (N = 6)"))
mapas$N1SK5 <- factor(mapas$N1SK5, levels=c("Sem Registro", "Grupo 1", "Grupo 2","Grupo 3", "Grupo 4", "Grupo 5"), labels = c("Sem Registro", "Grupo 1 (N = 21)", "Grupo 2 (N = 17)","Grupo 3 (N = 20)", "Grupo 4 (N = 10)", "Grupo 5 (N = 3)"))
mapas$N2WK2 <- factor(mapas$N2WK2, levels=c("Sem Registro", "Grupo 1", "Grupo 2"), labels = c("Sem Registro", "Grupo 1 (N = 21)", "Grupo 2 (N = 25)"))
mapas$N2SK2 <- factor(mapas$N2SK2, levels=c("Sem Registro", "Grupo 1", "Grupo 2"), labels = c("Sem Registro", "Grupo 1 (N = 30)", "Grupo 2 (N = 16)"))
mapas$N2WK3 <- factor(mapas$N2WK3, levels=c("Sem Registro", "Grupo 1", "Grupo 2","Grupo 3"), labels = c("Sem Registro", "Grupo 1 (N = 21)", "Grupo 2 (N = 12)","Grupo 3 (N = 13)"))
mapas$N2SK3 <- factor(mapas$N2SK3, levels=c("Sem Registro", "Grupo 1", "Grupo 2","Grupo 3"), labels = c("Sem Registro", "Grupo 1 (N = 16)", "Grupo 2 (N = 14)","Grupo 3 (N = 16)"))
mapas$N2WK4 <- factor(mapas$N2WK4, levels=c("Sem Registro", "Grupo 1", "Grupo 2","Grupo 3", "Grupo 4"), labels = c("Sem Registro", "Grupo 1 (N = 7)", "Grupo 2 (N = 14)","Grupo 3 (N = 12)", "Grupo 4 (N = 13)"))
mapas$N2SK4 <- factor(mapas$N2SK4, levels=c("Sem Registro", "Grupo 1", "Grupo 2","Grupo 3", "Grupo 4"), labels = c("Sem Registro", "Grupo 1 (N = 10)", "Grupo 2 (N = 6)","Grupo 3 (N = 14)", "Grupo 4 (N = 16)"))
mapas$N2WK5 <- factor(mapas$N2WK5, levels=c("Sem Registro", "Grupo 1", "Grupo 2","Grupo 3", "Grupo 4", "Grupo 5"), labels = c("Sem Registro", "Grupo 1 (N = 7)", "Grupo 2 (N = 14)","Grupo 3 (N = 7)", "Grupo 4 (N = 13)", "Grupo 5 (N = 5)"))
mapas$N2SK5 <- factor(mapas$N2SK5, levels=c("Sem Registro", "Grupo 1", "Grupo 2","Grupo 3", "Grupo 4", "Grupo 5"), labels = c("Sem Registro", "Grupo 1 (N = 10)", "Grupo 2 (N = 6)","Grupo 3 (N = 14)", "Grupo 4 (N = 11)", "Grupo 5 (N = 5)"))


AW1 <- ggplot(mapas) +
    geom_sf(aes(fill=N1WK2), color = NA) +
    labs(title="AW1")+ 
    scale_fill_manual (values = c('#f0f0f0','#009681','#CC476B'))+
    labs(fill = " ")+
    theme_minimal()

AS1 <- ggplot(mapas) +
    geom_sf(aes(fill=N1SK2), color = NA) +
    labs(title="AS1")+ 
    scale_fill_manual (values = c('#f0f0f0','#009681','#CC476B'))+
    labs(fill = " ")+
    theme_minimal()


AW2 <- ggplot(mapas) +
  geom_sf(aes(fill=N1WK3), color = NA) +
  labs(subtitle="Aw2")+ 
  scale_fill_manual (values = c('#f0f0f0','#0082CE','#228B00','#CC476B'))+
  labs(fill = " ")+
  theme_minimal()

AS2 <- ggplot(mapas) +
  geom_sf(aes(fill=N1SK3), color = NA) +
  labs(subtitle="AS2")+ 
  scale_fill_manual (values = c('#f0f0f0','#0082CE','#228B00','#CC476B'))+
  labs(fill = " ")+
  theme_minimal()

AW3 <- ggplot(mapas) +
  geom_sf(aes(fill=N1WK4), color = NA) +
  labs(subtitle="AW3")+ 
  scale_fill_manual (values = c('#f0f0f0','#7866D8','#767F00','#CC476B','#009681'))+
  labs(fill = " ")+
  theme_minimal()

AS3 <- ggplot(mapas) +
  geom_sf(aes(fill=N1SK4), color = NA) +
  labs(subtitle="AS3")+ 
  scale_fill_manual (values = c('#f0f0f0','#7866D8','#767F00','#CC476B','#009681'))+
  labs(fill = " ")+
  theme_minimal()


AW4 <- ggplot(mapas) +
  geom_sf(aes(fill=N1WK5), color = NA) +
  labs(subtitle="AW4")+ 
  scale_fill_manual (values = c('#f0f0f0','#B646C7','#228B00','#9F7000','#0082CE','#CC476B'))+
  labs(fill = " ")+
  theme_minimal()


AS4 <- ggplot(mapas) +
  geom_sf(aes(fill=N1SK5), color = NA) +
  labs(subtitle="AS4")+ 
  scale_fill_manual (values = c('#f0f0f0','#B646C7','#228B00','#9F7000','#0082CE','#CC476B'))+
  labs(fill = " ")+
  theme_minimal()

BW1 <- ggplot(mapas) +
  geom_sf(aes(fill=N2WK2), color = NA) +
  labs(subtitle="BW1")+ 
  scale_fill_manual (values = c('#f0f0f0','#009681','#CC476B'))+
  labs(fill = " ")+
  theme_minimal()

BS1 <- ggplot(mapas) +
  geom_sf(aes(fill=N2SK2), color = NA) +
  labs(subtitle="BS1")+ 
  scale_fill_manual (values = c('#f0f0f0','#009681','#CC476B'))+
  labs(fill = " ")+
  theme_minimal()


BW2 <- ggplot(mapas) +
  geom_sf(aes(fill=N2WK3), color = NA) +
  labs(subtitle="BW2")+ 
  scale_fill_manual (values = c('#f0f0f0','#0082CE','#228B00','#CC476B'))+
  labs(fill = " ")+
  theme_minimal()

BS2 <- ggplot(mapas) +
  geom_sf(aes(fill=N2SK3), color = NA) +
  labs(subtitle="BS2")+ 
  scale_fill_manual (values = c('#f0f0f0','#0082CE','#228B00','#CC476B'))+
  labs(fill = " ")+
  theme_minimal()

BW3 <- ggplot(mapas) +
  geom_sf(aes(fill=N2WK4), color = NA) +
  labs(subtitle="BW3")+ 
  scale_fill_manual (values = c('#f0f0f0','#7866D8','#767F00','#CC476B','#009681'))+
  labs(fill = " ")+
  theme_minimal()

BS3 <- ggplot(mapas) +
  geom_sf(aes(fill=N2SK4), color = NA) +
  labs(subtitle="BS3")+ 
  scale_fill_manual (values = c('#f0f0f0','#7866D8','#767F00','#CC476B','#009681'))+
  labs(fill = " ")+
  theme_minimal()


BW4 <- ggplot(mapas) +
  geom_sf(aes(fill=N2WK5), color = NA) +
  labs(subtitle="Bw4")+ 
  scale_fill_manual (values = c('#f0f0f0','#B646C7','#228B00','#9F7000','#0082CE','#CC476B'))+
  labs(fill = " ")+
  theme_minimal()


BS4 <- ggplot(mapas) +
  geom_sf(aes(fill=N2SK5), color = NA) +
  labs(subtitle="BS4")+ 
  scale_fill_manual (values = c('#f0f0f0','#B646C7','#228B00','#9F7000','#0082CE','#CC476B'))+
  labs(fill = " ")+
  theme_minimal()

ggarrange(AS1,AW1,BS1,BW1,AS2,AW2,BS2,BW2,AS3,AW3,BS3,BW3,AS4,AW4,BS4,BW4, ncol = 4, nrow= 4)

rm(list = ls())

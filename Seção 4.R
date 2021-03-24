#Importando bibliotecas e dados

library("openxlsx") 
library(ggplot2)
library(RColorBrewer)
library(hrbrthemes)
library(ggpubr)



dados <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S4/WS.xlsx")
RW <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S4/RW.xlsx")
RS <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S4/RS.xlsx")
RW2 <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/ProjWAV/Excel/S4/RW2.xlsx")



gráfico <- ggplot(dados, aes(x = A2, y = A1))+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
  expand_limits(x=0,y=0)+
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

png("4111.png", width = 1000, height = 450)
ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1)
dev.off()


gráfico <- ggplot(dados, aes(x = B2, y = B1))+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
  expand_limits(x=0,y=0)+
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

png("4211.png", width = 1000, height = 450)
ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1)
dev.off()


dados$L1 <- factor(dados$L1, levels=c("Wikiaves", "SpeciesLink", "Wikiaves 2"), labels = c("WAV (N = 620)","SLI (N = 143)","WAV 2 (N = 171)"))

gráfico <- ggplot(dados, aes(x = C1, y = C2, color = L1))+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
  expand_limits(x=0,y=0)+
  geom_point()+
  scale_colour_manual(values = c("blue","red", "green")) +
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

png("4311.png", width = 1000, height = 450)
ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
dev.off()

## Representação 1.2


gráfico <- ggplot(dados, aes(x = A2, y = A1))+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
  expand_limits(x=0,y=0)+
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

resíduos <- qplot(fitted(A), resid(A))+ 
  geom_point()+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

png("4112.png", width = 1000, height = 450)
ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
dev.off()


gráfico <- ggplot(dados, aes(x = B2, y = B1))+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
  expand_limits(x=0,y=0)+
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

resíduos <- qplot(fitted(A), resid(A))+ 
  geom_point()+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

png("4212.png", width = 1000, height = 450)
ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
dev.off()



gráfico <- ggplot(dados, aes(x = C1, y = C2, color = L1))+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
  expand_limits(x=0,y=0)+
  geom_point()+
  scale_colour_manual(values = c("blue","red", "green")) +
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
B <- lm(SE~poly(SR,2), data = RS)
C <- lm(W2E~poly(W2R,2), data = RW2)

a <- qplot(fitted(A), resid(A))+ 
  geom_point(col = "blue")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

b <- qplot(fitted(B), resid(B))+ 
  geom_point(col = "Red")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

c <- qplot(fitted(C), resid(C))+ 
  geom_point(col = "Green")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

resíduos <- ggarrange(a,b,c, ncol = 1, nrow = 3)

png("4312.png", width = 1000, height = 450)
ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
dev.off()


## Representação 1.3


gráfico <- ggplot(dados, aes(x = A2, y = A1))+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
  expand_limits(x=0,y=0)+
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

R1 <- resid (A)

min(R1)
max(R1)

AX <- hist(R1, 
           plot = FALSE, 
           breaks = c(-1.7,-1.545,-1.39,-1.235,-1.08,-0.925,-0.77,-0.615,-0.46,-0.305,-0.15,0.005,0.16,0.315,0.47,0.625,0.78,0.935,1.09,1.245,1.409))
AX$density = AX$counts/sum(AX$counts)*100


y1 <- AX$density

x1 <- c()
x1[1] <- -1.7
for (i in 1:19){
  x1[i+1] = x1[i]+0.155
}

B <- c()

for (i in 1:20){
  B[i] = x1[i]
}

C<- c()

for (i in 1:20){
  C[i] = y1[i]
}

data <- data.frame(x = x1, y = C)

Resíduos <-  ggplot(data, aes(x = x, y = y))+
  geom_line()+
  #stat_smooth(method = lm, formula = y ~ poly(x, 4),se = FALSE, fullrange = TRUE)+
  labs(x = "Resíduos",
       y = "Frequência Relativa (%)")+
  theme_classic()


png("4113.png", width = 1000, height = 450)
ggarrange(gráfico, Resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
dev.off()


gráfico <- ggplot(dados, aes(x = B2, y = B1))+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
  expand_limits(x=0,y=0)+
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

R1 <- resid (A)

min(R1)
max(R1)

AX <- hist(R1, 
           plot = FALSE, 
           breaks = c(-0.65,-0.5965,-0.543,-0.4895,-0.436,-0.3825,-0.329,-0.2755,-0.222,-0.1685,-0.115,-0.0615,-0.008,0.0455,0.099,0.1525,0.206,0.2595,0.313,0.3665,0.42))
AX$density = AX$counts/sum(AX$counts)*100


y1 <- AX$density

x1 <- c()
x1[1] <- -0.65
for (i in 1:19){
  x1[i+1] = x1[i]+0.0535
}

B <- c()

for (i in 1:20){
  B[i] = x1[i]
}

C<- c()

for (i in 1:20){
  C[i] = y1[i]
}

data <- data.frame(x = B, y = C)

Resíduos <-  ggplot(data, aes(x = x, y = y))+
  geom_line()+
  #stat_smooth(method = lm, formula = y ~ poly(x, 4),se = FALSE, fullrange = TRUE)+
  labs(x = "Resíduos",
       y = "Frequência Relativa (%)")+
  theme_classic()


png("4213.png", width = 1000, height = 450)
ggarrange(gráfico, Resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
dev.off()



gráfico <- ggplot(dados, aes(x = C1, y = C2, color = L1))+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
  expand_limits(x=0,y=0)+
  geom_point()+
  scale_colour_manual(values = c("blue","red", "green")) +
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
B <- lm(SE~poly(SR,2), data = RS)
C <- lm(W2E~poly(W2R,2), data = RW2)

R1 <- resid (A)
R2 <- resid (B)
R3 <- resid (C)

min(R1,R2,R3)
max(R1,R2,R3)

AX <- hist(R1, 
           plot = FALSE, 
           breaks = c(-0.4,-0.2765,-0.253,-0.2295,-0.206,-0.1825,-0.159,-0.1355,-0.112,-0.0885,-0.065,-0.0415,-0.018,0.0055,0.029,0.0525,0.076,0.0995,0.123,0.1465,0.18))
AX$density = AX$counts/sum(AX$counts)*100

BX <- hist(R2, 
           plot = FALSE, 
           breaks = c(-0.4,-0.2765,-0.253,-0.2295,-0.206,-0.1825,-0.159,-0.1355,-0.112,-0.0885,-0.065,-0.0415,-0.018,0.0055,0.029,0.0525,0.076,0.0995,0.123,0.1465,0.18))
BX$density = BX$counts/sum(BX$counts)*100

CX <- hist(R3, 
           plot = FALSE, 
           breaks = c(-0.4,-0.2765,-0.253,-0.2295,-0.206,-0.1825,-0.159,-0.1355,-0.112,-0.0885,-0.065,-0.0415,-0.018,0.0055,0.029,0.0525,0.076,0.0995,0.123,0.1465,0.18))
CX$density = CX$counts/sum(CX$counts)*100

y1 <- AX$density
y2 <- BX$density
y3 <- CX$density

x1 <- c()
x1[1] <- -0.3
for (i in 1:19){
  x1[i+1] = x1[i]+0.0235
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

png("4313.png", width = 1000, height = 450)
ggarrange(gráfico, Resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
dev.off()


## Representação 2.1


gráfico <- ggplot(dados, aes(x = C1, y = C2, color = L1, alpha =I(.5)))+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
  expand_limits(x=0,y=0)+
  geom_point()+
  scale_colour_manual(values = c( "blue","red","green")) +
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
B <- lm(SE~poly(SR,2), data = RS)
C <- lm(W2E~poly(W2R,2), data = RW2)

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

png("4321.png", width = 1000, height = 450)
ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
dev.off()

## Representação 2.2


gráfico <- ggplot(dados, aes(x = C1, y = C2, color = L1, alpha =I(.5)))+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
  expand_limits(x=0,y=0)+
  geom_point()+
  scale_colour_manual(values = c( "blue","red","green")) +
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
B <- lm(SE~poly(SR,2), data = RS)
C <- lm(W2E~poly(W2R,2), data = RW2)

a <- qplot(fitted(A), resid(A))+ 
  geom_point(col = "blue")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

b <- qplot(fitted(B), resid(B))+ 
  geom_point(col = "Red")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

c <- qplot(fitted(C), resid(C))+ 
  geom_point(col = "Green")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

resíduos <- ggarrange(a,b,c, ncol = 1, nrow = 3)

png("4322.png", width = 1000, height = 450)
ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
dev.off()


## Representação 2.3


gráfico <- ggplot(dados, aes(x = C1, y = C2, color = L1, alpha =I(.5)))+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
  expand_limits(x=0,y=0)+
  geom_point()+
  scale_colour_manual(values = c( "blue","red","green")) +
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
B <- lm(SE~poly(SR,2), data = RS)
C <- lm(W2E~poly(W2R,2), data = RW2)


R1 <- resid (A)
R2 <- resid (B)
R3 <- resid (C)

min(R1,R2,R3)
max(R1,R2,R3)

AX <- hist(R1, 
           plot = FALSE, 
           breaks = c(-0.4,-0.2765,-0.253,-0.2295,-0.206,-0.1825,-0.159,-0.1355,-0.112,-0.0885,-0.065,-0.0415,-0.018,0.0055,0.029,0.0525,0.076,0.0995,0.123,0.1465,0.18))
AX$density = AX$counts/sum(AX$counts)*100

BX <- hist(R2, 
           plot = FALSE, 
           breaks = c(-0.4,-0.2765,-0.253,-0.2295,-0.206,-0.1825,-0.159,-0.1355,-0.112,-0.0885,-0.065,-0.0415,-0.018,0.0055,0.029,0.0525,0.076,0.0995,0.123,0.1465,0.18))
BX$density = BX$counts/sum(BX$counts)*100

CX <- hist(R3, 
           plot = FALSE, 
           breaks = c(-0.4,-0.2765,-0.253,-0.2295,-0.206,-0.1825,-0.159,-0.1355,-0.112,-0.0885,-0.065,-0.0415,-0.018,0.0055,0.029,0.0525,0.076,0.0995,0.123,0.1465,0.18))
CX$density = CX$counts/sum(CX$counts)*100

y1 <- AX$density
y2 <- BX$density
y3 <- CX$density

x1 <- c()
x1[1] <- -0.3
for (i in 1:19){
  x1[i+1] = x1[i]+0.0235
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
  geom_line(fullrange = TRUE)+
  scale_colour_manual(values = c("red", "blue", "green")) +
  #stat_smooth(method = lm, formula = y ~ poly(x, 4),se = FALSE, fullrange = TRUE)+
  labs(x = "Resíduos",
       y = "Frequência Relativa (%)",
       colour = " ")+
  theme_classic()

png("4323.png", width = 1000, height = 450)
ggarrange(gráfico, Resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
dev.off()

## Representação 3.1



gráfico <- ggplot(dados, aes(x = C1, y = C2, color = L1, shape = L1))+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
  expand_limits(x=0,y=0)+
  geom_point()+
  scale_shape_manual(values=c(15, 16, 17))+
  scale_colour_manual(values = c("blue","red", "green")) +
  stat_smooth(method = "lm",
              formula = y~poly(x,2),
              se = FALSE,
              fullrange = TRUE,
              size = 1)+
  labs( x ="Número de Registros (Log10)",
        y =  "Número de Espécies (Log10)",
        colour = " ",
        shape = " ")+
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

png("4331.png", width = 1000, height = 450)
ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
dev.off()

## Representação 3.2


gráfico <- ggplot(dados, aes(x = C1, y = C2, color = L1, shape = L1))+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
  expand_limits(x=0,y=0)+
  geom_point()+
  scale_colour_manual(values = c("blue","red", "green")) +
  scale_shape_manual(values=c(15, 16, 17))+
  stat_smooth(method = "lm",
              formula = y~poly(x,2),
              se = FALSE,
              fullrange = TRUE,
              size = 1)+
  labs( x ="Número de Registros (Log10)",
        y =  "Número de Espécies (Log10)",
        colour = " ",
        shape = " ")+
  theme_classic()

A <- lm(WE~poly(WR,2), data = RW)
B <- lm(SE~poly(SR,2), data = RS)
C <- lm(W2E~poly(W2R,2), data = RW2)

a <- qplot(fitted(A), resid(A))+ 
  geom_point(col = "blue")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

b <- qplot(fitted(B), resid(B))+ 
  geom_point(col = "Red")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

c <- qplot(fitted(C), resid(C))+ 
  geom_point(col = "Green")+
  geom_hline(yintercept=0) + 
  labs(y = "Resíduos",
       x = " ")+
  theme_classic()

resíduos <- ggarrange(a,b,c, ncol = 1, nrow = 3)

png("4332.png", width = 1000, height = 450)
ggarrange(gráfico,resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
dev.off()


## Representação 3.3



gráfico <- ggplot(dados, aes(x = C1, y = C2, color = L1, shape = L1))+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
  expand_limits(x=0,y=0)+
  geom_point()+
  scale_colour_manual(values = c("blue","red", "green")) +
  scale_shape_manual(values=c(15, 16, 17))+
  stat_smooth(method = "lm",
              formula = y~poly(x,2),
              se = FALSE,
              fullrange = TRUE,
              size = 1)+
  labs( x ="Número de Registros (Log10)",
        y =  "Número de Espécies (Log10)",
        colour = " ",
        shape = " ")+
  theme_classic()

A <- lm(WE~poly(WR,2), data = RW)
B <- lm(SE~poly(SR,2), data = RS)
C <- lm(W2E~poly(W2R,2), data = RW2)

R1 <- resid (A)
R2 <- resid (B)
R3 <- resid (C)

min(R1,R2,R3)
max(R1,R2,R3)

AX <- hist(R1, 
           plot = FALSE, 
           breaks = c(-0.4,-0.2765,-0.253,-0.2295,-0.206,-0.1825,-0.159,-0.1355,-0.112,-0.0885,-0.065,-0.0415,-0.018,0.0055,0.029,0.0525,0.076,0.0995,0.123,0.1465,0.18))
AX$density = AX$counts/sum(AX$counts)*100

BX <- hist(R2, 
           plot = FALSE, 
           breaks = c(-0.4,-0.2765,-0.253,-0.2295,-0.206,-0.1825,-0.159,-0.1355,-0.112,-0.0885,-0.065,-0.0415,-0.018,0.0055,0.029,0.0525,0.076,0.0995,0.123,0.1465,0.18))
BX$density = BX$counts/sum(BX$counts)*100

CX <- hist(R3, 
           plot = FALSE, 
           breaks = c(-0.4,-0.2765,-0.253,-0.2295,-0.206,-0.1825,-0.159,-0.1355,-0.112,-0.0885,-0.065,-0.0415,-0.018,0.0055,0.029,0.0525,0.076,0.0995,0.123,0.1465,0.18))
CX$density = CX$counts/sum(CX$counts)*100

y1 <- AX$density
y2 <- BX$density
y3 <- CX$density

x1 <- c()
x1[1] <- -0.3
for (i in 1:19){
  x1[i+1] = x1[i]+0.0235
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
  geom_line(fullrange = TRUE)+
  scale_colour_manual(values = c("red", "blue", "green")) +
  #stat_smooth(method = lm, formula = y ~ poly(x, 4),se = FALSE, fullrange = TRUE)+
  labs(x = "Resíduos",
       y = "Frequência Relativa (%)",
       colour = " ")+
  theme_classic()

png("4333.png", width = 1000, height = 450)
ggarrange(gráfico, Resíduos,  ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
dev.off()



rm(list=ls())


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
library(gclus)
library(cluster)
library(FD)
library(ggcorrplot)

Dados <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/R/S5/Mapas.xlsx")
FE <- read.xlsx ("C:/Users/bia99/OneDrive/Documents/Wikiaves/R/S5/Mapas2.xlsx")
mc <- read_municipality(code_muni= "SP")



FAT <- merge(mc,FE)
cid <- merge(mc,Dados)



FAT$AL <- cut(FAT$Altitude, breaks = c(0,200,400,600,800,1000,1200,1400,1600,Inf),
              labels = c("0-200","200-400","400-600","600-800","800-1000","1000-1200","1200-1400","1400-1600","1600+"))

FAT$AR <- cut(FAT$Area, breaks = c(0,250,500,750,1000,1250,1500,Inf), 
              labels = c("0-250","250-500","500-750","750-1000","1000-1250","1250-1500","1500+"))

FAT$POP <- cut(FAT$Populacao, breaks = c(0,1500000,300000,4500000,6000000,7500000,9000000,10500000,12000000,Inf), 
               labels = c("0 - 1.500.000","1.500.000 - 3.000.000","3.000.000 - 4.500.000" , "4.500.000 - 6.000.000","6.000.000 - 7.500.000","7.500.000 - 9.000.000", "9.000.000 - 10.500.000","10.500.000 - 12.000.000","12.000.000+"))

FAT$LAT <- cut(FAT$Latitude, breaks = c(-26,-25,-24,-23,-22,-21,-20,Inf), 
               labels = c("-26 a -25","-25 a -24","-24 a -23","-23 a -22","-22 a -21", "-21 a -20","-20+"))

FAT$LOG <- cut(FAT$Longitude, breaks = c(-55,-52.5,-50,-47.5,-45,Inf), 
               labels = c("-55 a -52.5","-52.5 a -50","-50 a -47.5","-47.5 a -45","-45 +"))

FAT$LAR <- cut(FAT$AreaL, breaks = c(0,0.5,1,1.5,2,2.5,3,3.5,4),
               labels = c("0 - 0.5","0.5 - 1", "1 - 1.5", "1.5 - 2", "2 - 2.5", "2.5 - 3", "3 - 3.5","3.5 - 4"))

FAT$LPOP <- cut (FAT$PopulacaoL, breaks = c(2,3,4,5,6,7,8), 
                 labels = c("2 - 3", "3 - 4", "4 - 5", "5 - 6", "6 - 7", "7+"))


## Altitude

png("511.png", width = 600, height = 300)
ggplot(FAT) +  
  geom_sf(aes(fill=AL), color = NA) + 
  labs(subtitle="", size=8)+ 
  scale_fill_manual (values = brewer.pal(9,'OrRd'))+  
  labs(fill = "Altitude (m) ")+  
  theme_minimal()
dev.off()

## Área

png("521.png", width = 600, height = 300)
ggplot(FAT) +
  geom_sf(aes(fill=LAR), color = NA) +
  labs(subtitle=" ", size=8)+  
  scale_fill_manual (values = brewer.pal(7,'OrRd'))+  
  labs(fill = "Área (Log10(km^2))")+  
  theme_minimal()
dev.off()

## População

png("531.png", width = 600, height = 300)
ggplot(FAT) + 
  geom_sf(aes(fill=LPOP), color = NA) + 
  labs(subtitle="", size=8)+   
  scale_fill_manual (values = brewer.pal(9,'OrRd'))+  
  labs(fill = "População (Log10)")+ 
  theme_minimal()
dev.off()

##Latitude

png("541.png", width = 600, height = 300)
ggplot(FAT) + 
  geom_sf(aes(fill=LAT), color = NA) + 
  labs(subtitle="", size=8)+  
  scale_fill_manual (values = brewer.pal(7,'OrRd'))+ 
  labs(fill = "Latitude (°) ")+ 
  theme_minimal()
dev.off()


##Longitude

png("551.png", width = 600, height = 300)
ggplot(FAT) + 
  geom_sf(aes(fill=LOG), color = NA) + 
  labs(subtitle="", size=8)+  
  scale_fill_manual (values = brewer.pal(6,'OrRd'))+ 
  labs(fill = "Longitude  (°)")+ 
  theme_minimal()
dev.off()

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

png("561.png", width = 1100, height = 800)
ggarrange(E,A,D,C,G,B, ncol = 2, nrow = 3)
dev.off()

rm(list = ls())

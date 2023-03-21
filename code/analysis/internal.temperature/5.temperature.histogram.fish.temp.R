#Histogram of temperature frequency, as recorded, then standardized by 
#profile, then by site

library(readr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(trekcolors)
library(extrafont)
library(ggstatsplot)
library(grid)
library(gridExtra)
library(ggpubr)

lcars_colors()

#' Here I use the slope equation of y=mx=b as d? = D1 + (D2-D1)/(X2-X1)*(T1-X1)
#' where d? is the depth of the fish that we are trying to determine, D1 and D2 
#' the known depths of the bounding temperatures, X2 and X1 are the known temperatures
#' of those sensors, and T1 is the temperature of the fish, x is T1-X1 (pretending that
#' X1 is the y intercept for these two points...)

setwd("C:/Users/barrehan/GitHub/projects/cwa.habitat.selection")
ysi <- read.csv("data/raw.data/internal.temperature/2019.2021.thermocline.fish.internal.temp.cwa.csv")
ysi$date <- mdy(ysi$date)
fish <- read.csv("data/raw.data/internal.temperature/2019.2021.fish.internal.temp.cwa.csv")
fish$date <- mdy(fish$date)

#misrecorded do value row 90, replacing with average of bounding do values
ysi$do.mg.l[ysi$do.mg.l == "0.3"]<-6.82


ysi<-ysi[-c(154,155),]

for(i in 1:nrow(fish)){
  row <- fish[i,]
  match <- ysi[ysi$thermo.measure.loc == row$thermo.measure.loc,]
  t1 <- row$fish.temp
  x1 <- max(match$temperature.c[which(match$temperature.c < t1)])
  x2 <- min(match$temperature.c[which(match$temperature.c > t1)])
  d1 <- match$depth.m[match$temperature.c == x1]
  d2 <- match$depth.m[match$temperature.c == x2]
  d <- (d2-d1)/(x2-x1)*(t1 - x1) + d1
  depth <- ifelse(length(d > 1), mean(d), d)
  #if there are two value options take the average between the two...
  max<-match[match$depth.m == max(match$depth),]
  maxd<-max$depth.m
  depth<-ifelse(is.na(depth), maxd, depth)
  fish[i,11] <- depth
}
colnames(fish)[11] <- "depth"
colnames(fish)[9] <- "temperature"
colnames(ysi)[8] <- "depth"
colnames(ysi)[9] <- "temperature"

fish<- fish[-c(10)]

fish$thermo.measure.loc[fish$thermo.measure.loc == "br.0822.thermo.1"] <- "Blue Ruin site 1"
fish$thermo.measure.loc[fish$thermo.measure.loc == "br.0822.thermo.2"] <- "Blue Ruin site 2"
fish$thermo.measure.loc[fish$thermo.measure.loc == "br.0822.thermo.3"] <- "Blue Ruin site 3"
fish$thermo.measure.loc[fish$thermo.measure.loc == "br.0822.thermo.4"] <- "Blue Ruin site 4"
fish$thermo.measure.loc[fish$thermo.measure.loc == "cottonwood.site.1"] <- "Cottonwood site 1"
fish$thermo.measure.loc[fish$thermo.measure.loc == "cottonwood.site.4"] <- "Cottonwood site 2"
fish$thermo.measure.loc[fish$thermo.measure.loc == "harrisburg.site.1"] <- "South Harrisburg site 1"
fish$thermo.measure.loc[fish$thermo.measure.loc == "harrisburg.site.2"] <- "South Harrisburg site 2"
fish$thermo.measure.loc[fish$thermo.measure.loc == "harrisburg.site.3"] <- "South Harrisburg site 3"
fish$thermo.measure.loc[fish$thermo.measure.loc == "harrisburg.site.4"] <- "South Harrisburg site 4"
fish$thermo.measure.loc[fish$thermo.measure.loc == "nor.0827.thermo.1"] <- "Norwood site 1"
fish$thermo.measure.loc[fish$thermo.measure.loc == "nor.0827.thermo.2"] <- "Norwood site 2"

ysi$thermo.measure.loc[ysi$thermo.measure.loc == "br.0822.thermo.1"] <- "Blue Ruin site 1"
ysi$thermo.measure.loc[ysi$thermo.measure.loc == "br.0822.thermo.2"] <- "Blue Ruin site 2"
ysi$thermo.measure.loc[ysi$thermo.measure.loc == "br.0822.thermo.3"] <- "Blue Ruin site 3"
ysi$thermo.measure.loc[ysi$thermo.measure.loc == "br.0822.thermo.4"] <- "Blue Ruin site 4"
ysi$thermo.measure.loc[ysi$thermo.measure.loc == "cottonwood.site.1"] <- "Cottonwood site 1"
ysi$thermo.measure.loc[ysi$thermo.measure.loc == "cottonwood.site.4"] <- "Cottonwood site 2"
ysi$thermo.measure.loc[ysi$thermo.measure.loc == "harrisburg.site.1"] <- "South Harrisburg site 1"
ysi$thermo.measure.loc[ysi$thermo.measure.loc == "harrisburg.site.2"] <- "South Harrisburg site 2"
ysi$thermo.measure.loc[ysi$thermo.measure.loc == "harrisburg.site.3"] <- "South Harrisburg site 3"
ysi$thermo.measure.loc[ysi$thermo.measure.loc == "harrisburg.site.4"] <- "South Harrisburg site 4"
ysi$thermo.measure.loc[ysi$thermo.measure.loc == "nor.0827.thermo.1"] <- "Norwood site 1"
ysi$thermo.measure.loc[ysi$thermo.measure.loc == "nor.0827.thermo.2"] <- "Norwood site 2"
ysi<-ysi[c(1,5,9)]
ysi$case <- 0
fish<-fish[c(1,8,9)]
fish$case <-1

dater <-rbind(ysi,fish)

dater<- 
  dater %>% 
  group_by(thermo.measure.loc)%>%
  mutate(temp_prof_s = scale(temperature))%>%
  ungroup

dater<-
  dater%>%
  group_by(site)%>%
  mutate(temp_loc_s = scale(temperature))%>%
  ungroup

dens.col<-c("#02401B","#03436A","#F1BB7B","#FD6467")
colors <- c("#D8B70A", "#02401B", "#A2A475", "seagreen4", "#78B7C5","#03436A",  "#FD6467","#CB2314", "#9986A5", "#F1BB7B", "#79402E", "#D67236", "#972D15")


f1<-ggplot()+
  geom_density(data = dater[dater$case == 0,], aes(x =temp_prof_s, fill = "Available temperature"), alpha = 0.9)+
  geom_density(data = dater[dater$case == 1,], aes(x = temp_prof_s, fill = "Selected temperature"), alpha = 0.8)+ 
  scale_fill_manual(values = c("#35274A","#E1BD6D"), name = "")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 12, family = "serif"), legend.key = element_rect(fill = "transparent"))+
  xlab('Temperature (°C) standardized by profile')+
  ylab("Frequency")

ggplot()+
  geom_density(data = dater[dater$case == 0,], aes(x =temp_prof_s, fill = "Available temperature"), alpha = 0.9)+
  geom_density(data = dater[dater$case == 1,], aes(x = temp_prof_s, fill = "Selected temperature"), alpha = 0.8)+ 
  scale_fill_manual(values = c("gray40","gray80"), name = "")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 12, family = "serif"), legend.key = element_rect(fill = "transparent"))+
  xlab('Temperature (°C) standardized by profile')+
  ylab("Frequency")

f2<-ggplot()+
  geom_histogram(data = dater[dater$case == 0,], aes(x =temp_prof_s, fill= site), position = "identity", alpha = 0.5, binwidth = .15)+
  geom_jitter(data = dater[dater$case == 1,], aes(x = temp_prof_s, y = 1, color = thermo.measure.loc), height = .01)+ 
  scale_fill_manual(values = dens.col, name = "")+
  scale_color_manual(values = colors, name = "")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 12, family = "serif"), legend.key = element_rect(fill = "transparent"))+
  xlab('Temperature (°C) standardized by profile')+
  ylab("Frequency")

f3<-ggplot()+
  geom_histogram(data = dater[dater$case == 0,], aes(x =temp_prof_s), position = "identity", alpha = 0.5, binwidth = .15)+
  geom_jitter(data = dater[dater$case == 1,], aes(x = temp_prof_s, y = 1, color = thermo.measure.loc), height = .05)+ 
  #scale_fill_manual(values = dens.col, name = "")+
  scale_color_manual(values = colors, name = "")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 12, family = "serif"), legend.key = element_rect(fill = "transparent"))+
  xlab('Temperature (°C) standardized by profile')+
  ylab("Frequency")


f4<-ggplot()+
  geom_histogram(data = dater[dater$case == 0,], aes(x =temp_prof_s), position = "identity", alpha = 0.5, binwidth = .15)+
  geom_density(data = dater[dater$case == 1,], aes(x = temp_prof_s), fill = "#78B7C5")+ 
  #scale_fill_manual(values = dens.col, name = "")+
  scale_color_manual(values = colors, name = "")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 12, family = "serif"), legend.key = element_rect(fill = "transparent"))+
  xlab('Temperature (°C) standardized by profile')+
  ylab("Frequency")



fig1<-ggplot()+
  geom_histogram(data = dater[dater$case == 0,], aes(x =temperature))+
  geom_point(data = dater[dater$case == 1,], aes(x = temperature, y = 1))+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 12, family = "serif"))+
  xlab('Temperature (°C)')+
  ylab("Frequency")



fig3<-ggplot(ysi, aes(temp_loc_s))+
  geom_histogram()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 12, family = "serif"))+
  xlab('Temperature (°C) standardized by location')+
  ylab("Frequency")

figure <-ggarrange(f1, f2, f3,
                   ncol = 1, nrow =3,
                   common.legend = T,
                   legend = "right")


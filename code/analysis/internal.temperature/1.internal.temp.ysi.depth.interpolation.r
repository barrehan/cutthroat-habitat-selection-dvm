#'2022-11-09

# Interpolation to determine fish depth using internal temperatures -------

library(readr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(trekcolors)
library(extrafont)

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

# #fish$thermo.measure.loc <- factor(fish$thermo.measure.loc, levels = c("Blue Ruin site 1", "Blue Ruin site 2",
#                                                                       "Blue Ruin site 3", "Blue Ruin site 4",
#                                                                       "Harrisburg site 1", "Harrisburg site 2",
#                                                                       "Harrisburg site 3", "Harrisburg site 4",
#                                                                       "Cottonwood site 1", "Cottonwood site 4",
#                                                                       "Norwood site 1", "Norwood site 2"))
# ysi$thermo.measure.loc <- factor(ysi$thermo.measure.loc, levels = c("Blue Ruin site 1", "Blue Ruin site 2",
#                                                                     "Blue Ruin site 3", "Blue Ruin thermocline 4",
#                                                                     "Harrisburg thermocline 1", "Harrisburg thermocline 2",
#                                                                     "Harrisburg thermocline 3", "Harrisburg thermocline 4",
#                                                                     "Cottonwood thermocline 1", "Cottonwood thermocline 4",
#                                                                     "Norwood thermocline 1", "Norwood thermocline 2"))



#' two fish temperatures at the harrisburg alcove were below the lowest temperature 
#' recorded using the ysi so these fish were likely on a different thermocline and
#' will be excluded from the plots below

colors <- c("#D8B70A", "#02401B", "#A2A475", "seagreen4", "#78B7C5","#03436A",  "#FD6467","#CB2314", "#9986A5", "#F1BB7B", "#79402E", "#D67236", "#972D15")

ggplot()+
geom_point(data = ysi, aes(x = temperature, y = depth, color = "YSI profile"), color = "#273046",alpha = 0.5)+
  geom_point(data = fish, aes(x = temperature, y = depth, color = thermo.measure.loc),alpha = 0.75, size = 3, show.legend =F)+
  scale_color_manual(values = colors)+
  facet_wrap(~thermo.measure.loc)+
  scale_y_reverse()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 12, family = "serif"))+
  xlab('Temperature (°C)')+
  ylab("Depth (m)")



ggplot(NULL, aes(temperature, depth)) +                 
  geom_point(data = ysi,
             col = "gray58", alpha = 0.5,
             size = 2) +
  geom_point(data = fish,
             col = thermo.measure.loc, alpha = 0.75,
             size = 4)+
  facet_wrap(~thermo.measure.loc)+
  scale_y_reverse()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 12, family = "serif"))+
  xlab('Temperature °C)')+
  ylab("Depth (m)")

#ggsave("results/figures/internal.temperature/internal.temp.with.thermocline.jpg", width = 14, height = 8)

#' now using the same equation using the fish depth to determine DO at that location
#' and time. we were stupid and didn't use the YSI at norwood so we do not have DO 
#' measurements there...

for(i in 1:nrow(fish)){
  row <- fish[i,]
  match <- ysi[ysi$thermo.measure.loc == row$thermo.measure.loc,]
  t1 <- row$depth 
  x1 <- max(match$depth[which(match$depth < t1)])
  x2 <- min(match$depth[which(match$depth> t1)])
  d1 <- match$do.mg.l[match$depth == x1]
  d1<-ifelse(length(d1>1),mean(d1), d)
  d2 <- match$do.mg.l[match$depth == x2]
  d2 <- ifelse(length(d2>1), mean(d2), d2)
  d <- (d2-d1)/(x2-x1)*(t1 - x1) + d1
  do <- ifelse(length(d > 1), mean(d), d)
  #if there are two value options take the average between the two...
  max<-match[match$depth == max(match$depth),]
  mindo<-max$do.mg.l
  do<-ifelse(is.na(do), mindo, do)
  fish[i,11] <- do
}
colnames(fish)[11] <- "dissolved.oxygen"
colnames(ysi)[10]<- "dissolved.oxygen"

#' harrisburg fish that couldn't have depth assigned also could not have
#' DO assigned, as well as norwood fish where we did not measure DO
#' because of some stupid reason we likely justified in the field...I regret

ggplot()+
  geom_point(data = ysi, aes(x = dissolved.oxygen, y = depth, color = "YSI profile"), color = "#273046",alpha = 0.5)+
  geom_point(data = fish, aes(x = dissolved.oxygen, y = depth, color = thermo.measure.loc),alpha = 0.75, size = 3, show.legend =F)+
  scale_color_manual(values = colors)+
  facet_wrap(~thermo.measure.loc)+
  scale_y_reverse()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 12, family = "serif"))+
  xlab('Dissolved oxygen (mg/L)')+
  ylab("Depth (m)")

ggplot(NULL, aes(dissolved.oxygen, depth)) +                 
  geom_point(data = ysi,
             col = "#AA5533", alpha = 0.5,
             size = 2) +
  geom_point(data = fish,
             col = "#882211", alpha = 0.75,
             size = 4)+
  facet_wrap(~thermo.measure.loc)+
  scale_y_reverse()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 12, family = "serif"))+
  xlab("Dissolved oxygen (mg/L)")+
  ylab('Depth (m)')

#ggsave("figures/internal.temperature.figures/choke.poke.do.vs.depth.jpg", width = 14, height = 8)


#' now plotting DO versus Temperature for each location with fish included

ggplot()+
  geom_point(data = ysi, aes(x = dissolved.oxygen, y = temperature, color = "YSI profile"), color = "#273046",alpha = 0.5)+
  geom_point(data = fish, aes(x = dissolved.oxygen, y = temperature, color = thermo.measure.loc),alpha = 0.75, size = 3, show.legend =F)+
  scale_color_manual(values = colors)+
  facet_wrap(~thermo.measure.loc)+
  scale_y_reverse()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 12, family = "serif"))+
  xlab('Dissolved oxygen (mg/L)')+
  ylab("Temperature (Â°C)")

ggplot(NULL, aes(dissolved.oxygen, temperature)) +                 
  geom_point(data = ysi,
             col = "#AA5533", alpha = 0.5,
             size = 2) +
  geom_point(data = fish,
             col ="#882211", alpha = 0.75,
             size = 4)+
  facet_wrap(~thermo.measure.loc)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 12, family = "serif"))+
  xlab("Dissolved oxygen (mg/L)")+
  ylab("Temperature (Â°C)")

#ggsave("figures/internal.temperature.figures/choke.poke.do.vs.temp.jpg", width = 14, height = 8)

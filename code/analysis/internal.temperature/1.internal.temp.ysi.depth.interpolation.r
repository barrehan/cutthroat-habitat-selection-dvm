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
  fish[i,11] <- depth
}
colnames(fish)[11] <- "depth"
colnames(fish)[9] <- "temperature"
colnames(ysi)[8] <- "depth"
colnames(ysi)[9] <- "temperature"

fish<- fish[-c(10)]

fish$thermo.measure.loc[fish$thermo.measure.loc == "br.0822.thermo.1"] <- "Blue Ruin thermocline 1"
fish$thermo.measure.loc[fish$thermo.measure.loc == "br.0822.thermo.2"] <- "Blue Ruin thermocline 2"
fish$thermo.measure.loc[fish$thermo.measure.loc == "br.0822.thermo.3"] <- "Blue Ruin thermocline 3"
fish$thermo.measure.loc[fish$thermo.measure.loc == "br.0822.thermo.4"] <- "Blue Ruin thermocline 4"
fish$thermo.measure.loc[fish$thermo.measure.loc == "cottonwood.site.1"] <- "Cottonwood thermocline 1"
fish$thermo.measure.loc[fish$thermo.measure.loc == "cottonwood.site.4"] <- "Cottonwood thermocline 4"
fish$thermo.measure.loc[fish$thermo.measure.loc == "harrisburg.site.1"] <- "Harrisburg thermocline 1"
fish$thermo.measure.loc[fish$thermo.measure.loc == "harrisburg.site.2"] <- "Harrisburg thermocline 2"
fish$thermo.measure.loc[fish$thermo.measure.loc == "harrisburg.site.3"] <- "Harrisburg thermocline 3"
fish$thermo.measure.loc[fish$thermo.measure.loc == "harrisburg.site.4"] <- "Harrisburg thermocline 4"
fish$thermo.measure.loc[fish$thermo.measure.loc == "nor.0827.thermo.1"] <- "Norwood thermocline 1"
fish$thermo.measure.loc[fish$thermo.measure.loc == "nor.0827.thermo.2"] <- "Norwood thermocline 2"

ysi$thermo.measure.loc[ysi$thermo.measure.loc == "br.0822.thermo.1"] <- "Blue Ruin thermocline 1"
ysi$thermo.measure.loc[ysi$thermo.measure.loc == "br.0822.thermo.2"] <- "Blue Ruin thermocline 2"
ysi$thermo.measure.loc[ysi$thermo.measure.loc == "br.0822.thermo.3"] <- "Blue Ruin thermocline 3"
ysi$thermo.measure.loc[ysi$thermo.measure.loc == "br.0822.thermo.4"] <- "Blue Ruin thermocline 4"
ysi$thermo.measure.loc[ysi$thermo.measure.loc == "cottonwood.site.1"] <- "Cottonwood thermocline 1"
ysi$thermo.measure.loc[ysi$thermo.measure.loc == "cottonwood.site.4"] <- "Cottonwood thermocline 4"
ysi$thermo.measure.loc[ysi$thermo.measure.loc == "harrisburg.site.1"] <- "Harrisburg thermocline 1"
ysi$thermo.measure.loc[ysi$thermo.measure.loc == "harrisburg.site.2"] <- "Harrisburg thermocline 2"
ysi$thermo.measure.loc[ysi$thermo.measure.loc == "harrisburg.site.3"] <- "Harrisburg thermocline 3"
ysi$thermo.measure.loc[ysi$thermo.measure.loc == "harrisburg.site.4"] <- "Harrisburg thermocline 4"
ysi$thermo.measure.loc[ysi$thermo.measure.loc == "nor.0827.thermo.1"] <- "Norwood thermocline 1"
ysi$thermo.measure.loc[ysi$thermo.measure.loc == "nor.0827.thermo.2"] <- "Norwood thermocline 2"

fish$thermo.measure.loc <- factor(fish$thermo.measure.loc, levels = c("Blue Ruin thermocline 1", "Blue Ruin thermocline 2",
                                                                      "Blue Ruin thermocline 3", "Blue Ruin thermocline 4",
                                                                      "Harrisburg thermocline 1", "Harrisburg thermocline 2",
                                                                      "Harrisburg thermocline 3", "Harrisburg thermocline 4",
                                                                      "Cottonwood thermocline 1", "Cottonwood thermocline 4",
                                                                      "Norwood thermocline 1", "Norwood thermocline 2"))
ysi$thermo.measure.loc <- factor(ysi$thermo.measure.loc, levels = c("Blue Ruin thermocline 1", "Blue Ruin thermocline 2",
                                                                    "Blue Ruin thermocline 3", "Blue Ruin thermocline 4",
                                                                    "Harrisburg thermocline 1", "Harrisburg thermocline 2",
                                                                    "Harrisburg thermocline 3", "Harrisburg thermocline 4",
                                                                    "Cottonwood thermocline 1", "Cottonwood thermocline 4",
                                                                    "Norwood thermocline 1", "Norwood thermocline 2"))



#' two fish temperatures at the harrisburg alcove were below the lowest temperature 
#' recorded using the ysi so these fish were likely on a different thermocline and
#' will be excluded from the plots below

ggplot(NULL, aes(temperature, depth)) +                 
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
  xlab('Temperature (°C)')+
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
  fish[i,11] <- do
}
colnames(fish)[11] <- "dissolved.oxygen"
colnames(ysi)[10]<- "dissolved.oxygen"

#' harrisburg fish that couldn't have depth assigned also could not have
#' DO assigned, as well as norwood fish where we did not measure DO
#' because of some stupid reason we likely justified in the field...I regret



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
  ylab("Temperature (°C)")

#ggsave("figures/internal.temperature.figures/choke.poke.do.vs.temp.jpg", width = 14, height = 8)

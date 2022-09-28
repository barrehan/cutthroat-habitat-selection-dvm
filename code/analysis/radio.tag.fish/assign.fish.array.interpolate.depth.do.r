library(readr)
library(tidyverse)
library(lubridate)
library(dplyr)
library(data.table)
library(rgdal)

setwd("C:/Users/barrehan/Box/projects/2021.alcove.DO.project")

#'assign fish location (receiver site/antenna number) to closest DO/temp array
#'use temperature to interoplate depth and DO for that timestep

fish.dat <- read.csv('data/radio.tag.data/tag.reads.10.min.interval.csv')
fish.dat$date.time = mdy_hm(fish.dat$date.time)
receiver.dat <- unique(fish.dat[c("receiver.site", "antenna.number", "lat", 'long')])
receiver.dat<- receiver.dat %>%arrange(receiver.site, antenna.number)
receiver.dat$ID <- 1:nrow(receiver.dat)

logger.dat<- read.csv('data/temp.do.data/logger.array.setup.csv')
logger.dat<- logger.dat[!(logger.dat$site == "telemetry.slough"),]
logger.dat <- logger.dat[-c(1,3:7,10:12)]
logger.dat<-unique(logger.dat[c(1:3)])

#plot all points on map

br.poly <- readOGR("data/blue.ruin.polygon/brpolygon.shp")

##figure out what the projection is so I can use it for my radio tag data below

br.poly@proj4string

plot(br.poly, axes = TRUE)

plot(br.poly, height = 700, width = 900) 


##you have to fortify your shapefile to make it work with ggplot2

br.poly <- fortify(br.poly)

# Now the shapefile can be plotted as either a geom_path or a geom_polygon.
# Paths handle clipping better. Polygons can be filled.
# You need the aesthetics long, lat, and group.

receiver.dat$ID<-as.factor(receiver.dat$ID)

ggplot() +
  geom_polygon(data = br.poly, 
               aes(x = long, y = lat, group = group), fill = "light grey")
colors <- c("#D8B70A", "#D67236", "#02401B", "#A2A475", "#81A88D", "#972D15")

map.points <- ggplot() +
  geom_polygon(data =br.poly, 
               aes(x = long, y = lat, group = group), fill = "light grey") +
  geom_jitter(data = receiver.dat, 
             aes(x=long, y = lat, colour = ID), width = 0.00001, size = 2, shape = 16)+
  scale_color_manual(values = colors)+
  geom_point(data = logger.dat, 
             aes(x = lon, y = lat), size = 2, shape = 6)+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  labs(color = "Antenna")
  
ggsave("figures/radio.tag.map.figures/blue.ruin.arrays.receivers.png", width = 15, height = 10, units = "cm")
  

             
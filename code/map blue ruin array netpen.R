#'this script creates a figure of the blue ruin polygon with labeled
#'triangles marking logger arrays and points marking net pen locations

#'packages used
library(readr)
library(tidyverse)
library(lubridate)
library(dplyr)
library(data.table)
library(rgdal)
library(cowplot) #grid ggplots
library(viridis)
library(ggrepel)
#'clear workspace
rm(list = ls())
#close open graphics devices
graphics.off() 

# Create Blue Ruin polygon with points for arrays ---------------

logger.dat<- read.csv('data/raw.data/logger.array/logger.array.setup.deployment.metadata.csv')
logger.dat<- logger.dat[!(logger.dat$site == "telemetry.slough"),]
logger.dat <- logger.dat[-c(1,3:7,10:12)]
logger.dat<-unique(logger.dat[c(1:3)])
logger.dat <-logger.dat[-c(1),]

# Add in lat lon data for netpens
location <- c("net pen 1", "net pen 2")
lat <-c(44.23025, 44.23015)
lon <- c(-123.162431, -123.162403)

netpen <-data.frame(location, lat, lon)

#plot all points on map
br.poly <- readOGR("data/modif.data/blue.ruin.polygon/brpolygon.shp")

##figure out what the projection is
br.poly@proj4string

plot(br.poly, axes = TRUE)

plot(br.poly, height = 700, width = 900) 


##you have to fortify your shapefile to make it work with ggplot2
br.poly <- fortify(br.poly)

# Now the shape file can be plotted as either a geom_path or a geom_polygon.
# Paths handle clipping better. Polygons can be filled.
# You need the aesthetics long, lat, and group.


ggplot() +
  geom_polygon(data = br.poly, 
               aes(x = long, y = lat, group = group), fill = "light grey")

map<- ggplot() +
  geom_polygon(data =br.poly, 
               aes(x = long, y = lat, group = group), fill = "light grey") +
  geom_point(data = logger.dat, 
             aes(x = lon, y = lat, colour = "logger array"), size = 1.5, shape = 2)+
  scale_color_manual(values=  c("black"))+
  geom_text_repel(aes(x = -123.1630, y =44.23229), label = "1", 
                   nudge_x = .00014, family = "serif")+
  geom_text_repel(aes(x = -123.1628, y =44.23156), label = "2", 
                  nudge_x = .00015, family = "serif")+
  geom_text_repel(aes(x = -123.1626, y =44.23052), label = "3", 
                  nudge_x = .00015, family = "serif")+
  geom_text_repel(aes(x = -123.1624, y =44.23019), label = "4", 
                  nudge_x = .0001, family = "serif")+
  geom_text_repel(aes(x = -123.1623, y =44.23003), label = "5", 
                  nudge_x = .00012, family = "serif")+
  geom_point(data = netpen, aes (x = lon, y = lat), shape = 20, size = 2)+
  geom_text_repel(data = netpen, aes(x = lon, y = lat, label = location), 
                  nudge_x = c(.0005, -.0005), nudge_y = c(0.0005, -0.0005),
                  family = "serif")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.key=element_rect(fill="white"), text = element_text(size = 15, family = "serif"),
        legend.title=element_blank())+
  xlab(label = "longitude") +
  ylab (label = "latitude")
 

ggsave(map, file = paste0("results/figures/logger.array/blue.ruin.logger.array.map.png"), 
       width = 18, height = 15, units = "cm")

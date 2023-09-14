# plot for norwood logger array
library(readr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(RColorBrewer)
library(viridis)
library(scales)

setwd("C:/Users/barrehan/GitHub/projects/cwa.habitat.selection.dvm")

array <- read.csv("data/raw.data/logger.array/norwood.mouth.temp.do.csv")
unique(array$sensor.depth)
array$sensor.depth<- as.factor(array$sensor.depth)
array$date.time<- mdy_hm(array$date.time) 
array <- array %>% force_tz(array$date.time, tzone = "America/Los_Angeles")
array<- array[array$date.time >="2021-07-31 00:00:00",]
array<- array[array$date.time < "2021-08-07 00:00:00",]


pp<-ggplot(data= array, aes(x = date.time))+
  geom_jitter(aes(y=temperature, colour = sensor.depth))+
  geom_jitter(aes(y=dissolved.oxygen, colour = sensor.depth))+
  scale_colour_manual(values = c("goldenrod2", "aquamarine3", "deeppink4", "darkviolet", "darkorange", "dark blue",  "red"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_x_datetime(date_labels = "%b %d", date_breaks = "2 days")+
  xlab(label = "Date") +
  ylab(label = "")+
  theme(text = element_text(size = 15), legend.position = "right")

# ggsave(pp, file=paste0("results/figures/logger.array/norwood.temp.do.depth.time.png"), width = 50, height = 30, units = "cm") 

# pull 24-hour window, August 2, reduce to 3 do loggers

array24<- array[array$date.time >="2021-08-01 00:00:00" & array$date.time <= "2021-08-02 00:00:00",]
array24 <- array24 %>%drop_na(dissolved.oxygen)
array24$shp1 <- as.factor(1)
array24$shp2 <- as.factor(2)

lims <- strptime(c("00:00:00","00:00:00"), format = "%H")

p<- ggplot()+
  geom_line(data= array24, stat = "smooth", method = "loess", aes(x = date.time, y=temperature, colour = sensor.depth), linetype = "dashed", se = F, size = 0.85)+
  geom_line(data= array24, stat = "smooth", method = "loess", aes(x = date.time, y=dissolved.oxygen, colour = sensor.depth), linetype = "solid", se = F, size = 0.85)+
  scale_colour_manual(labels = c("0.25m", "0.85m", "1.45m"), values = c("#212E52", "#386EC2", "#8087AA"),
                      name = "Sensor depth")+
  scale_linetype_manual(values= c("dashed", "solid"), labels = c("Temperature (\u00B0C)", "Dissolved oxygen (mg/L)"), name = "Metric")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.key=element_rect(fill="white"), text = element_text(size = 12, family = "serif"))+
  scale_x_datetime(breaks = breaks_width("2 hours"), date_labels = "%H")+
  xlab(label = "Hour of the day") +
  ylab(label = "")+
  theme(text = element_text(size = 15), legend.position = "right")
  ggsave(p, filename = paste("results/figures/logger.array/norwood.24hr.temp.do.png"), width = 16, height = 10, units = "cm")



# Just look at DO determine min/max windows -------------------------------

array$hourID <-hour(array$date.time)
array$hourID <- as.factor(array$hourID)
#discrete 24 hour color selection 
c24 <- c("dodgerblue2", "#E31A1C", "green4","#6A3D9A", "#FF7F00", "black", "gold1","skyblue2", "#FB9A99", "palegreen2", "#CAB2D6","#FDBF6F", "gray70", "khaki2", "maroon","orchid1", "deeppink1", "blue1", "steelblue4", "darkturquoise", "green1", "yellow4","yellow3", "darkorange4","brown")

p<-ggplot(data= array, aes(x = date.time, y = dissolved.oxygen, colour = hourID))+
  geom_jitter()+
  scale_colour_manual(values = c24)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_x_datetime(date_labels = "%b %d", date_breaks = "2 days")+
  xlab(label = "Date") +
  ylab(label = "")+
  theme(text = element_text(size = 15), legend.position = "right")

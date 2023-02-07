rm(list=ls())

Sys.setenv(TZ = "America/Los_Angeles")

library(ggplot2)
library(tidyverse)
library(lme4)
library(viridis)
library(dplyr)
library(tidyverse)
library(lubridate)
library(chron)

setwd("C:/Users/barrehan/GitHub/projects/cwa.habitat.selection")

fish <-read.csv("data/modif.data/hab.select.mod/br.ibutton.data/br.ibutton.pooled.csv")
fish$date.time<-ymd_hms(fish$date.time)
fish <-fish[fish$case ==1,]
fish<-fish[,c(1:4,6)]
fish$hour <-hour(fish$date.time)
fish$date <-as.Date(fish$date.time)
fish$time <-fish$date.time
fish$time <- format(as.POSIXct(fish$date.time), format = "%H:%M:%S")
fish$fake.date <- "2021-01-01"
fish$time2 <- paste(fish$fake.date, fish$time)
fish$time2 <-ymd_hms(fish$time2)

buttons <-unique(fish$ibutton.id)
dates <- unique(fish$date)

dat <-setNames(data.frame(matrix(ncol = 8, nrow = 0)), c("date.time", "temperature", "depth","dissolved.oxygen", "ibutton.id", "hour", "date", "min.max"))

for(i in 1:length(buttons)){
  ib<-buttons[i]
  f.match <- fish[fish$ibutton.id == ib,]
  for(j in 1:length(dates)){
    dt<-dates[j]
    dt.match <-f.match[f.match$date == dt,]
    if(!nrow(dt.match)){next}
    deep.row <- which(dt.match$depth == max(dt.match$depth, na.rm = TRUE)) # row index for 'max slope' (all slopes negative so max is closest to 0 slope)
    deep<-dt.match[deep.row,] # return row(s)
    deep$min.max <-"deepest"
    shal.row <-which(dt.match$depth == min(dt.match$depth, na.rm = TRUE))
    shal<-dt.match[shal.row,]
    shal$min.max <- "shallowest"
    
    new <-rbind(deep,shal)
    dat<-rbind(new,dat)
    
  }

}


shallow <-dat[dat$min.max == "shallowest",]
deep <- dat[dat$min.max == "deepest",]

ggplot(data = shallow, aes(x =hour))+
  geom_histogram(binwidth = 1, boundary = -7.5, colour = "black", fill = "lightsteelblue",size = .2)+
  coord_polar()+
  scale_x_continuous(limits = c(0, 24),
                     breaks = seq(0,24, by = 4),
                     minor_breaks = seq(0,24, by = 1))+
  theme_bw()+
  theme(panel.border = element_blank())+
  ggtitle("Daily timing most shallow water column position")

ggplot(data = deep, aes(x =hour))+
  geom_histogram(binwidth = 1, boundary = -7.5, colour = "black", fill = "red4",size = .2)+
  coord_polar()+
  scale_x_continuous(limits = c(0, 24),
                     breaks = seq(0,24, by = 4),
                     minor_breaks = seq(0,24, by = 1))+
  theme_bw()+
  theme(panel.border = element_blank())+
  ggtitle("Daily timing deepest water column position")

ggplot(data = fish, aes (x = time2, y = depth))+
  geom_point()+
  geom_smooth(colour = "seagreen4", fill = "burlywood")+
  scale_y_reverse()+
  scale_x_datetime(
    breaks = "2 hours",
    date_labels = "%H")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  ggtitle("Smoothed fit fish depth across 24-hour period, Blue Ruin netpens")+
  xlab("Time")+
  ylab("Depth (m)")

  


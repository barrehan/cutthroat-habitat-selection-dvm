library(readr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggpubr)

setwd("C:/Users/barrehan/Box/projects/2021.alcove.DO.project")

ysi <- read.csv("data/choke.poke/2021.choke.poke.coordinates.csv")

fish <- read.csv("data/choke.poke/2021.choke.poke.csv")

cottonwood.ysi <- ysi[ysi$site == "cottonwood.alcove",]
harrisburg.ysi <- ysi[ysi$site == "harrisburg.alcove",]

cottonwood.fish <- fish[fish$site == "cottonwood.alcove",]
harrisburg.fish <- fish[fish$site == "harrisburg.alcove",]

a <- ggplot(data = cottonwood.ysi, aes(x = temperature.c, y = do.mg.l))+
  geom_jitter(aes(shape = location, colour = location))+
  scale_color_manual(values = c("cottonwood.site.1" = "black", "cottonwood.site.4" = "red"))+
  scale_x_continuous(limits = c(14, 20))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))

b <- ggplot(data = cottonwood.fish, aes(x = fish.internal.temp.c, y= location))+
  geom_boxplot(aes(colour = location))+
  scale_x_continuous(limits = c(14, 20))+
  scale_color_manual(values = c("cottonwood.site.1" = "black", "cottonwood.site.4" = "red"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


c <- ggplot(data = harrisburg.ysi, aes(x = temperature.c, y = do.mg.l))+
  geom_jitter(aes(shape = location, colour = location))+
  scale_x_continuous(limits = c(14, 20))+
  scale_color_manual(values = c("harrisburg.site.1" = "black", "harrisburg.site.2" = "red", 
                                "harrisburg.site.3" = "blue", "harrisburg.site.4" = "green"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

d <- ggplot(data = harrisburg.fish, aes(x = fish.internal.temp.c, y= location))+
  geom_boxplot(aes(colour = location))+
  scale_x_continuous(limits = c(14, 20))+
  scale_color_manual(values = c("harrisburg.site.1" = "black", "harrisburg.site.2" = "red", 
                                "harrisburg.site.3" = "blue", "harrisburg.site.4" = "green"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank())
 
ggarrange(a, b, c, d,nrow = 4, align = "v")
          



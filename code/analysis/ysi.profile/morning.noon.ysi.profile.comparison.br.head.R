#do/temp profiles from blue ruin terminus early morning and late afternoon
#figure 1 for dvm paper

#TO DO: interpolate between points for noon reading to estimate temp and do
#at 0.1 m intervals
rm(list=ls())

library(readr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)

setwd("C:/Users/barrehan/GitHub/projects/cwa.habitat.selection.dvm")

br<-read.csv("data/raw.data/ysi.profile/br.netpen.ysi.profile.csv")

br$day.segment <-as.factor(br$day.segment)

log5 <- br[br$location == "site.5.logger.5",]

ggplot(log5, aes(x = temp.c, y = depth.m, col = day.segment))+
  geom_point()+
  scale_y_reverse()+
  scale_color_manual(values = c("#664466", "#FFCC99"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "right", legend.key = element_rect(fill = "transparent"),
        text = element_text(size = 12, family = "serif"), legend.title = element_blank())+
  ylab("Depth (m)")+
  xlab("Dissolved oxygen (mg/L)")

ggplot(log5, aes(x = do.mg.l, y = depth.m, col = day.segment))+
  geom_point()+
  scale_y_reverse()+
  scale_color_manual(values = c("#664466", "#FFCC99"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "right", legend.key = element_rect(fill = "transparent"),
        text = element_text(size = 12, family = "serif"), legend.title = element_blank())+
  ylab("Depth (m)")+
  xlab("Temperature (\u00B0C)")


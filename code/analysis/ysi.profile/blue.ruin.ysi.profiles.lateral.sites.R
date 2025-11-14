library(readr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(grid)

setwd("C:/Users/barrehan/Documents/GitHub/cwa.habitat.selection.dvm")
ysi <- read.csv("data/raw.data/ysi.profile/2021.ysi.profiles.csv")

noon <- ysi[ysi$day.segment == "noon",]
morning <- ysi[ysi$day.segment == "morning",]

ggplot(data= morning, aes(x = do.mg.l, y = depth.m, colour = location), group_by = location)+
  geom_point()+
  scale_y_reverse()+
  facet_wrap(~location)+
  ylab("depth (m)")
  xlab("temperature C")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 20))
  
max.dn <- noon %>%
  group_by(location)%>%
  top_n(1, depth.m)

max.dm <- morning %>%
  group_by(location)%>%
  top_n(1, depth.m)

max.dme <- morning %>%
  group_by(location)%>%
  top_n(-1, depth.m)
  
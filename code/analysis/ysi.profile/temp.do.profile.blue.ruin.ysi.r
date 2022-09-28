library(readr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(grid)

setwd("C:/Users/barrehan/Box/projects/2021.alcove.DO.project")
ysi <- read.csv("data/temp.do.data/ysi.profile.data/br.netpen.ysi.profile.csv")

netpen<- ysi[ysi$location == "site.4.netpen",]
noon <- netpen[netpen$day.segment == "noon",]
morning <- netpen[netpen$day.segment == "morning",]

p1 <-ggplot(data= noon, aes(x = temp.c, y = depth.m))+
  geom_point(colour = "#00AFBB", size =5 )+
  scale_y_reverse()+
  ylab("depth (m)")+
  xlab("temperature °C")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 20))

p2<-ggplot(data= noon, aes(x = do.mg.l, y = depth.m))+
  geom_point(colour = "#E7B800", size = 5)+
  scale_y_reverse()+
  ylab("depth (m)")+
  xlab("dissolved oxygen mg/l")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 20))
                                                        


p <- grid.arrange(p1, p2, nrow =1, top =textGrob("Blue ruin alcove temp/do profiles afternoon", gp=gpar(fontsize = 20)))
ggsave(p, file=paste0("figures/ibutton.figures/blue.ruin.temp.do.profile.png"), width = 50, height = 30, units = "cm") 

netpen$time<- as.factor(netpen$time)
netpen$time <- factor(netpen$time, levels = rev(levels(netpen$time)))


p3 <-ggplot(data= netpen, aes(x = temp.c, y = depth.m, colour = time))+
  geom_point(size = 5)+
  scale_colour_manual(values = c("#FC4E07", "#00AFBB"))+
  scale_y_reverse()+
  ylab("depth (m)")+
  xlab("temperature °C")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 20))

p4<-ggplot(data= netpen, aes(x = do.mg.l, y = depth.m, colour = time))+
  geom_point(size = 5)+
  scale_colour_manual(values = c("purple", "#E7B800"))+ 
  scale_y_reverse()+
  ylab("depth (m)")+
  xlab("dissolved oxygen mg/l")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 20))

pp<- grid.arrange(p3, p4, nrow =1, top =textGrob("Blue ruin alcove temp/do profiles morning/afternoon", gp=gpar(fontsize = 20)))

ggsave(pp, file=paste0("figures/ibutton.figures/blue.ruin.temp.do.profile.morning.noon.png"), width = 50, height = 30, units = "cm") 

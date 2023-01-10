rm(list=ls())
library(readr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(data.table)
library(ggpubr)

setwd("C:/Users/barrehan/GitHub/projects/cwa.habitat.selection")

fish <- read.csv("data/modif.data/radio.tag/tags.all.arrays.daytime.temp.select.csv")

#'reduce df to daytime only (10:00am-22:00pm)

fish$hour <- hour(fish$date.time)

dt<-fish[fish$hour >= 09 & fish$hour <=14,]

#'standardize temperature by each stratID, e.g. for the range of available
#'temperatures at a logger site, what is the fish selecting - 
#' group by stratID, standardize temp

# case <- dt[dt$stratID == 8,]
# case$stnd<-scale(case$temperature)

dt<- dt %>%
  group_by(stratID)%>%
  mutate(standardized.temp = scale(temperature))

tags<- unique(dt$tag.id)

dt$stratID<-as.factor(dt$stratID)

for(i in tags){
  
  t0<-dt[dt$tag.id == i,] #df for tag
  t1<-t0[t0$case == 1,] #df for tag when case =1 (fish temp selected)
  
  f0<-ggplot()+
    geom_boxplot(data = t0, aes(x = stratID, y = standardized.temp))+
    geom_point(data = t1, aes(x = stratID, y = standardized.temp), color = 'red')+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
  f1 <-ggplot()+
    geom_boxplot(data = t0, aes(x = stratID, y = temperature))+
    geom_point(data = t1, aes(x = stratID, y = temperature), color = 'red')+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
  figure <- ggarrange(f0, f1 + rremove("x.text"),
                      ncol = 1, nrow = 2)
  
  fig<-annotate_figure(figure,
                       top = text_grob(i, face = "bold", size = 16),
                       fig.lab = "Blue Ruin radio tag fish temperature vs available, 09:00am-2:00pm")
  #ggsave(ibutton.plot, file=paste0("figures/ibutton.figures/br.netpen1/br.netpen1.ibutton.", i,".png"), width = 20, height = 10, units = "cm")
  ggsave(fig, file = paste0("results/figures/radio.tag.figures/temp.select.daytime/daytime.temp.all.arrays/br.rt.", i,".temp.select.daytime.jpg"), width = 12, height = 8, units = "in")
  
}

dt.netpen <- fish[fish$logger.site == "site.4.netpen",]
dt.netpen$stratID <- as.factor(dt.netpen$stratID)


dt.netpen<- dt.netpen %>%
  group_by(stratID)%>%
  mutate(standardized.temp = scale(temperature))

for(i in tags){
  
  t0<-dt.netpen[dt.netpen$tag.id == i,] #df for tag
  t1<-t0[t0$case == 1,] #df for tag when case =1 (fish temp selected)
  
  f0<-ggplot()+
    geom_boxplot(data = t0, aes(x = stratID, y = standardized.temp))+
    geom_point(data = t1, aes(x = stratID, y = standardized.temp), color = 'red')+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
  f1 <-ggplot()+
    geom_boxplot(data = t0, aes(x = stratID, y = temperature))+
    geom_point(data = t1, aes(x = stratID, y = temperature), color = 'red')+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
  figure <- ggarrange(f0, f1 + rremove("x.text"),
                      ncol = 1, nrow = 2)
  
  fig<-annotate_figure(figure,
                       top = text_grob(i, face = "bold", size = 16),
                       fig.lab = "RT near netpen array, 12:00am-4:00pm")
  #ggsave(ibutton.plot, file=paste0("figures/ibutton.figures/br.netpen1/br.netpen1.ibutton.", i,".png"), width = 20, height = 10, units = "cm")
  ggsave(fig, file = paste0("results/figures/radio.tag.figures/temp.select.daytime/daytime.temp.all.arrays/br.rt.", i,".temp.select.daytime.netpen.jpg"), width = 12, height = 8, units = "in")
  
}
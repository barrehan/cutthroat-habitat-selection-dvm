#' 01/15/2023
#' Analyze foray files to see if there is any pattern to when fish exit/return
#' to BR alcove

library(plyr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(pals) #color palettes discrete colors


#'clear workspace
rm(list = ls())
#close open graphics devices
graphics.off() 

data_all <- list.files(path = "C:/Users/barrehan/GitHub/projects/cwa.habitat.selection.dvm/data/modif.data/radio.tag/mainstem.foray/foray.deets", pattern = "*.csv", full.names = TRUE) 
csv<-lapply(data_all, read.csv)  
results<-do.call(rbind,csv)

setwd("C:/Users/barrehan/GitHub/projects/cwa.habitat.selection.dvm")

results$date.time <- ymd_hms(results$date.time)
results <- results %>% force_tz(results$date.time, tzone = "America/Los_Angeles")


min.120 <- results[,c(1, 2, 21, 22, 32)]
min.120 <- min.120[min.120$foray.120 ==1,]
length(unique(min.120$tag.id))

min.120<-min.120[,c(1,2)]
min.120$exit <- NA
min.120$tag.id <- as.factor(min.120$tag.id)
min.120$date <- as.Date(min.120$date.time)

p<-ggplot(data = min.120, aes(date, fill = tag.id)) +
  geom_histogram(position = "stack")+
  scale_fill_manual(values = as.vector(glasbey(28)))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  ggtitle("Mainstem foray", subtitle = "read gap >= 120 minutes, last & return reads at mouth receiver station, 
returning fish temp >=1.5°C warmer than exit temp")
  

ex <- read.csv("data/modif.data/radio.tag/mainstem.foray/all.tags.last.read.csv")
ex$date.time <- mdy_hm(ex$date.time)
ex <- ex %>% force_tz(ex$date.time, tzone = "America/Los_Angeles")
ex<- ex[,c(1,2,8)]
ex$tag.id <- as.factor(ex$tag.id)
ex$date <- as.Date(ex$date.time)

ex$exit <- factor(ex$exit, levels = c("y", "m", "n"))
q<-ggplot(data = ex, aes(date, fill = tag.id, color = exit))+
  geom_bar(linewidth = 1.2)+
  scale_fill_manual(values = as.vector(glasbey(28)))+
  scale_color_manual(values = c("yellow", "black", "red"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  guides(color=guide_legend(override.aes=list(fill=NA)))+
  ggtitle("Final tag read")

ggsave(p, filename = paste("results/figures/radio.tag.figures/mainstem.foray/br.rt.ms.foray.png"), width = 18, height = 12, units = "cm")

ggsave(q, filename = paste("results/figures/radio.tag.figures/mainstem.foray/br.rt.final.read.png"), width = 20, height = 16, units = "cm")



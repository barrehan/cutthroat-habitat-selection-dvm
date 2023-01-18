#' 01/15/2023
#' Analyze foray files to see if there is any pattern to when fish exit/return
#' to BR alcove

library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(plyr)

#'clear workspace
rm(list = ls())
#close open graphics devices
graphics.off() 

data_all <- list.files(path = "C:/Users/barrehan/GitHub/projects/cwa.habitat.selection/data/modif.data/radio.tag/mainstem.foray/foray.deets", pattern = "*.csv", full.names = TRUE) 
csv<-lapply(data_all, read.csv)  
results<-do.call(rbind,csv)

setwd("C:/Users/barrehan/GitHub/projects/cwa.habitat.selection")

results$date.time <- ymd_hms(results$date.time)
results <- results %>% force_tz(results$date.time, tzone = "America/Los_Angeles")


min.120 <- results[,c(1, 2, 21, 22, 32)]
min.120 <- min.120[min.120$foray.120 ==1,]
length(unique(min.120$tag.id))

exit <- read.csv("data/modif.data/radio.tag/mainstem.foray/foray.deets/all.tags.last.read.csv")
exit$date.time <- mdy_hm(exit$date.time)
exit <- exit %>% force_tz(exit$date.time, tzone = "America/Los_Angeles")

exit <- exit[,c(1,2,8)]

min.120<-min.120[,c(1,2)]
min.120$exit <- 'n'
foray <-rbind(min.120, exit)

foray$tag.id <-as.factor(foray$tag.id)
foray$exit <-as.factor(foray$exit)

ggplot(data = min.120, aes(x = date.time, fill = tag.id, color = exit)) +
  geom_histogram(position = "stack")+
  scale_color_brewer(palette="Dark2")

#'black outline around instance of final exit 

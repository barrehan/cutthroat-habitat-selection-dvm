rm(list=ls())
Sys.setenv(TZ = "America/Los_Angeles")

setwd("C:/Users/barrehan/GitHub/projects/cwa.habitat.selection")
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(plyr)
library(data.table) #shift/lag function
library(readr)

all.rt<- read.csv("data/modif.data/radio.tag/tag.reads.10.min.interval.csv")
all.rt$date.time <- mdy_hm(all.rt$date.time)
all.rt <-all.rt[all.rt$date.time >= "2021-07-25 00:00:00",]

#'order by tag id then date.time
all.rt<-all.rt[with(all.rt, order(tag.id, date.time)),]

river.temp<-read.csv("data/raw.data/logger.array/blue.ruin.site.0.river.temp.csv")
river.temp<- river.temp[, c('date.time', 'temperature')]
river.temp$date.time <- mdy_hm(river.temp$date.time)

tags <-unique(all.rt$tag.id)

for(i in tags){
  t0 <- all.rt[all.rt$tag.id == i,] # df for ith tag



rm(list=ls())

Sys.setenv(TZ = "America/Los_Angeles")

library(ggplot2)
library(tidyverse)
library(viridis)
library(dplyr)
library(tidyverse)
library(lubridate)

setwd("C:/Users/barrehan/GitHub/projects/cwa.habitat.selection")

array <- read.csv("data/modif.data/logger.array/br.unif.do.temp.set.depth.simulation.csv")
array<-array[,c(1:4)]
array$date.time <-ymd_hms(array$date.time)
array$hour <-hour(array$date.time)
array$date <-as.Date(array$date.time)
array<-na.omit(array)

#just going to do july 29 right now instead of all days
day<-array[array$date.time >="2021-07-29 00:00:00" & array$date.time <"2021-07-30 00:00:00",]

di <-unique(day$depth)
ti <-unique(day$hour)

dat <-setNames(data.frame(matrix(ncol = 4, nrow =  0)), c("hour", "depth", "temperature", "dissolved.oxygen"))

cntr = 0
for(i in 1:length(di)){
  dep<-di[i]
  match<-day[day$depth == dep,]
  for(j in 1:length(ti)){
    time<-ti[j]
    t.match<-match[match$hour == time,]
    t<-round(mean(t.match$temperature), digits = 1)
    do<-round(mean(t.match$dissolved.oxygen), digits = 1)
    cntr<-cntr+1 #start a new row
    dat[cntr,1] <-time
    dat[cntr,2] <-dep
    dat[cntr,3]<-t
    dat[cntr,4]<-do
  }
}

datrix<- split(dat,dat$hour)
head(datrix)



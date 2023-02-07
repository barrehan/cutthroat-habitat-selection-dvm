rm(list=ls())

Sys.setenv(TZ = "America/Los_Angeles")

library(ggplot2)
library(tidyverse)
library(lme4)
library(viridis)
library(dplyr)
library(tidyverse)
library(lubridate)

setwd("C:/Users/barrehan/GitHub/projects/cwa.habitat.selection")

fish <-read.csv("data/modif.data/hab.select.mod/br.ibutton.data/br.ibutton.pooled.csv")

fish <-fish[fish$case ==1,]
fish<-fish[,c(1:4,6)]
fish$hour <-hour(fish$date.time)
fish$date <-as.Date(fish$date.time)

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




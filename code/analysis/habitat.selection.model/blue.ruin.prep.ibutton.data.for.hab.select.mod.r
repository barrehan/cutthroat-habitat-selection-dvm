#'10-31-2022
# Use uniform distribution estimates of do and temp to create ibutton.csvs -----
rm(list=ls())
setwd("C:/Users/barrehan/GitHub/projects/cwa.habitat.selection")
library(readr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)

array<- read.csv("data/modif.data/hab.select.mod/br.unif.do.temp.csv")
array$date.time <-ymd_hms(array$date.time)
array <- array %>% force_tz(array$date.time, tzone = "America/Los_Angeles")

#'bring in ibutton data from blue ruin netpen (starting with netpen 1)
#'br pen 1: 1 3 7 10 11 23 30 ; 
#'br pen 2: 5 8 15 17 21 22 31   

ib <- read.csv("data/modif.data/ibutton/do.depth.interpolation/ibutton.31.depth.do.interpolation.csv")
ib$date.time <- mdy_hm(ib$date.time)
ib <- ib %>% force_tz(ib$date.time, tzone = "America/Los_Angeles")
ib <- subset(ib, select = c(2,5,7:8))
ib <- rename(ib, temperature = ibutton.temp)
ib <- rename(ib, depth = fish.depth)
ib$case <- 1

#find matching date.time between two df
df <- array %>% filter(array$date.time %in% ib$date.time)
merge <-rbind(ib,df)

#'start df where times line up

merge <- merge[merge$date.time >= "2021-07-25 12:10:00",]

##############################################
#add ibutton id and netpen id
merge$ibutton.id <- 31
merge$netpen <- "blue.ruin.netpen.2"
##############################################



new.dater<- merge %>%arrange(date.time)
new.dater$standardized.do <- scale(new.dater$dissolved.oxygen)
new.dater$standardized.temp <-scale(new.dater$temperature)

#'stratum column
new.dater <- transform(new.dater,                                 # Create ID by group
                       stratID = as.numeric(factor(date.time)))

write.csv(new.dater, file = "data/modif.data/ibutton/hab.select.mod/button.31.hab.select.mod.csv", row.names = F)



# Explore epilimnion & hypolimnion avg min/max in temp and DO ---------------

library(readr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(RColorBrewer)
library(viridis)
library(scales)

setwd("C:/Users/barrehan/GitHub/projects/cwa.habitat.selection.dvm")


# First Norwood -----------------------------------------------------------

array <- read.csv("data/raw.data/logger.array/norwood.mouth.temp.do.csv")
unique(array$sensor.depth)
array$sensor.depth<- as.factor(array$sensor.depth)
array$date.time<- mdy_hm(array$date.time) 
array <- array %>% force_tz(array$date.time, tzone = "America/Los_Angeles")
array<- array[array$date.time >="2021-07-31 00:00:00",]
array<- array[array$date.time < "2021-08-07 00:00:00",]

array$date <-as.Date(array$date.time, width = "1 day")

epi <-array[array$sensor.depth == 0.25,]
hyp <-array[array$sensor.depth == 1.45,]

min.hyp <- hyp%>%
  group_by(date)%>%
  summarise_at(vars(dissolved.oxygen),
               list(min = min))

mean(min.hyp$min)

max.hyp <- hyp%>%
  group_by(date)%>%
  summarise_at(vars(dissolved.oxygen),
               list(max=max))  

mean(max.hyp$max)

min.epi <- epi%>%
  group_by(date)%>%
  summarise_at(vars(dissolved.oxygen),
               list(min = min))

mean(min.epi$min)

max.epi <- epi%>%
  group_by(date)%>%
  summarise_at(vars(dissolved.oxygen),
               list(max=max))  

mean(max.epi$max)

# Next Blue Ruin ----------------------------------------------------------

br <- read.csv("data/modif.data/logger.array/blue.ruin.netpen.array.do.temp.csv")
unique(br$sensor.depth)
br$sensor.depth<- as.factor(br$sensor.depth)
br$date.time<- mdy_hm(br$date.time) 
br <- br %>% force_tz(br$date.time, tzone = "America/Los_Angeles")
br<- br[br$date.time >="2021-07-31 00:00:00",]
br<- br[br$date.time < "2021-08-07 00:00:00",]

br$date <-as.Date(br$date.time, width = "1 day")

epi <-br[br$sensor.depth == 0.25,]
hyp.do <-br[br$sensor.depth == 1.35,]
hyp.temp<-br[br$sensor.depth == 1.55,]

min.hyp.do <- hyp.do%>%
  group_by(date)%>%
  summarise_at(vars(dissolved.oxygen),
               list(min = min), na.rm = T)

mean(min.hyp.do$min)

max.hyp.do <- hyp.do%>%
  group_by(date)%>%
  summarise_at(vars(dissolved.oxygen),
               list(max=max), na.rm = T)  

mean(max.hyp.do$max)

min.hyp.temp <- hyp.temp%>%
  group_by(date)%>%
  summarise_at(vars(temperature),
               list(min = min), na.rm = T)

mean(min.hyp.temp$min)

max.hyp.temp <- hyp.temp%>%
  group_by(date)%>%
  summarise_at(vars(temperature),
               list(max=max), na.rm = T)  

mean(max.hyp.temp$max)

min.epi.do <- epi%>%
  group_by(date)%>%
  summarise_at(vars(dissolved.oxygen),
               list(min = min))

mean(min.epi.do$min)

max.epi.do <- epi%>%
  group_by(date)%>%
  summarise_at(vars(dissolved.oxygen),
               list(max=max))  

mean(max.epi.do$max)

min.epi.temp <- epi%>%
  group_by(date)%>%
  summarise_at(vars(temperature),
               list(min = min))

mean(min.epi.temp$min)

max.epi.temp <- epi%>%
  group_by(date)%>%
  summarise_at(vars(temperature),
               list(max=max))  

mean(max.epi.temp$max)


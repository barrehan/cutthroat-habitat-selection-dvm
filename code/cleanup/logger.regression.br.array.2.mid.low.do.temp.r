#'09/28/2022
#'determining relationship between bottom and mid logger do and temp at blue ruin
#'site two array - lowest do/temp logger did not work during study, need to add
#'in these data before calculating coefficient of variation of logger temps vs 
#'fish temps during radio tag study

library(readr)
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)

setwd("C:/Users/barrehan/GitHub/projects/cwa.habitat.selection")
array <-read.csv("data/raw.data/logger.array/blue.ruin.site.2.mid.low.logger.redeploy.regression.bottom.logger.csv")
array$date.time <-ymd_hms(array$date.time)
array <- array %>% force_tz(array$date.time, tzone = "America/Los_Angeles")
#round time 1 minute to match time stamp interval for original logger deploy
array$date.time<-round_date(array$date.time,unit="5 minutes")
#'create time column
array$time <- format(as.POSIXct(
  array$date.time),format = "%H:%M:%S")
array<-array[array$date.time >="2021-08-24 00:00:00"&array$date.time < "2021-09-13 00:00:00",]

ggplot(array, aes(x = time, y = temperature, colour = logger.position))+
  geom_point()

mid.rep<-array[array$logger.position == "mid.do",]

#'compare temp/do of mid logger on from original study array to this
#'temporary backdate array

original.array<-read.csv("data/raw.data/logger.array/blue.ruin.site.2.mid.temp.do.csv")
original.array$date.time <-mdy_hm(original.array$date.time)
original.array <- original.array %>% force_tz(original.array$date.time, tzone = "America/Los_Angeles")
original.array<-original.array[original.array$date.time >="2021-07-26 00:00:00"& original.array$date.time < "2021-08-17 00:00:00",]
mid.log<-original.array[original.array$logger.position == "mid.do",]

mid.log$time <- format(as.POSIXct(
  mid.log$date.time),format = "%H:%M:%S")

ggplot(mid.log, aes(x = time, y = temperature))+
  geom_point()

ggplot()+
  geom_point(data=mid.log, aes(x = date.time, y = temperature), colour = "blue")+
  geom_point(data=mid.rep, aes(x = date.time, y = temperature), colour = "pink")

ggplot()+
  geom_point(data=mid.log, aes(x = date.time, y = dissolved.oxygen), colour = "blue")+
  geom_point(data=mid.rep, aes(x = date.time, y = dissolved.oxygen), colour = "pink")

site.3.array<-read.csv("data/raw.data/logger.array/blue.ruin.site.3.mid.temp.do.csv")
site.3.array$date.time <-mdy_hm(site.3.array$date.time)
site.3.array <- site.3.array %>% force_tz(site.3.array$date.time, tzone = "America/Los_Angeles")
site.3.array<-site.3.array[site.3.array$date.time >="2021-07-26 00:00:00"& site.3.array$date.time < "2021-08-17 00:00:00",]
mid.low.3<-site.3.array[which(site.3.array$logger.position %in% c("mid.do", "bottom.do")),]

ggplot(mid.low.3, aes(x = date.time, y = temperature, colour = logger.position))+
  geom_point()

site.3.mid<-site.3.array[site.3.array$logger.position == "mid.do",]
site.3.bottom <- site.3.array[site.3.array$logger.position == "bottom.do",]

ggplot()+
  geom_point(data = mid.log, aes(x = date.time, y = temperature), colour = "blue")+
  geom_point(data = site.3.mid, aes(x = date.time, y= temperature), colour = "red")+
  geom_point(data = site.3.bottom, aes(x = date.time, y = temperature), colour= "dark green")

ggplot()+
  geom_point(data = mid.log, aes(x = date.time, y = dissolved.oxygen), colour = "blue")+
  geom_point(data = site.3.mid, aes(x = date.time, y= dissolved.oxygen), colour = "red")+
  geom_point(data = site.3.bottom, aes(x = date.time, y = dissolved.oxygen), colour= "dark green")

original.array$sensor.depth <-as.factor(original.array$sensor.depth)
original.array<-original.array%>%drop_na(date.time)

ggplot(data = original.array, aes(x = date.time, y = temperature, group = sensor.depth))+
  geom_point(aes(colour = sensor.depth))

ggplot(data = original.array, aes(x = date.time, y =dissolved.oxygen, group = sensor.depth))+
  geom_point(aes(colour = sensor.depth))

#'linear relationship between mid and lower logger do?

library(readr)
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)

setwd("C:/Users/barrehan/GitHub/projects/cwa.habitat.selection")
s0<- read.csv("data/raw.data/logger.array/blue.ruin.site.0.river.temp.csv")
s1<-read.csv("data/raw.data/logger.array/blue.ruin.site.1.mouth.temp.do.csv")
s2<-read.csv("data/modif.data/logger.array/blue.ruin.site.2.array.do.temp.csv")
s3<- read.csv("data/raw.data/logger.array/blue.ruin.site.3.mid.temp.do.csv")
s4<-read.csv("data/modif.data/logger.array/blue.ruin.netpen.array.do.temp.csv")
s5<-read.csv("data/raw.data/logger.array/blue.ruin.site.5.head.temp.do.csv")

#clean end dates with wacky temp/do readings

s0$date.time <-mdy_hm(s0$date.time)
s0 <- s0 %>% force_tz(s0$date.time, tzone = "America/Los_Angeles")
s0<-s0[s0$date.time >="2021-07-26 00:00:00"& s0$date.time < "2021-08-09 00:00:00",]

s1$date.time <-mdy_hm(s1$date.time)
s1 <- s1 %>% force_tz(s1$date.time, tzone = "America/Los_Angeles")
s1<-s1[s1$date.time >="2021-07-26 00:00:00"& s1$date.time < "2021-08-09 00:00:00",]

s2$date.time <-ymd_hms(s2$date.time)
s2 <- s2 %>% force_tz(s2$date.time, tzone = "America/Los_Angeles")
s2<-s2[s2$date.time >="2021-07-26 00:00:00"& s2$date.time < "2021-08-09 00:00:00",]

s2<- s2%>%filter_at(vars(temperature), all_vars(!is.na(.)))

s3$date.time <-mdy_hm(s3$date.time)
s3 <- s3 %>% force_tz(s3$date.time, tzone = "America/Los_Angeles")
s3<-s3[s1$date.time >="2021-07-26 00:00:00"& s3$date.time < "2021-08-09 00:00:00",]

s4$date.time <-ymd_hms(s4$date.time)
s4 <- s4 %>% force_tz(s4$date.time, tzone = "America/Los_Angeles")
s4<-s4[s1$date.time >="2021-07-26 00:00:00"& s4$date.time < "2021-08-09 00:00:00",]

s4<- s4%>%filter_at(vars(temperature), all_vars(!is.na(.)))

s5$date.time <-mdy_hm(s5$date.time)
s5 <- s5 %>% force_tz(s5$date.time, tzone = "America/Los_Angeles")
s5<-s5[s5$date.time >="2021-07-26 00:00:00"& s5$date.time < "2021-08-09 00:00:00",]

#cv <- sd(#)/mean(#)*100
#'need to automate - for each df remove NA from temp column and calculate summary
#'stats - mean, max, min, cv for temp and do



cv.temp.s0<- sd(s0$temp)/mean(s0$temp)*100
mean.temp.s0<-mean(s0$temp)
cv.temp.s1<- sd(s1$temp)/mean(s1$temp)*100

cv.temp.s2<- sd(s2$temp)/mean(s2$temp)*100

cv.temp.s3<- sd(s3$temp)/mean(s3$temp)*100

cv.temp.s4<- sd(s4$temp)/mean(s4$temp)*100

cv.temp.s5<- sd(s5$temp)/mean(s5$temp)*100

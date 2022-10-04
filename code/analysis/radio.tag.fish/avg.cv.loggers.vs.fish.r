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
s3<-s3[s3$date.time >="2021-07-26 00:00:00"& s3$date.time < "2021-08-09 00:00:00",]

s4$date.time <-mdy_hm(s4$date.time)
s4 <- s4 %>% force_tz(s4$date.time, tzone = "America/Los_Angeles")
s4<-s4[s4$date.time >="2021-07-26 00:00:00"& s4$date.time < "2021-08-09 00:00:00",]

s4<- s4%>%filter_at(vars(temperature), all_vars(!is.na(.)))

s5$date.time <-mdy_hm(s5$date.time)
s5 <- s5 %>% force_tz(s5$date.time, tzone = "America/Los_Angeles")
s5<-s5[s5$date.time >="2021-07-26 00:00:00"& s5$date.time < "2021-08-09 00:00:00",]

#'combine df to automate summary stats
arrays<- do.call("rbind", list(s0, s1, s2, s3, s4, s5))

stats.temp<- arrays %>% 
  group_by(logger.site)%>%
  summarise(
    max_temp = max(temperature, na.rm = T),
    min_temp = min(temperature, na.rm = T),
    mean_temp = mean(temperature, na.rm = T),
    sd_temp = sd(temperature, na.rm = T),
    cv_temp = sd_temp/mean_temp*100,
  )
  
arrays.do<- arrays%>%filter_at(vars(dissolved.oxygen), all_vars(!is.na(.)))

stats.do<- arrays.do %>% 
  group_by(logger.site)%>%
  summarise(
    max_do = max(dissolved.oxygen, na.rm = T),
    min_do = min(dissolved.oxygen, na.rm = T),
    mean_do = mean(dissolved.oxygen, na.rm = T),
    sd_do = sd(dissolved.oxygen, na.rm = T),
    cv_do = sd_do/mean_do*100
  )

rt <- read.csv("data/modif.data/radio.tag/tag.reads.10.min.interval.csv")
rt$date.time <- mdy_hm(rt$date.time)
rt<-rt[rt$date.time >="2021-07-27 00:00:00",]
stats.rt.indiv<- rt %>% 
  group_by(tag.id)%>%
  summarise(
    max_temp = max(temp.strong, na.rm = T),
    min_temp = min(temp.strong, na.rm = T),
    mean_temp = mean(temp.strong, na.rm = T),
    sd_temp = sd(temp.strong, na.rm = T),
    cv_temp = sd_temp/mean_temp*100,
  )

stats.rt<- rt %>% 
  summarise(
    max_temp = max(temp.strong, na.rm = T),
    min_temp = min(temp.strong, na.rm = T),
    mean_temp = mean(temp.strong, na.rm = T),
    sd_temp = sd(temp.strong, na.rm = T),
    cv_temp = sd_temp/mean_temp*100,
  )
rt$tag.id<-as.factor(rt$tag.id)
ggplot(data = rt, aes(x = date.time, y = temp.strong, colour = tag.id), group_by = tag.id)+
  geom_line()

ggplot(data = rt, aes (x = date.time, y = temp.strong))+
  geom_line()+
  facet_wrap(~tag.id)
#color represents unique logger array
#what they choose vs what is available and how that changes between sites
#' available temps pretty consistent, just depth id different 
#' at each timestep (of choice) for each region (1-4) what are fish temps
#' compared to range of water temps available 
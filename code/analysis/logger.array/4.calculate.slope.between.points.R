rm(list=ls())

library(ggplot2)
library(tidyverse)
library(lme4)
library(viridis)
library(dplyr)
library(tidyverse)
library(lubridate)

setwd("C:/Users/barrehan/GitHub/projects/cwa.habitat.selection")

unif.dist.temp<- read.csv("data/modif.data/logger.array/br.unif.do.temp.set.depth.to.calc.thermocline.csv")
unif.dist.temp$date.time <-ymd_hms(unif.dist.temp$date.time)
unif.dist.temp <- unif.dist.temp%>% force_tz(unif.dist.temp$date.time, tzone = "America/Los_Angeles")
unif.dist.temp <- unif.dist.temp[ -c(5) ]

# remove points at 1.1m and .5m depth they are slightly off and will throw off 
# thermocline slope calculation

unif.dist.temp<-unif.dist.temp[unif.dist.temp$depth != 1.1 & unif.dist.temp$depth != 0.5,]

#create columns for next depth and temp at that depth to calculate slope

unif.dist.temp<- unif.dist.temp %>%
  group_by(date.time)%>%
  mutate(next.temp = lead(temperature)) %>%
  mutate(next.depth = lead(depth))

# calculate slopes between points

for(i in 1:nrow(unif.dist.temp)){
  row <-unif.dist.temp[i,]
  x1 <- row$next.temp
  x2 <- row$temperature
  y1 <-row$next.depth
  y2<- row$depth
  slope <- ((y1-y2)/(x1-x2))
  s2 <- 'NA'
  slope<-ifelse(is_empty(slope), s2, slope)
 unif.dist.temp[i,7] <- slope
  }

names(unif.dist.temp)[7] <- "slope"

step <- unif.dist.temp[unif.dist.temp$date.time == "2021-08-02 20:00:00",]

ggplot(step, aes(x = temperature, y = depth)) + 
  #stat_smooth(method = "lm", formula = y ~ poly(x,3), size = 1, se = F)+
  geom_point(shape = 21, size = 2, fill = "red")+
  geom_line()+
  theme_bw()

write.csv(unif.dist.temp, "data/modif.data/logger.array/br.netpen.array.depths.temps.thermo.calc.csv", row.names = F)



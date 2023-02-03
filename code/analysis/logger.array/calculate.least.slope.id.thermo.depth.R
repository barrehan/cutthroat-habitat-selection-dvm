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

step <- unif.dist.temp[unif.dist.temp$date.time == "2021-08-02 10:00:00",]

ggplot(step, aes(x = temperature, y = depth)) + 
  stat_smooth(method = "lm", formula = y ~ poly(x,3), size = 1, se = F)+
  geom_point(shape = 21, size = 2, fill = "red")+
  theme_bw()

times = unique(unif.dist.temp$date.time) # Storing the unique "times"


#calculate slope between consecutive points, find least slope, find depth
#at slope midpoint

for(i in 1:length(unif.dist.temp)){
  
  tmp <- unif.dist.temp %>% filter(date.time == times[i]) ## Filtering out the logger data for this "Time"
  
  tmp.poly <- lm(depth~ poly(temperature,3, raw = T), tmp) # Fitting the relationship for only Time i
  
  pred_depth <- predict(tmp.poly, filter(fish.dater, Time == Times[i]), se.fit = F) # Predicts single temp value for Time i based on fish temp and the polynomial for Time i
  
  fish.dater[fish.dater$Time == Times[i],]$Depth_pred <- round(pred_depth,1) # Stores the depth prediction in the right place
  
}

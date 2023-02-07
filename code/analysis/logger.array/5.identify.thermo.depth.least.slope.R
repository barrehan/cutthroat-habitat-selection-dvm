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

array <- read.csv("data/modif.data/logger.array/br.netpen.array.depths.temps.thermo.calc.csv")
array$date.time <-ymd_hms(array $date.time)
array$date<-as.Date(array$date.time)

# Store unique "times"
times <- unique(array$date.time) 

#create blank data frame
dat <-setNames(data.frame(matrix(ncol = 7, nrow = 0)), c("date.time", "depth", "temperature", "dissolved.oxygen", "next.temp", "next.depth", "slope"))

#find least slope, find depth at slope midpoint

for(i in 1:length(times)){
  t<-times[i]
  t1 <- array %>% filter(date.time == times[i]) ## Filtering out the logger data for this "Time"
  m <- which(t1$slope == max(t1$slope, na.rm = TRUE)) # row index for 'max slope' (all slopes negative so max is closest to 0 slope)
  df<-t1[m,] # return row(s)
  mt<-which(df$depth == max(df$depth)) # which row has the min temp
  d<-df[mt,] # add to new df
  
  dat <-rbind(dat,d)
}

# remove rows with slopes >0, when array was disturbed or error temp read occurred

dater <-dat[dat$slope < 0,]
dater <- dater %>% rowwise() %>% mutate(thermo.depth=mean(c(depth, next.depth), na.rm=T)) 

fish <-read.csv("data/modif.data/hab.select.mod/br.ibutton.data/br.ibutton.pooled.csv")

fish <-fish[fish$case ==1,]
fish<-fish[,c(1:4,6)]

fish$date.time<-ymd_hms(fish$date.time)
fish$date<-as.Date(fish$date.time)

fish$ibutton.id<-as.factor(fish$ibutton.id)


ggplot()+
  geom_point(data = dater, aes(x=date.time, y=depth), color = "red")+
  geom_point(data=fish, aes(x=date.time, y=depth), color = "blue")+
  scale_y_reverse()+
  facet_wrap(vars(ibutton.id))

fish.step <- fish[fish$date == "2021-08-04",]
array.step <- dater[dater$date == "2021-08-04",]

ggplot()+
  geom_smooth(data = array.step, aes(x=date.time, y=depth), color = "black")+
  geom_smooth(data = fish.step, aes(x = date.time, y = depth, group = ibutton.id, colour = ibutton.id))+
  scale_y_reverse()

spline.d <- as.data.frame(spline(step$date.time, step$thermo.depth))

ggplot() +
  geom_line(data = spline.d, aes(x=x,y=y))+
  scale_y_reverse()

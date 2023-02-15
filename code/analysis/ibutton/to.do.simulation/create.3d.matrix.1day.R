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

# Source for movement model
# Sullivan AB, Jager HI, Myers R. 2003. Modeling white sturgeon movement in a 
# reservoir: the effect of water quality and sturgeon density. Ecological modelling. 
# 167:97-114.

#score for t or do individually, 0 = worst, 1 = best, use actual highest and lowest
#temp and do that occur during this 24 hour period
#set min max do and min max temp as values of 0 and 1 respectively to calculate line slopes

temp <- c(11.9,22.3)
do <-c(9.6,2.5)
factor <- c(1,0)

t.dat<-data.frame(temp,factor)
do.dat <-data.frame(do,factor)

ggplot(t.dat, aes(temp,factor))+
  geom_point()+
  geom_smooth(method="lm")

ggplot(do.dat, aes(do,factor))+
  geom_point()+
  geom_smooth(method="lm")

t.fit<-lm(t.dat$factor~t.dat$temp)
do.fit<-lm(do.dat$factor~do.dat$do)

#intercept and slope values for each
t.cept<-t.fit$coef[1]
t.slope<-t.fit$coef[2]

do.cept<-do.fit$coef[1]
do.slope<-do.fit$coef[2]

dat$temp.fact<- round((t.slope*dat$temperature) + t.cept, digits =2)
dat$do.fact <-round((do.slope*dat$dissolved.oxygen) + do.cept, digits = 2)

#t/do combination score 
#WQI = (Tfact*DOfact)^1/2
dat$WQI <-round(((dat$temp.fact*dat$do.fact)^.5), digits = 2)

#write.csv(dat, "data/modif.data/ibutton/simulation.data.csv", row.names = F)

array3 <-array(data = NA, dim =c(8,6,24))

for(i in 1:24){
  slice.i<-dat[dat$hour == i,]
  as.array(slice.i, dim = c(8,7,1))
  array3[,,1]<-slice.i
  
}

tapply(dat$hour, dat[,-1], c)


?tapply

#create 3d matrix for simulation
datrix<- array(dat,
               dim = length(dat$hour))


simplify2array(by(dat,dat$hour,as.matrix))
head(datrix)

?array

#at each time step (hour) conditions.hours = 

dim(dat)

for(i in 1:24){
  condit.hour<-datrix[,,i]
  
  t.max <-which.max[condit.hour$temp.fact]
  dep.maxt<-
}
# condition.hour <- datrix[,,i]
# which.max if length >1, average...

?split

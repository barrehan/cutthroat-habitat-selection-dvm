#lowest per day, or averaged lowest per hour?

rm(list=ls())

Sys.setenv(TZ = "America/Los_Angeles")

library(ggplot2)
library(tidyverse)
library(viridis)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggpubr)

setwd("C:/Users/barrehan/GitHub/projects/cwa.habitat.selection.dvm")

array <- read.csv("data/modif.data/ibutton/all.ib.interp.depth.csv") #going to use ibutton interp data rather than 
#interp df created for sim - less precise
array <- array[array$site == "norwood",]
array<- array[,c(2,5:7)]
#array<-array[,c(1:4)]
array$date.time <-mdy_hm(array$date.time)
array$hour <-hour(array$date.time)
array$date <-as.Date(array$date.time)
array<-na.omit(array)

#pull the week of days that we are interested in
array<-array[array$date.time >="2021-07-30 00:00:00" & array$date.time < "2021-08-06 00:00:00",]

# da<-unique(days$date)
# di <-unique(days$depth)
# ti <-unique(days$hour)

#per day per hour per depth, find the mean do and mean temperature
# dat <-setNames(data.frame(matrix(ncol = 5, nrow =  0)), c("day", "hour", "depth", "temperature", "dissolved.oxygen"))
# 
# cntr = 0
# for(i in 1:length(di)){
#   dep<-di[i]
#   match<-days[days$depth == dep,]
#   for(j in 1:length(ti)){
#     time<-ti[j]
#     t.match<-match[match$hour == time,]
#     for(k in 1:length(da)){
#       day<-da[k]
#       d.match <- t.match[t.match$date == day,]
#       t<-round(mean(d.match$temperature), digits = 1)
#       do<-round(mean(d.match$dissolved.oxygen), digits = 1)
#       cntr<-cntr+1 #start a new row
#       dat[cntr,1] <- day
#       dat[cntr,2] <-time
#       dat[cntr,3] <-dep
#       dat[cntr,4]<-t
#       dat[cntr,5]<-do
#     }
#   }
# }
# 
# 
# dat$day<- as.Date(dat$day)
# 
# dat<-na.omit(dat)



# SIMULATION --------------------------------------------------------------


# Source for movement model
# Sullivan AB, Jager HI, Myers R. 2003. Modeling white sturgeon movement in a 
# reservoir: the effect of water quality and sturgeon density. Ecological modelling. 
# 167:97-114.

#score for t or do individually, 0 = worst, 1 = best, use actual highest and lowest
#temp and do that occur during this 7-day period
#set min max do and min max temp as values of 0 and 1 respectively to calculate line slopes
min(array$temperature)
max(array$temperature)
temp <- c(10.5,26.1)
factor <- c(1,0)
t.dat<-data.frame(temp,factor)

min(array$dissolved.oxygen)
max(array$dissolved.oxygen)
do <-c(11.4,1.97)
factor<-c(1,0)

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

array$temp.fact<- round((t.slope*array$temperature) + t.cept, digits =2)
array$do.fact <-round((do.slope*array$dissolved.oxygen) + do.cept, digits = 2)

#t/do combination score 
#WQI = (Tfact*DOfact)^1/2
array$WQI <-round(((array$temp.fact*array$do.fact)^.5), digits = 2)

#find highest factor score per hour per strategy
 hour<-unique(array$hour)
 day<-unique(array$date)
 
 #Looking just at the temperature strategy, per hour, what do
 #and what temp have the highest factor scores?
 tfact.df <-array[,-c(8:9)]
 
 tmin.dater <-setNames(data.frame(matrix(ncol = 5, nrow =  0)), c("day", "hour","temperature", "dissolved.oxygen", "strategy"))
 cntr = 0
 for(i in 1:length(day)){
   d<-day[i]
   slice.i<-tfact.df[tfact.df$date == d,]
   for(j in 1:length(hour)){
     h<-hour[j]
     slice.j<-slice.i[slice.i$hour ==h,]
     max.fact <-which(slice.j$temp.fact == max(slice.j$temp.fact))
     trows<-slice.j[max.fact,]
     t<-trows$temperature
     t<-ifelse(t>1, mean(t), t)
     t<-t[1]
     do<-trows$dissolved.oxygen
     do<-ifelse(do>1, min(do), do) #min or mean do??
     do<-do[1]
     
     cntr<-cntr+1
     tmin.dater[cntr,1] <-d
     tmin.dater[cntr,2]<-h
     tmin.dater[cntr,3]<-t
     tmin.dater[cntr,4] <-do
     tmin.dater[cntr,5]<-"Tmin"
   
   }
 }
 
 tmin.dater$day <-as.Date(tmin.dater$day)
 tmin.dater<-na.omit(tmin.dater)

 
 #Looking just at the do strategy, per hour, what do
 #and what temp have the highest factor scores?
 dofact.df <-array[,-c(7,9)]
 
 domax.dater <-setNames(data.frame(matrix(ncol = 5, nrow =  0)), c("day", "hour","temperature", "dissolved.oxygen", "strategy"))
 cntr = 0
 for(i in 1:length(day)){
   d<-day[i]
   slice.i<-dofact.df[dofact.df$date == d,]
   for(j in 1:length(hour)){
     h<-hour[j]
     slice.j<-slice.i[slice.i$hour ==h,]
     max.fact <-which(slice.j$do.fact == max(slice.j$do.fact))
     trows<-slice.j[max.fact,]
     t<-trows$temperature
     t<-ifelse(t>1, mean(t), t)
     t<-t[1]
     do<-trows$dissolved.oxygen
     do<-ifelse(do>1, mean(do), do)
     do<-do[1]
     
     cntr<-cntr+1
     domax.dater[cntr,1] <-d
     domax.dater[cntr,2]<-h
     domax.dater[cntr,3]<-t
     domax.dater[cntr,4] <-do
     domax.dater[cntr,5]<-"DOmax"
     
   }
 }
 
 domax.dater$day <-as.Date(domax.dater$day)
 domax.dater<-na.omit(domax.dater)
 
 
 #Looking just at the WQI strategy, per hour, what do
 #and what temp have the highest factor scores?
 WQIfact.df <-array[,-c(7:8)]
 
 WQI.dater <-setNames(data.frame(matrix(ncol = 5, nrow =  0)), c("day", "hour","temperature", "dissolved.oxygen", "strategy"))
 cntr = 0
 for(i in 1:length(day)){
   d<-day[i]
   slice.i<-WQIfact.df[WQIfact.df$date == d,]
   for(j in 1:length(hour)){
     h<-hour[j]
     slice.j<-slice.i[slice.i$hour ==h,]
     max.fact <-which(slice.j$WQI == max(slice.j$WQI))
     trows<-slice.j[max.fact,]
     t<-trows$temperature
     t<-ifelse(t>1, mean(t), t)
     t<-t[1]
     do<-trows$dissolved.oxygen
     do<-ifelse(do>1, mean(do), do)
     do<-do[1]
     
     cntr<-cntr+1
     WQI.dater[cntr,1] <-d
     WQI.dater[cntr,2]<-h
     WQI.dater[cntr,3]<-t
     WQI.dater[cntr,4] <-do
     WQI.dater[cntr,5]<-"TDOopt"
     
   }
 }
 
 WQI.dater$day <-as.Date(WQI.dater$day)
 WQI.dater<-na.omit(WQI.dater)
 
 #average daily min DO, average daily max temp
 all.dat <- do.call("rbind", list(tmin.dater, domax.dater, WQI.dater))
 
 min.do <-all.dat %>% group_by(strategy, day)%>%
    summarize(mindo = min(dissolved.oxygen))
 
 max.t <-all.dat %>% group_by(strategy, day)%>%
    summarize(maxt = max(temperature))

joint.dat <- left_join(min.do, max.t, by = c("strategy" = "strategy", "day" = "day"))

a <- ggplot()+
   geom_boxplot(data = joint.dat, aes(x = strategy, y = mindo))+
   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
         panel.background = element_blank(), axis.line = element_line(colour = "black"),
         text = element_text(size = 15, family = "serif"))+
   xlab(label = "Strategy") +
   ylab (label = "Minimum daily DO (mg/L)")

b<- ggplot()+   
   geom_boxplot(data = joint.dat, aes(x = strategy, y = maxt))+
   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
         panel.background = element_blank(), axis.line = element_line(colour = "black"),
         text = element_text(size = 15, family = "serif"))+
   xlab(label = "Strategy") +
   ylab (label = "Maximum daily temperature (\u00B0C)")


# NOW BRING IN THE REAL FISH DATER ----------------------------------------

fish <-read.csv("data/modif.data/ibutton/all.ib.interp.depth.csv")
fish<-fish[fish$case ==1,]
fish$date.time <-mdy_hm(fish$date.time)
fish$hour <-hour(fish$date.time)
#pull the week of days that we are interested in
fish<-fish[fish$date.time >="2021-07-30 00:00:00" & fish$date.time < "2021-08-06 00:00:00",]
fish<-fish[fish$site == "norwood",]

fish<-fish[,c(1,3,5,7,9,10)]

calc <- fish%>% group_by(ibutton.id, date) %>%
   summarise(maxt = max(temperature), mindo = min(dissolved.oxygen))

calc <- calc %>% group_by(date) %>%
  summarize(maxt = mean(maxt), mindo = mean(mindo))

calc$strategy <- "Tagged fish"

calc<- calc%>%
   rename(day = date)

#calc <- calc[,c(2:5)]
calc$day <-mdy(calc$day)

calc<- select(calc, 4, 1, 3, 2)

sim.fish.join <- rbind(calc, joint.dat)

sim.fish.join$strategy <- factor(sim.fish.join$strategy, levels = c("DOmax", "Tmin", "TDOopt", "Tagged fish"))

a <- ggplot()+
   geom_violin(data = sim.fish.join, aes(x = strategy, y = maxt), linewidth = .8)+
   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
         panel.background = element_blank(), axis.line = element_line(colour = "black"),
         text = element_text(size = 17, family = "serif"))+
   xlab(label = "Strategy") +
   ylab (label = "Maximum daily temperature (\u00B0C)")

b <-ggplot()+
   geom_violin(data = sim.fish.join, aes(x = strategy, y = mindo), linewidth = .8)+
   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
         panel.background = element_blank(), axis.line = element_line(colour = "black"),
         text = element_text(size = 17, family = "serif"))+
   xlab(label = "Strategy") +
   ylab (label = "Minimum daily DO (mg/L)")


f3<-ggarrange(a, b, 
              ncol=1)

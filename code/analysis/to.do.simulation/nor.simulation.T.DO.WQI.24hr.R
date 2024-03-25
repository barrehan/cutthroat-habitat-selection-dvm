rm(list=ls())

Sys.setenv(TZ = "America/Los_Angeles")

library(ggplot2)
library(tidyverse)
library(viridis)
library(dplyr)
library(tidyverse)
library(lubridate)

setwd("C:/Users/barrehan/GitHub/projects/cwa.habitat.selection.dvm")

array <- read.csv("data/modif.data/logger.array/nor.unif.do.temp.set.depth.simulation.csv")
array<-array[,c(1:4)]

array$date.time <-ymd_hms(array$date.time)
array$hour <-hour(array$date.time)
array$date <-as.Date(array$date.time)


#just going to do july 29 right now instead of all days
day<-array[array$date.time >="2021-08-01 00:00:00" & array$date.time <"2021-08-02 00:00:00",]
day<-day %>%drop_na(depth)

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

dat<-na.omit(dat)

# Source for movement model
# Sullivan AB, Jager HI, Myers R. 2003. Modeling white sturgeon movement in a 
# reservoir: the effect of water quality and sturgeon density. Ecological modelling. 
# 167:97-114.

#score for t or do individually, 0 = worst, 1 = best, use actual highest and lowest
#temp and do that occur during this 24 hour period
#set min max do and min max temp as values of 0 and 1 respectively to calculate line slopes
min(dat$temperature)
max(dat$temperature)
temp <- c(12.4,22.6)
factor <- c(1,0)
t.dat<-data.frame(temp,factor)

min(dat$dissolved.oxygen)
max(dat$dissolved.oxygen)
do <-c(12.1,0.9)
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

dat$temp.fact<- round((t.slope*dat$temperature) + t.cept, digits =2)
dat$do.fact <-round((do.slope*dat$dissolved.oxygen) + do.cept, digits = 2)

#t/do combination score 
#WQI = (Tfact*DOfact)^1/2
dat$WQI <-round(((dat$temp.fact*dat$do.fact)^.5), digits = 2)

#write.csv(dat, "data/modif.data/ibutton/simulation.data.csv", row.names = F)

dat$hour<-as.numeric(dat$hour)
hour <-unique(dat$hour)

dater<-data.frame(matrix(ncol = 4,nrow = 0))
cntr = 0
for(i in 1:length(hour)){
  h <-hour[i]
  slice.i<-dat[dat$hour ==h,]
  maxt <-which(slice.i$temp.fact == max(slice.i$temp.fact))
  t.rows<-slice.i[maxt,]
  t.dep <-t.rows$depth
  t<-ifelse(t.dep>1,mean(t.dep), t.dep)
  t<-t[1]
  
  maxdo <-which(slice.i$do.fact == max(slice.i$do.fact))
  do.rows<-slice.i[maxdo,]
  do.dep <-do.rows$depth
  d<-ifelse(do.dep>1,mean(do.dep), do.dep)
  d<-d[1]
  
  maxwqi <-which(slice.i$WQI == max(slice.i$WQI))
  wqi.rows<-slice.i[maxwqi,]
  wqi.dep <-wqi.rows$depth
  wqi<-ifelse(wqi.dep>1,mean(wqi.dep), wqi.dep)
  wqi<-wqi[1]
  
  cntr<-cntr+1 #start a new row
  
  dater[cntr,1]<-h
  dater[cntr,2]<-t
  dater[cntr,3]<-d
  dater[cntr,4]<-wqi
  
}

colnames(dater) <- c("Hour", "Tmin", "DOmax", "TDOopt")

#wide to long for easier plottinghttp://127.0.0.1:15187/graphics/plot_zoom_png?width=1048&height=895

datlong<-dater %>%gather(Factor, Depth, Tmin:TDOopt)

p<-ggplot(data = datlong, aes(x = Hour, y = Depth))+
  #first smooth; se only
  stat_smooth(aes(group=Factor), col=NA, method = "auto", size=1, se=TRUE, fill = "#7C5467")+
  #now smooth;line only
  stat_smooth(aes(lty = Factor), colour = "#291919", se = F)+
  scale_y_reverse()+
  xlab('Hour of day')+
  ylab("Depth (m)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 20, family = "serif"), legend.key = element_rect(colour = NA, fill = NA))+
  labs(linetype = "Depth selection")+
  scale_linetype_manual(values = c("solid", "dotted", "dashed"), limits = c("DOmax", "TDOopt", "Tmin"))+
  scale_x_continuous(breaks = seq(0,24,2))



ggsave(p, filename = paste("results/figures/ibutton.simulation/nor.depth.selection.simulation.png"), width = 18, height = 10, units = "cm")
  


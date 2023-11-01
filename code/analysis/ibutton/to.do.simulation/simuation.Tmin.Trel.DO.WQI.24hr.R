rm(list=ls())

Sys.setenv(TZ = "America/Los_Angeles")

library(ggplot2)
library(tidyverse)
library(viridis)
library(dplyr)
library(tidyverse)
library(lubridate)

setwd("C:/Users/barrehan/GitHub/projects/cwa.habitat.selection.dvm")

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


# ORIGINAL TEMP FACTOR ----------------------------------------------------

temp <- c(11.9,22.3)
factor <- c(1,0)

t.dat<-data.frame(temp,factor)
t.fit<-lm(t.dat$factor~t.dat$temp)

#intercept and slope values
t.cept<-t.fit$coef[1]
t.slope<-t.fit$coef[2]


dat$temp.fact1<- round((t.slope*dat$temperature) + t.cept, digits =2)

# NEW TEMP FACTOR WITH 15C AS MAX SCORE -----------------------------------

#for temps below 15
temp1 <- c(5, 15.1)
factor1 <- c(0,1)
t.dat1<-data.frame(temp1,factor1)
t.fit1<-lm(t.dat1$factor1~t.dat1$temp1)

#for temps above 15
temp2 <- c(15.1, 26)
factor2 <- c(1,0)
t.dat2 <-data.frame(temp2, factor2)
t.fit2 <- lm(t.dat2$factor2~t.dat2$temp2)

#intercept and slope values for each
t.cept1<-t.fit1$coef[1]
t.slope1<-t.fit1$coef[2]

t.cept2 <-t.fit2$coef[1]
t.slope2 <-t.fit2$coef[2]

#temperature factor score (0-1)

dat$temp.fact2<- ifelse(dat$temperature <=15, round((t.slope1*dat$temperature) + t.cept1, digits =2),
                       round((t.slope2*dat$temperature) + t.cept2, digits =2))

#DO score
do <-c(9.6,2.5)
factor<- c(1,0)
do.dat <-data.frame(do,factor)
do.fit<-lm(do.dat$factor~do.dat$do)

do.cept<-do.fit$coef[1]
do.slope<-do.fit$coef[2]

dat$do.fact <-round((do.slope*dat$dissolved.oxygen) + do.cept, digits = 2)



# WQI USING ORIGINAL TEMP FACTOR ------------------------------------------

#t/do combination score 
#WQI = (Tfact*DOfact)^1/2
dat$WQI1 <-round(((dat$temp.fact1*dat$do.fact)^.5), digits = 2)


# WQI USING NEW TEMP FACTOR -----------------------------------------------

dat$WQI2 <-round(((dat$temp.fact2*dat$do.fact)^.5), digits = 2)


# FOR LOOP FOR SCORES -----------------------------------------------------


dat$hour<-as.numeric(dat$hour)
hour <-unique(dat$hour)

dater<-data.frame(matrix(ncol = 6,nrow = 0))
cntr = 0

for(i in 1:length(hour)){
  h <-hour[i]
  slice.i<-dat[dat$hour ==h,]
  
  maxt <-which(slice.i$temp.fact1 == max(slice.i$temp.fact1))
  t.rows<-slice.i[maxt,]
  t.dep <-t.rows$depth
  t<-ifelse(t.dep>1,mean(t.dep), t.dep)
  t<-t[1]
  
  maxt2 <-which(slice.i$temp.fact2 == max(slice.i$temp.fact2))
  t.rows2<-slice.i[maxt2,]
  t.dep2 <-t.rows2$depth
  t2<-ifelse(t.dep2>1,mean(t.dep2), t.dep2)
  t2<-t2[1]
  
  maxdo <-which(slice.i$do.fact == max(slice.i$do.fact))
  do.rows<-slice.i[maxdo,]
  do.dep <-do.rows$depth
  d<-ifelse(do.dep>1,mean(do.dep), do.dep)
  d<-d[1]
  
  maxwqi1 <-which(slice.i$WQI1 == max(slice.i$WQI1))
  wqi.rows1<-slice.i[maxwqi1,]
  wqi.dep1 <-wqi.rows1$depth
  wqi1<-ifelse(wqi.dep1>1,mean(wqi.dep1), wqi.dep1)
  wqi1<-wqi1[1]
  
  maxwqi2 <-which(slice.i$WQI2 == max(slice.i$WQI2))
  wqi.rows2<-slice.i[maxwqi2,]
  wqi.dep2 <-wqi.rows2$depth
  wqi2<-ifelse(wqi.dep2>1,mean(wqi.dep2), wqi.dep2)
  wqi2<-wqi2[1]
  
  cntr<-cntr+1 #start a new row
  
  dater[cntr,1]<-h
  dater[cntr,2]<-t
  dater[cntr,3]<-t2
  dater[cntr,4]<-d
  dater[cntr,5]<-wqi1
  dater[cntr,6] <-wqi2
  
}

colnames(dater) <- c("Hour", "Tmin.abs", "Tmin.rel", "DOmax", "TDOopt", "TDOopt2")

#dat <- dater[,-c(6)]

#wide to long for easier plottinghttp://127.0.0.1:15187/graphics/plot_zoom_png?width=1048&height=895

datlong<-dater %>%gather(Factor, Depth, Tmin.abs:TDOopt)

p<-ggplot(data = datlong, aes(x = Hour, y = Depth))+
  #first smooth; se only
  stat_smooth(aes(group=Factor), col=NA, method = "auto", size=1, se=TRUE, fill = "#6B7F7F")+
  #now smooth;line only
  stat_smooth(aes(lty = Factor), colour = "#293633", se = F)+
  scale_y_reverse()+
  xlab('Hour of day')+
  ylab("Depth (m)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 15, family = "serif"))+
  labs(linetype = "Depth selection")+
  scale_linetype_manual(values = c("solid", "dotted", "dashed", "dotdash"), limits = c("DOmax", "TDOopt", "Tmin.rel","Tmin.abs"))+
  scale_x_continuous(breaks = seq(0,21,3))



ggsave(p, filename = paste("results/figures/ibutton.simulation/depth.selection.simulation.png"), width = 18, height = 10, units = "cm")



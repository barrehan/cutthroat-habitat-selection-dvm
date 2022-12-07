#'2022_11_07
rm(list=ls())
setwd("C:/Users/barrehan/GitHub/projects/cwa.habitat.selection")
library(readr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)


# Using runif to create distribution do/temp norwood array ----------------
#' using the linear interpolation slope equation y=mx+b where 
#' x = (((depth.est-d1)*(t2-t1))/(d2-d1))+t1
#' where x is the temp estimate, depth.est is the depth that we are interpolating for 
#' (selected using runif), D1 and D2 are the known depths of the bounding sensors, 
#' and t1 and t2 are the known temperatures of those sensors

nor.array<- read.csv("data/raw.data/logger.array/norwood.mouth.temp.do.csv")
nor.array$date.time <-mdy_hm(nor.array$date.time)
nor.array <- nor.array %>% force_tz(nor.array$date.time, tzone = "America/Los_Angeles")
nor.array$date.time <- round_date(nor.array$date.time, "5 minutes") 

#'remove temperature NA rows
temp.array<-nor.array%>%drop_na(temperature)

#'create blank data frame that will house depth/temp estimates from for loop

length(unique(temp.array$date.time))
row.ct<-14965*10
new.dat<-as.data.frame(matrix(ncol=3,nrow=row.ct))
new.dat$V1<-mdy_hms(new.dat$V1)
new.dat$V1 <- force_tz(new.dat$V1, tzone = "America/Los_Angeles")

dates <- as.data.frame(unique(nor.array$date.time))
colnames(dates)<- "date.time"

unique(temp.array$sensor.depth)

cntr = 0

#'x= (y-b)/m

for(i in 1:nrow(dates)){
  dt <- dates[i,]
  match<- temp.array[temp.array$date.time == dt,]
  #fish <- ifelse(fish == '', NA, fish)
  dist <- as.data.frame(runif(10, 0 , 1.60)) #max depth set to .05m less than total depth 
  for(i in 1:nrow(dist)){
    d.unif<- dist[i,]
    d1 <- max(match$sensor.depth[which(match$sensor.depth < d.unif)])
    d2 <- min(match$sensor.depth[which(match$sensor.depth > d.unif)])
    t1 <- match$temperature[match$sensor.depth == d1]
    t2 <- match$temperature[match$sensor.depth == d2]
    t <- (((d.unif-d1)*(t2-t1))/(d2-d1))+t1
    #unif dist selecting depths greater than deepest sensor (at 1.55m), if depth
    #is greater than 1.55, use temp from that sensor since relatively consistent
    #at depth
    bd <- match[match$sensor.depth== 1.50,]
    nbd <- bd$temperature
    t<-ifelse(d.unif>=1.50, nbd, t)
    cntr<-cntr+1 #start a new row
    new.dat[cntr,1]<-dt #date time
    new.dat[cntr,2]<-d.unif #depth from uniform distribution
    new.dat[cntr,3]<-t #temperature estimate for this depth using slope equation
  }
}
colnames(new.dat) <- c("date.time","depth","temperature")

#'need to estimate DO at unif depths too
#'create df for the DO data
dater<- new.dat
do.array<-nor.array%>%drop_na(dissolved.oxygen)
unique(do.array$sensor.depth)

for(i in 1:nrow(dater)){
  row <- dater[i,]
  match <- do.array[do.array$date.time == row$date.time,]
  depth.est <- row$depth
  closest <-match[which.min(abs(depth.est-match$sensor.depth)),]
  closest.do<-closest$dissolved.oxygen
  x1 <- max(match$sensor.depth[which(match$sensor.depth < depth.est)])
  x2 <- min(match$sensor.depth[which(match$sensor.depth > depth.est)])
  d1 <- match$dissolved.oxygen[match$sensor.depth == x1]
  d2 <- match$dissolved.oxygen[match$sensor.depth == x2]
  do <- (d2-d1)/(x2-x1)*(depth.est - x1) + d1
  #'if do is na because depth from unif dist is >/< deepest/shallowest
  #'DO sensor than grab the DO from the closest sensor depth (closest.do)
  #'these bounding sensors often out of thermocline?
  do<-ifelse(is_empty(do), closest.do, do)
  dater[i,4] <- do
  dater[i,5] <- 0
}
colnames(dater)[4] <- "dissolved.oxygen"
colnames(dater)[5] <- "case"

dater <- dater%>%drop_na(temperature)

write.csv(dater, "data/modif.data/hab.select.mod/norwood.unif.do.temp.csv", row.names = F)

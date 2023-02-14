#'2023-01-31
#'Creating an even series of temp/depth/do estimates to use to calculate the
#'thermocline depth and then plot fish depth alongside thermocline depth (help to
#'decipher the purple-yellow scale DVM plots, hard to tell where fish is at 
#'specific time of day/where it is in relation to thermocline)

setwd("C:/Users/barrehan/GitHub/projects/cwa.habitat.selection")
library(readr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)

#'In which I attempt to use an even distribution of do and 
#'temperature estimates between sensors on each logger array using the linear
#'interpolation slope equation y=mx+b where x = (((depth.est-d1)*(t2-t1))/(d2-d1))+t1
#' where x is the temp estimate, depth.est is the depth that we are interpolating for 
#' (selected using runif), D1 and D2 are the known depths of the bounding sensors, 
#' and t1 and t2 are the known temperatures of those sensors

br.array<- read.csv("data/modif.data/logger.array/blue.ruin.netpen.array.do.temp.csv")
br.array$date.time <-mdy_hm(br.array$date.time)
br.array <- br.array %>% force_tz(br.array$date.time, tzone = "America/Los_Angeles")

#'remove temperature NA rows
temp.array<-br.array%>%drop_na(temperature)

#'create blank data frame that will house depth/temp estimates from for loop

length(unique(temp.array$date.time))
row.ct<-5999*10
new.dat<-as.data.frame(matrix(ncol=3,nrow=row.ct))
new.dat$V1<-mdy_hms(new.dat$V1)
new.dat$V1 <- force_tz(new.dat$V1, tzone = "America/Los_Angeles")

dates <- as.data.frame(unique(br.array$date.time))
colnames(dates)<- "date.time"

di<- as.data.frame(seq(from = 0.2, to = 1.6, by = 0.2))
cntr = 0

#'x= (y-b)/m

for(i in 1:nrow(dates)){
  dt <- dates[i,]
  match<- temp.array[temp.array$date.time == dt,]
  #fish <- ifelse(fish == '', NA, fish)
  dist <- as.data.frame(seq(from = 0.2, to = 1.6, by = 0.2)) #max depth 0.05 less than deepest sensor so we can interpolate
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
    bd <- match[match$sensor.depth== 1.55,]
    nbd <- bd$temperature
    t<-ifelse(d.unif>=1.55, nbd, t)
    sd<-match[match$sensor.depth == 0.25,]
    nsd <-sd$temperature
    t<-ifelse(d.unif<0.25,nsd,t)
    
    cntr<-cntr+1 #start a new row
    new.dat[cntr,1]<-dt #date time
    new.dat[cntr,2]<-d.unif #depth from uniform distribution
    new.dat[cntr,3]<-t #temperature estimate for this depth using slope equation
  }
}

#unique id for each group of 10 estimates per date.time segment
#'currently have maximum depth for runif at 1.5 whereas total site depth
#'is 1.7m - deepest sensor is at 1.55 and wanted unif within bounds for interpolation
#'could also make it so any runif depths deeper than 1.55 (deepest sensor), are assigned
#'the temp/do recorded at that sensor since difference with change in depth at the bottom
#'is nominal (low do, low temp)

colnames(new.dat) <- c("date.time","depth","temperature")

#'need to estimate DO at unif depths too
#'create df for the DO data
dater<- new.dat
do.array<-br.array%>%drop_na(dissolved.oxygen)
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

write.csv(dater, "data/modif.data/logger.array/br.unif.do.temp.set.depth.simulation.csv", row.names = F)
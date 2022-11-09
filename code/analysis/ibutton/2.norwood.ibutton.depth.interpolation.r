library(readr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
#'this script assigns ibutton depth using interpolation between bounding
#'sensor temperatures using the slope equation y=mx+b as d? = D1 + (D2-D1)/(X2-X1)*(T1-X1)
#' where d? is the depth of the fish that we are trying to determine, D1 and D2 
#' the known depths of the bounding sensors, X2 and X1 are the known temperatures
#' of those sensors, and T1 is the temperature of the fish, x is T1-X1 (pretending that
#' X1 is the y intercept for these two points...)

setwd("C:/Users/barrehan/GitHub/projects/cwa.habitat.selection")

##NORWOOD ALCOVE IBUTTONS##
#' 2, 4, 6, 13, 14, 16, 18

ib <- read.csv("data/raw.data/ibutton/norwood.netpen/ibutton.18.norwood.csv")
ib$date.time <- mdy_hm(ib$date.time)
ib <- ib %>% force_tz(ib$date.time, tzone = "America/Los_Angeles")

#'get rid of temperatures where tag was out of water before/after deploy

ib <- ib[ib$date.time >= "2021-07-25 00:00:00" & ib$date.time < "2021-08-14 12:00:00",]

ggplot(ib, aes(date.time, temperature.c))+
  geom_point()

logger.array <- read.csv("data/raw.data/logger.array/norwood.mouth.temp.do.csv")
logger.array$date.time <- mdy_hm(logger.array$date.time)
logger.array <- logger.array %>% force_tz(logger.array$date.time, tzone = "America/Los_Angeles")
logger.array <- logger.array[logger.array$date.time < "2021-08-19 00:00:00",]

#' do loggers are recording 1 minute off from the temp sensors need to round everything to the closest
#' 5 minute to get all 7 sensors to line up

logger.array$date.time<-round_date(logger.array$date.time, "5 minutes")

#'reset ibutton time step to match logger array recording interval (recording
#'interval was 2 minutes off)

ans <- vapply(ib$date.time, function(x) x-logger.array$date.time, numeric(51261)) 
indx <- apply(abs(ans), 2, which.min)
time.match <- cbind(ib, logger.array[indx, ])

#clean up new ibutton data frame time.match

time.match <- time.match[, -c(2:4, 8:19)]
time.match <- time.match %>%
  rename(ibutton.temp = temperature.c)

unique(logger.array$logger.position)

#'create new blank data frame for forloop, apparently you need to pre-set
#'date time column as such otherwise it gets all fucked up
#'need to figure out why I have to keep forcing time zone 

row.ct<-nrow(time.match)
new.dat<-as.data.frame(matrix(ncol=5,nrow=row.ct))
new.dat$V2<-mdy_hms(new.dat$V2)
new.dat$V2 <- force_tz(new.dat$V2, tzone = "America/Los_Angeles")

cntr = 0

for(i in 1:nrow(time.match)){
  row <- time.match[i,] # grab first row of ibutton df
  match<- logger.array[logger.array$date.time == row$date.time,] # match date time to logger array df
  t1 <- row$ibutton.temp #pull out ibutton temp from the row
  x1 <- max(match$temperature[which(match$temperature < t1)]) #find the next closest sensor temperature less than ibutton temp
  #x1 <-ifelse(t1>x1, (t1-0.001), x1)
  x2 <- min(match$temperature[which(match$temperature > t1)]) #find the next closest temperature greater than ibutton temp
  d1 <- match$sensor.depth[match$temperature == x1] #find the sensor depth associated with the lower temp
  d2 <- match$sensor.depth[match$temperature == x2] #find the sensor depth associated with the upper temp
  d <- (d2-d1)/(x2-x1)*(t1 - x1) + d1 #slope equation to calculate fish depth based on sensor depth/temp
  depth <- ifelse(length(d> 1), mean(d), d) # take average if you have multiple results
  #if fish temperature is lower than lowest sensor temperature than
  #assign depth as 1.55m (deepest sensor was at 1.5, total depth
  #at site is 1.65)
  depth<-ifelse(is.na(depth), 1.55, depth)  
  
  cntr<-cntr+1 #start a new row
  new.dat[cntr,1]<-row$ibutton
  new.dat[cntr,2]<-row$date.time
  new.dat[cntr,3]<-row$ibutton.temp
  new.dat[cntr,4]<-row$logger.site
  new.dat[cntr,5]<-depth
}
colnames(new.dat) <- c("ibutton","date.time","ibutton.temp","site","fish.depth")

#'now assign do to fish based on its temp and calculated depth
#'first need to create new logger array data frame with just
#'DO sensors

do.array<-logger.array%>%drop_na(dissolved.oxygen)

for(i in 1:nrow(new.dat)){
  row <- new.dat[i,]
  match <- do.array[do.array$date.time == row$date.time,]
  depth.est <- row$fish.depth
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
  new.dat[i,6] <- do
}
colnames(new.dat)[6] <- "dissolved.oxygen"

#tag 18 fell off at 8/7 midnight so cutting out data beyoond that point
new.dat<- new.dat[new.dat$date.time < "2021-08-06 00:00:00",]

write.csv(new.dat,"data/modif.data/ibutton/do.depth.interpolation/norwood.ibutton.18.depth.do.interpolation.csv", row.names = F)


ggplot(new.dat, aes(date.time, ibutton.temp))+
  geom_point()












################################################################################
#'do same for blue ruin fish now
#' 1, 3, 4, 5, 7, 8, 15, 17, 21, 22, 30, 
#' done: 31, 23, 11, 10

ib <- read.csv("data/ibutton.data/ibutton.original.data/blue.ruin.netpens/ibutton.10.br.pen1.csv")
ib$date <-mdy(ib$date)
ib$date.time <- mdy_hms(ib$date.time)
ib <- ib %>% force_tz(ib$date.time, tzone = "America/Los_Angeles")
#'get rid of temperatures where tag was out of water before/after deploy
ib <- ib[ib$date.time >= "2021-07-25 00:00:00" & ib$date.time < "2021-08-13 12:00:00",]

ggplot(ib, aes(date.time, temperature.c))+
  geom_point()

logger.array <- read.csv("data/temp.do.data/do.temp.combined/blue.ruin.site.4.netpen.temp.do.combined.csv")
logger.array$date.time <- mdy_hm(logger.array$date.time)
logger.array <- logger.array %>% force_tz(logger.array$date.time, tzone = "America/Los_Angeles")
logger.array <- logger.array[logger.array$date.time < "2021-08-19 00:00:00",]

logger.array$date.time<-round_date(logger.array$date.time, "5 minutes")

ans <- vapply(ib$date.time, function(x) x-logger.array$date.time, numeric(45721)) 
indx <- apply(abs(ans), 2, which.min)
time.match <- cbind(ib, logger.array[indx, ])

time.match <- time.match[, -c(2:4, 8, 10:20)]
time.match <- time.match %>%
  rename(ibutton.temp = temperature.c)

row.ct<-nrow(time.match)
new.dat<-as.data.frame(matrix(ncol=5,nrow=row.ct))
new.dat$V2<-mdy_hms(new.dat$V2)
new.dat$V2 <- force_tz(new.dat$V2, tzone = "America/Los_Angeles")
unique(logger.array$sensor.depth)

cntr = 0

for(i in 1:nrow(time.match)){
  row <- time.match[i,]
  match<- logger.array[logger.array$date.time == row$date.time,]
  t1 <- row$ibutton.temp
  #find the next closest temperature less than ibutton temp
  x1 <- max(match$temperature[which(match$temperature < t1)])
  #x1 <-ifelse(t1>x1, (t1-0.001), x1)
  #find the next closest temperature greater than ibutton temp
  x2 <- min(match$temperature[which(match$temperature > t1)])
  d1 <- match$sensor.depth[match$temperature == x1]
  d2 <- match$sensor.depth[match$temperature == x2]
  #slope equation to caluclate fish depth based on sensor depth/temp
  d <- (d2-d1)/(x2-x1)*(t1 - x1) + d1
  depth <- ifelse(length(d> 1), mean(d), d)
  #if fish temperature is lower than lowest sensor temperature than
  #assign depth as 1.55m (deepest sensor was at 1.5, total depth
  #at site is 1.65)
  depth<-ifelse(is.na(depth), 1.60, depth)  
  
  cntr<-cntr+1 #start a new row
  new.dat[cntr,1]<-row$ibutton
  new.dat[cntr,2]<-row$date.time
  new.dat[cntr,3]<-row$ibutton.temp
  new.dat[cntr,4]<-'blue.ruin.1'
  new.dat[cntr,5]<-depth
}
colnames(new.dat) <- c("ibutton","date.time","ibutton.temp","site","fish.depth")

ggplot(new.dat, aes(date.time, fish.depth))+
  geom_point()+
  scale_y_reverse()

do.array<-logger.array%>%drop_na(dissolved.oxygen)
unique(do.array$sensor.depth)

for(i in 1:nrow(new.dat)){
  row <- new.dat[i,]
  match <- do.array[do.array$date.time == row$date.time,]
  t1 <- row$fish.depth
  #find the next closest sensor with depth less than ibutton depth
  x1 <- max(match$sensor.depth[which(match$sensor.depth < t1)])
  #some tag depths are deeper than the deepest
  #DO sensor (0.95), if this is the case then assigning the fish that
  #sensor depth so we can perform the interpolation
  x1 <-ifelse(t1<x1, 0.25, x1)
  x2 <- min(match$sensor.depth[which(match$sensor.depth > t1)])
  #looks like one instance of fish being shallower than the
  #shallowest DO sensor, so assigning depth to that sensor
  #depth so we can perform interpolation
  x2<-ifelse(t1>x2, 0.95,x2)
  d1 <- match$dissolved.oxygen[match$sensor.depth == x1]
  d2 <- match$dissolved.oxygen[match$sensor.depth == x2]
  d <- (d2-d1)/(x2-x1)*(t1 - x1) + d1
  do <- ifelse(length(d > 1), mean(d), d)
  do<-ifelse(t1==1.6, d2, do)
  #if there are two value options take the average between the two...
  new.dat[i,6] <- do
}
colnames(new.dat)[6] <- "dissolved.oxygen"

ggplot(new.dat, aes(date.time, dissolved.oxygen))+
  geom_point()

write.csv(new.dat,"data/ibutton.data/ibutton.depth.do.interpolation/ibutton.10.depth.do.interpolation.csv", row.names = F)

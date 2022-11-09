#' 2022-11-09

# Using linear interpolation to calculate fish depth and DO, Blue Ruin --------
rm(list=ls())
library(readr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)

setwd("C:/Users/barrehan/GitHub/projects/cwa.habitat.selection")

#'this script assigns ibutton depth using interpolation between bounding
#'sensor temperatures using the slope equation y=mx+b as d? = D1 + (D2-D1)/(X2-X1)*(T1-X1)
#' where d? is the depth of the fish that we are trying to determine, D1 and D2 
#' the known depths of the bounding sensors, X2 and X1 are the known temperatures
#' of those sensors, and T1 is the temperature of the fish, x is T1-X1 (pretending that
#' X1 is the y intercept for these two points...)

# Blue Ruin ibuttons -----------------------------------------------------
#'pen 1: 01, 03, 07, 10, 11, 23, 30
#'pen2 05, 08, 15, 17, 21, 22, 31

ib <- read.csv("data/raw.data/ibutton/blue.ruin.netpens/ibutton.23.br.pen1.csv")
ib$date.time <- mdy_hms(ib$date.time)
ib <- ib %>% force_tz(ib$date.time, tzone = "America/Los_Angeles")
#'get rid of temperatures where tag was out of water before/after deploy
ib <- ib[ib$date.time > "2021-07-25 12:00:00" & ib$date.time < "2021-08-13 12:00:00",]

ggplot(ib, aes(date.time, temperature.c))+
  geom_point()

# Blue Ruin logger array (using site 4/5 array combo for DO) --------------

logger.array <- read.csv("data/modif.data/logger.array/blue.ruin.netpen.array.do.temp.csv")
logger.array$date.time <-mdy_hm(logger.array$date.time)
logger.array <- logger.array %>% force_tz(logger.array$date.time, tzone = "America/Los_Angeles")

logger.array <- logger.array[logger.array$date.time < "2021-08-13 00:00:00",]

#' original point plots make it look like tag 3 fell off before the 30th - this may be 
#' because depth was being assigned just using the 3 DO sensors? 
#'it kind of looks like tag 1 fell off early too?
#' tag 23 had to temp reads below 0C, removing before interpolation
#ib$temperature.c[ib$temperature.c < 10]<- NA

#'reset ibutton time step to match logger array recording interval (recording
#'interval was 2 minutes off)

ans <- vapply(ib$date.time, function(x) x-logger.array $date.time, numeric(42184)) 
indx <- apply(abs(ans), 2, which.min)
time.match <- cbind(ib, logger.array[indx, ])

#clean up new ibutton data frame time.match

time.match <- time.match[, -c(2:4, 8:16)]
time.match <- time.match %>%
  rename(ibutton.temp = temperature.c)

unique(logger.array$sensor.depth)

#'create new blank data frame for forloop, apparently you need to pre-set
#'date time column as such otherwise it gets all fucked up
#'need to figure out why I have to keep forcing time zone 

row.ct<-nrow(time.match)
new.dat<-as.data.frame(matrix(ncol=5,nrow=row.ct))
new.dat$V2<-mdy_hms(new.dat$V2)
new.dat$V2 <- force_tz(new.dat$V2, tzone = "America/Los_Angeles")

#'remove NA from temp column (from adding DO from array 5 but not temp)

temp.array<- logger.array %>% drop_na(temperature)

cntr = 0

for(i in 1:nrow(time.match)){
  row <- time.match[i,] # grab first row of ibutton df
  match<- temp.array[temp.array$date.time == row$date.time,] # match date time to logger array df
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
  depth<-ifelse(is.na(depth), 1.60, depth)  
  
  cntr<-cntr+1 #start a new row
  new.dat[cntr,1]<-row$ibutton
  new.dat[cntr,2]<-row$date.time
  new.dat[cntr,3]<-row$ibutton.temp
  new.dat[cntr,4]<-'blue.ruin.1'
  new.dat[cntr,5]<-depth
}
colnames(new.dat) <- c("ibutton","date.time","ibutton.temp","site","fish.depth")

do.array<-logger.array%>%drop_na(dissolved.oxygen)
unique(do.array$sensor.depth)

#'looked at ysi profiles again, DO below deepest and shallowest sensors (1.35 and 0.25)
#'stays pretty consistent so any fish depths above or below those sensors just going
#'to assign the DO from the closest sensor

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

#' ibutton 1 and 3 fell off, end of df cut off when continuously assigned to 
#' full depth
#' ibutton 23 has two very low temp reads that I removed (<10C)


write.csv(new.dat,"data/modif.data/ibutton/do.depth.interpolation/br.ibutton.23.depth.do.interpolation.csv", row.names = F)



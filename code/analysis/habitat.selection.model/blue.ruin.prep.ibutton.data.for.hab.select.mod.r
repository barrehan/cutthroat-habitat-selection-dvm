#'2022_09_12

setwd("C:/Users/barrehan/GitHub/projects/cwa.habitat.selection")
library(readr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)

#'In which I attempt to use runif to create a uniform distribution of do and 
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

cntr = 0

#'x= (y-b)/m

for(i in 1:nrow(dates)){
  dt <- dates[i,]
  match<- temp.array[temp.array$date.time == dt,]
  #fish <- ifelse(fish == '', NA, fish)
  dist <- as.data.frame(runif(10, 0 , 1.7)) #max depth 0.05 less than deepest sensor so we can interpolate
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

#'bring in ibutton data from blue ruin netpen (starting with netpen 1)
#'br pen 1: 1 30 3 7 10 11 23; 
#'br pen 2: 5 31 15 8 21 22 17

ib <- read.csv("data/modif.data/ibutton/do.depth.interpolation/ibutton.31.depth.do.interpolation.csv")
ib$date.time <- mdy_hm(ib$date.time)
ib <- ib %>% force_tz(ib$date.time, tzone = "America/Los_Angeles")
ib <- subset(ib, select = c(2,5,7:8))
ib <- rename(ib, temperature = ibutton.temp)
ib <- rename(ib, depth = fish.depth)
ib$case <- 1

#find matching date.time between two df
df <- dater %>% filter(dater$date.time %in% ib$date.time)
merge <-rbind(ib,df)

#create unique id for time
merge$time <- format(as.POSIXct(
  merge$date.time),format = "%H:%M:%S")
merge <-transform(merge, time.ID = as.numeric(factor(time)))

##############################################
#add ibutton id and netpen id
merge$ibutton.id <- 31
merge$netpen <- "blue.ruin.netpen.2"
##############################################



new.dater<- merge %>%arrange(date.time)
new.dater$standardized.do <- scale(new.dater$dissolved.oxygen)
new.dater$standardized.temp <-scale(new.dater$temperature)
new.dater<- new.dater[new.dater$date.time >="2021-07-25 12:10:00",]

#'stratum column
new.dater <- transform(new.dater,                                 # Create ID by group
                       stratID = as.numeric(factor(date.time)))

write.csv(new.dater, file = "data/modif.data/ibutton/hab.select.mod/button.31.hab.select.mod.csv", row.names = F)


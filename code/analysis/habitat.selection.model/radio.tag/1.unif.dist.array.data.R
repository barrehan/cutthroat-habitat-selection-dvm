#'11-27-2022
#'creating uniform distribution of temperature/do for each logger array in
#'blue ruin alcove for habitat selection model with longitudinal movement

rm(list=ls())
library(readr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)

setwd("C:/Users/barrehan/GitHub/projects/cwa.habitat.selection")

#'logger array data
arrays <- read.csv("data/modif.data/logger.array/all.arrays.5min.interval.csv")
unique(arrays$logger.site)
arrays$date.time <- mdy_hm(arrays$date.time)
arrays <- arrays %>% force_tz(arrays$date.time, tzone = "America/Los_Angeles")

#restrict to when date.times exist for all arrays
arrays<- arrays[arrays$date.time>= "2021-07-26 00:00:00" & arrays$date.time <= "2021-08-15 07:45:00", ]

#'remove mixed mouth logger
alcove<- arrays[arrays$logger.site != "site.0.mouth",]

#'remove temperature NA rows
temp.array <- alcove %>% drop_na(temperature)

#'create blank data frame that will house depth/temp estimates from for loop
length(unique(temp.array$date.time))
row.ct<-7818*10*4
new.dat<-as.data.frame(matrix(ncol=4,nrow=row.ct))
new.dat$V1<-mdy_hms(new.dat$V1)
new.dat$V1 <- force_tz(new.dat$V1, tzone = "America/Los_Angeles")

#'create data frame of unique date.times
dates <- as.data.frame(unique(temp.array$date.time))
colnames(dates)<- "date.time"

#'create data frame of unique site names
sites<- as.data.frame(unique(temp.array$logger.site))

#'create data frame of max depths at each site
unique(temp.array$site.total.depth)
max.depth <- c(2.9, 3.8, 2.4, 1.7, 1.6)
logger.site <- c("site.1", "site.2", "site.3", "site.4.netpen", "site.5.head")
depths <- data.frame(logger.site, max.depth)

#progress bar
total <- nrow(dates)
pb <- txtProgressBar(min = 0, max = total, style = 3)

#row count for forloop
cntr = 0

#'x= (y-b)/m

for(i in 1:nrow(dates)){
  dt <- dates[i,]
  match<- temp.array[temp.array$date.time == dt,]
  for(j in 1:nrow(sites)){
    s <-sites[j,]
    l <- match[match$logger.site == s,]
    d <- depths[depths$logger.site == s,]
    md <- d$max.depth
    dist <- as.data.frame(runif(10, 0 , md)) #uniform distribution 10 values between 0 and maximum depth
    for(k in 1:nrow(dist)){
      d.unif<- dist[k,]
      d1 <- max(l$sensor.depth[which(l$sensor.depth < d.unif)])
      d2 <- min(l$sensor.depth[which(l$sensor.depth > d.unif)])
      t1 <- match$temperature[match$sensor.depth == d1]
      t2 <- match$temperature[match$sensor.depth == d2]
      t <- (((d.unif-d1)*(t2-t1))/(d2-d1))+t1
      #unif dist selecting depths greater than deepest sensor, if so
      #use temp from deepest sensor, temp is relatively consistent at depth
      b<-max(l$sensor.depth)
      bd <-l[max(l$sensor.depth),]
      nbd <- bd$temperature
      t<-ifelse(d.unif>=b, nbd, t)
      #unif dist selecting depths less than shallowest sensor, if so
      #use temp from shallowest sensor, temp is relatively consistent at depth
      z<-min(l$sensor.depth)
      zd <- l[min(l$sensor.depth),]
      nzd<-zd$temperature
      t<-ifelse(d.unif<=z, nzd, t)
    
      cntr<-cntr+1 #start a new row
      new.dat[cntr,1]<-dt #date time
      new.dat[cntr,2]<-s
      new.dat[cntr,3]<-d.unif #depth from uniform distribution
      new.dat[cntr,4]<-t #temperature estimate for this depth using slope equation
    }
  }
  setTxtProgressBar(pb, i)
}
close(pb)

new.dat<- drop_na(new.dat)

colnames(new.dat) <- c("date.time","logger.site", "depth", "temperature")

#write.csv(new.dat, "data/modif.data/hab.select.mod/br.radio.tag.data/br.array.temp.unif.dist.csv", row.names = F)

# For loop to interpolate dissolved oxygen  -------------------------------

dater<- new.dat
do.array<-alcove%>%drop_na(dissolved.oxygen)

#progress bar
total <- nrow(dater)
pb <- txtProgressBar(min = 0, max = total, style = 3)

for(i in 1:nrow(dater)){
  row <- dater[i,]
  match <- do.array[do.array$date.time == row$date.time,]
  a.match <- match[match$logger.site == row$logger.site,]
  depth.est <- row$depth
  closest <-a.match[which.min(abs(depth.est-a.match$sensor.depth)),]
  closest.do<-closest$dissolved.oxygen
  x1 <- max(a.match$sensor.depth[which(a.match$sensor.depth < depth.est)])
  x2 <- min(a.match$sensor.depth[which(a.match$sensor.depth > depth.est)])
  d1 <- a.match$dissolved.oxygen[a.match$sensor.depth == x1]
  d2 <- a.match$dissolved.oxygen[a.match$sensor.depth == x2]
  do <- (d2-d1)/(x2-x1)*(depth.est - x1) + d1
  #'if do is na because depth from unif dist is >/< deepest/shallowest
  #'DO sensor than grab the DO from the closest sensor depth (closest.do)
  #'these bounding sensors often out of thermocline
  do<-ifelse(is_empty(do), closest.do, do)
  dater[i,5] <- do
  dater[i,6] <- 0
  setTxtProgressBar(pb, i)
}
close(pb)
colnames(dater)[5] <- "dissolved.oxygen"
colnames(dater)[6] <- "case"

# Add site coordinates ----------------------------------------------------
dater$latitude <-ifelse(dater$logger.site == "site.1", 44.23229,
                        ifelse(dater$logger.site == "site.2", 44.23156,
                               ifelse(dater$logger.site == "site.3", 44.23052,
                                      ifelse(dater$logger.site == "site.4.netpen", 44.23019, 
                                             ifelse(dater$logger.site == "site.5.head", 44.23003, 9999)))))

dater$longitude <-ifelse(dater$logger.site == "site.1", -123.163,
                        ifelse(dater$logger.site == "site.2", -123.1628,
                               ifelse(dater$logger.site == "site.3", -123.1626,
                                      ifelse(dater$logger.site == "site.4.netpen", -123.1624, 
                                             ifelse(dater$logger.site == "site.5.head", -123.1623, 9999)))))
     

write.csv(dater, "data/modif.data/hab.select.mod/br.radio.tag.data/br.array.temp.do.unif.dist.csv", row.names = F)

#'12/28/2022
#'here I am combining the randomly generated do/temp data from all arrays with 
#'radio tag data - fish will not just be combined with the closest logger array but 
#'with all arrays so we can see the full span of temps available versus the fish temp
#'at at each recorded timestamp

rm(list=ls())
library(ggplot2)
library(dplyr)
library(tidyverse)
library(lubridate)

setwd("C:/Users/barrehan/GitHub/projects/cwa.habitat.selection")
arrays<- read.csv("data/modif.data/hab.select.mod/br.radio.tag.data/br.array.temp.do.unif.dist.csv")
rt.fish<-read.csv("data/modif.data/radio.tag/all.rt.depth.do.interpolated.csv")


arrays$date.time <- mdy_hm(arrays$date.time)
arrays <- arrays %>% force_tz(arrays$date.time, tzone = "America/Los_Angeles")
arrays$tag.id <-NA

rt.fish$date.time <-mdy_hm(rt.fish$date.time)
rt.fish<- rt.fish %>% force_tz(rt.fish$date.time, tzone = 'America/Los_Angeles')
fish<-subset(rt.fish, select=c("date.time", "tag.id", "logger.site", "fish.depth", "fish.do", "temp.strong", "lat", "long"))
fish$case <-1
fish.mouth<-fish[fish$logger.site == "site.0.mouth",]
fish<-fish[fish$logger.site != "site.0.mouth", ] 

#put dfs in same column order, make column names the same too
fish<-fish %>% 
  rename(
    depth = fish.depth,
    temperature = temp.strong,
    dissolved.oxygen = fish.do,
    longitude = long,
    latitude = lat
  )

arrays <- arrays[, c("date.time", "tag.id", "logger.site", "depth", "dissolved.oxygen", "temperature", "latitude", "longitude", "case")]

#'create blank data frame that will house depth/temp estimates from for loop
new.dat<-as.data.frame(matrix(ncol=9))
new.dat$V1<-mdy_hms(new.dat$V1)
new.dat$V1 <- force_tz(new.dat$V1, tzone = "America/Los_Angeles")
colnames(new.dat) <- c("date.time", "tag.id", "logger.site", "depth", "dissolved.oxygen", "temperature", "latitude", "longitude", "case")

#progress bar
total <- nrow(fish)
pb <- txtProgressBar(min = 0, max = total, style = 3)

for(i in 1:nrow(fish)){
  f<-fish[i,]
  t<-f$tag.id
  f.dt<-f$date.time
  dt<-arrays[arrays$date.time == f.dt,]
  dt$tag.id = t
  m<- rbind(f, dt)
  new.dat<-rbind(m,new.dat)
  
  setTxtProgressBar(pb, i)
}
close(pb)

new.dat<- drop_na(new.dat)

new.dat<- drop_na(new.dat)

#'order by fish.id and date.time
br.fish <- new.dat[
  order(new.dat[,2], new.dat[,1] ),
]

final <- br.fish %>% 
  mutate(stratID = group_indices(.,tag.id, date.time))

write.csv(final, "data/modif.data/rado.tag.fish/tags.array.temps.daytime.temp.select.csv", row.names = F)

setwd("C:/Users/barrehan/GitHub/projects/cwa.habitat.selection")
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(plyr)
library(data.table) #shift/lag function

ib<- read.csv("data/modif.data/radio.tag/individual.radio.tags/tag.11.csv")
ib$date.time <- mdy_hm(ib$date.time)
ib <- ib %>% force_tz(ib$date.time, tzone = "America/Los_Angeles")
ib<-ib[order(ib$date.time),]

#'decide on protocol for determining if fish appears to have made forray into 
#'mainstem river
#'OPTION: last read is at receiver 1, antenna 1 (mouth), fish is gone for 60min
#'or greater, difference in fish temperature at last read and temperature at return
#'read is 2C or greater?
# Take date.time rows 1 to n-1 and subtract rows 2 to n:
ib$gap <- c(NA, with(ib, date.time[-1] - date.time[-nrow(ib)]))

#'df just when fish reads skip hour or greater
ib.mod<- ib[ib$gap >= 60 | shift(ib$gap >= 60, n=1L, type = "lag"),]
ib.mod<- ib.mod %>%
  filter(!is.na(date.time))

#'order by date, calculate temperature difference for before/after reads
ib.mod<-ib.mod[order(ib.mod$date.time),]
temp.diff <- diff(ib.mod$temp.strong)
diff<-as.data.frame(temp.diff)
diff<- diff %>% add_row(temp.diff = 0, .before = 1)
ib.temps<- cbind(ib.mod, diff)

river.temp<-read.csv("data/raw.data/logger.array/blue.ruin.site.0.river.temp.csv")
river.temp<- river.temp[, c('date.time', 'temperature')]
river.temp$date.time <- mdy_hm(river.temp$date.time)
river.temp <- river.temp %>% force_tz(river.temp$date.time, tzone = "America/Los_Angeles")

ib.ms.temp<-merge(ib.temps,river.temp, "date.time")

ib.ms.temp$foray <- ifelse(ib.ms.temp$gap >= 60 & ib.ms.temp$temp.diff >= 1.5 & ib.ms.temp$receiver.site %in% c(1,2), 1, 0)


##################Detection efficiency#############################
#' create column that counts detection patterns to determine detection efficiency
#' 1 means zero misses 0 means missed  (skipped a receiver)
#' good (sequential antenna) reads == 11, 12, 21, 22, 23, 32, 33, 34, 43, 44
#' all others will be given 0

all.ib <- read.csv("data/modif.data/radio.tag/tag.reads.10.min.interval.csv")
all.ib$date.time <- mdy_hm(all.ib$date.time)
all.ib <- all.ib %>% force_tz(all.ib$date.time, tzone = "America/Los_Angeles")
all.ib<-all.ib[order(all.ib$tag.id, all.ib$date.time),]
all.ib<-all.ib[all.ib$date.time >="2021-07-26 00:00:00",]

all.ib$detection.id<-ifelse(all.ib$receiver.site == 1 & shift(all.ib$receiver.site == 1, n = 1L, type = "lag"), 1, 
                         ifelse(all.ib$receiver.site ==1 & shift(all.ib$receiver.site ==2, n = 1L, type = "lag"), 1,
                                ifelse(all.ib$receiver.site == 2 & shift(all.ib$receiver.site == 1, n = 1L, type = "lag"), 1,
                                       ifelse(all.ib$receiver.site == 2 & shift(all.ib$receiver.site ==2, n = 1L, type = "lag"), 1,
                                              ifelse(all.ib$receiver.site == 2 & shift(all.ib$receiver.site == 3, n = 1L, type = "lag"), 1,
                                                     ifelse(all.ib$receiver.site == 3 & shift(all.ib$receiver.site == 2, n = 1L, type = "lag"), 1,
                                                            ifelse(all.ib$receiver.site == 3 & shift(all.ib$receiver.site == 3, n = 1L, type = "lag"), 1,
                                                                   ifelse(all.ib$receiver.site == 3 & shift(all.ib$receiver.site == 4, n = 1L, type = "lag"), 1,
                                                                          ifelse(all.ib$receiver.site == 4 & shift(all.ib$receiver.site == 3, n = 1L, type = "lag"), 1,
                                                                                 ifelse(all.ib$receiver.site == 4 & shift(all.ib$receiver.site == 4, n = 1L, type = "lag"), 1, 0))))))))))


first.read<-all.ib %>% 
  group_by(tag.id) %>%
  filter(date.time == min(date.time))%>%
           mutate(detection.id = replace(detection.id, detection.id == 0|detection.id==1, NA))
           
new.ib<- all.ib[!(all.ib$date.time | all.ib$tag.id %in% first.read$date.time | first.read$tag.id),]

detects<-na.exclude(count(all.ib$detection.id[all.ib$detection.id ==1],))
miss<- na.exclude(count(all.ib$detection.id[all.ib$detection.id == 0],))

efficiency <-detects$freq/(detects$freq+miss$freq)

#make gap time a variable then for loop through different time options
#add up # of forays? 
#' time off alcove (what is the gap of time the fish is out)
#' previous vs current antenna
#' detection efficiency count sum of all detect codes (unique) vs non-detect
#' 14 stands for detected previously site 1 next site 4 this becomes a missed detection
#' 12 for detected previously site 1 next site 2, this is a good detection
#' code so only manual at last step

names(ib.ms.temp)[12]<- 'mainstem.temp'

write.csv(ib.ms.temp, "data/modif.data/radio.tag/mainstem.foray/tag.11.mainstem.movement.csv", row.names=F)


#'visual assessment now? if fish is gone for >= 60min, is registered at
#'antenna 1 or 2 on return, and temperature is warmer than previous by 1.5C, mark 
#'as T for potential MS foray 


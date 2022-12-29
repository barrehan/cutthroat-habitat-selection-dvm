setwd("C:/Users/barrehan/GitHub/projects/cwa.habitat.selection")
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(plyr)
library(data.table) #shift/lag function
library(readr)
library(purrr)

all.rt<- read.csv("data/modif.data/radio.tag/tag.reads.10.min.interval.csv")
all.rt$date.time <- mdy_hm(all.rt$date.time)
all.rt <- all.rt %>% force_tz(all.rt$date.time, tzone = "America/Los_Angeles")
all.rt <-all.rt[all.rt$date.time >= "2021-07-25 00:00:00",]

#'order by tag id then date.time
all.rt<-all.rt[with(all.rt, order(tag.id, date.time)),]

river.temp<-read.csv("data/raw.data/logger.array/blue.ruin.site.0.river.temp.csv")
river.temp<- river.temp[, c('date.time', 'temperature')]
river.temp$date.time <- mdy_hm(river.temp$date.time)
river.temp <- river.temp %>% force_tz(river.temp$date.time, tzone = "America/Los_Angeles")

#'protocol for determining if fish appears to have made foray into 
#'mainstem river
#'OPTION: last read is at receiver 1, antenna 1 (mouth), fish is gone for 60min
#'or greater, difference in fish temperature at last read and temperature at return
#'read is 2C or greater?

tags <-unique(all.rt$tag.id)

for(i in tags){
  t0<-all.rt[all.rt$tag.id == i,] # df for ith tag
  
  #Take date.time rows 1 to n-1 and subtract rows 2 to n
  t0$gap <- c(NA, with(t0, date.time[-1] - date.time[-nrow(t0)]))
  
  #new column for different gap time intervals starting at 30 minutes
  #and up to 120 minutes by 10 minute intervals
  
  t0$gap.30 <-ifelse(t0$gap >=30, 1, 0)
  t0$gap.40 <-ifelse(t0$gap >=40, 1, 0)
  t0$gap.50 <-ifelse(t0$gap >=50, 1, 0)
  t0$gap.60 <-ifelse(t0$gap >=60, 1, 0)
  t0$gap.70 <-ifelse(t0$gap >=70, 1, 0)
  t0$gap.80 <-ifelse(t0$gap >=80, 1, 0)
  t0$gap.90 <-ifelse(t0$gap >=90, 1, 0)
  t0$gap.100 <-ifelse(t0$gap >=100, 1, 0)
  t0$gap.110 <-ifelse(t0$gap >=110, 1, 0)
  t0$gap.120 <-ifelse(t0$gap >=120, 1, 0)
  
  #order by date, calculate temperature difference for before/after reads
  
  t0<-t0[order(t0$date.time),]
  temp.diff <- diff(t0$temp.strong)
  diff<-as.data.frame(temp.diff)
  diff<- diff %>% add_row(temp.diff = 0, .before = 1)
  rt.temps<- cbind(t0, diff)
  rt.ms.temp<-merge(rt.temps,river.temp, "date.time")
  names(rt.ms.temp)[22]<- 'mainstem.temp'
  
  #'calculate number of forays for each time gap (temp diff must be >=1.5C)
  #'must be re-registered at receiver site 1 or 2
  
  rt.ms.temp$foray.30 <- ifelse(rt.ms.temp$gap.30 == 1 & rt.ms.temp$temp.diff >= 1.5 & rt.ms.temp$receiver.site %in% c(1,2), 1, 0)
  rt.ms.temp$foray.40 <- ifelse(rt.ms.temp$gap.40 == 1 & rt.ms.temp$temp.diff >= 1.5 & rt.ms.temp$receiver.site %in% c(1,2), 1, 0)
  rt.ms.temp$foray.50 <- ifelse(rt.ms.temp$gap.50 == 1 & rt.ms.temp$temp.diff >= 1.5 & rt.ms.temp$receiver.site %in% c(1,2), 1, 0)
  rt.ms.temp$foray.60 <- ifelse(rt.ms.temp$gap.60 == 1 & rt.ms.temp$temp.diff >= 1.5 & rt.ms.temp$receiver.site %in% c(1,2), 1, 0)
  rt.ms.temp$foray.70 <- ifelse(rt.ms.temp$gap.70 == 1 & rt.ms.temp$temp.diff >= 1.5 & rt.ms.temp$receiver.site %in% c(1,2), 1, 0)
  rt.ms.temp$foray.80 <- ifelse(rt.ms.temp$gap.80 == 1 & rt.ms.temp$temp.diff >= 1.5 & rt.ms.temp$receiver.site %in% c(1,2), 1, 0)
  rt.ms.temp$foray.90 <- ifelse(rt.ms.temp$gap.90 == 1 & rt.ms.temp$temp.diff >= 1.5 & rt.ms.temp$receiver.site %in% c(1,2), 1, 0)
  rt.ms.temp$foray.100 <- ifelse(rt.ms.temp$gap.100 == 1 & rt.ms.temp$temp.diff >= 1.5 & rt.ms.temp$receiver.site %in% c(1,2), 1, 0)
  rt.ms.temp$foray.110 <- ifelse(rt.ms.temp$gap.110 == 1 & rt.ms.temp$temp.diff >= 1.5 & rt.ms.temp$receiver.site %in% c(1,2), 1, 0)
  rt.ms.temp$foray.120 <- ifelse(rt.ms.temp$gap.120 == 1 & rt.ms.temp$temp.diff >= 1.5 & rt.ms.temp$receiver.site %in% c(1,2), 1, 0)
  
  # df just when a foray (1) is present in at least one of the columns
  # pull event and read before (last read before exit, first read upon
  # return)
  
  t0.mod<- rt.ms.temp[rt.ms.temp$foray.30 ==1 | shift(rt.ms.temp$foray.30, n=1L, type = "lag"),] 
  t0.mod<- t0.mod %>%
    filter(!is.na(date.time))
  
  forays <-t0.mod[,c(23:32)]
  forays <- forays %>% summarize_all(funs(sum))
  forays$tag.id <-i
  
  write.csv(t0.mod, paste0('data/modif.data/radio.tag/mainstem.foray/foray.deets/tag.', i,'.mainstem.movement.csv'), row.names=F)
  write.csv(forays, paste0('data/modif.data/radio.tag/mainstem.foray/foray.sums/tag.', i, '.foray.sum.csv'), row.names = F)
}

setwd("C:/Users/barrehan/GitHub/projects/cwa.habitat.selection/data/modif.data/radio.tag/mainstem.foray/foray.sums")

data_all <- list.files(path = "C:/Users/barrehan/GitHub/projects/cwa.habitat.selection/data/modif.data/radio.tag/mainstem.foray/foray.sums", pattern = "*.csv", full.names = TRUE) 
csv<-lapply(data_all, read.csv)  
results<-do.call(rbind,csv)

write.csv(results, "pooled.tags.foray.sums.csv", row.names = F)


#'old for loop just for 60 minute gap interval
#'
#' for(i in tags){
#'   t0<-all.rt[all.rt$tag.id == i,] # df for ith tag
#'   
#'   #Take date.time rows 1 to n-1 and subtract rows 2 to n
#'   t0$gap <- c(NA, with(t0, date.time[-1] - date.time[-nrow(t0)])) 
#'   
#'   # df just when fish reads skip hour or greater
#'   t0.mod<- t0[t0$gap >= 60 | shift(t0$gap >= 60, n=1L, type = "lag"),] 
#'   t0.mod<- t0.mod %>%
#'     filter(!is.na(date.time))
#'   
#'   #'order by date, calculate temperature difference for before/after reads
#'   t0.mod<-t0.mod[order(t0.mod$date.time),]
#'   temp.diff <- diff(t0.mod$temp.strong)
#'   diff<-as.data.frame(temp.diff)
#'   diff<- diff %>% add_row(temp.diff = 0, .before = 1)
#'   rt.temps<- cbind(t0.mod, diff)
#'   rt.ms.temp<-merge(rt.temps,river.temp, "date.time")
#'   
#'   rt.ms.temp$foray <- ifelse(rt.ms.temp$gap >= 60 & rt.ms.temp$temp.diff >= 1.5 & rt.ms.temp$receiver.site %in% c(1,2), 1, 0)
#'   
#'   names(rt.ms.temp)[12]<- 'mainstem.temp'
#'   
#'   write.csv(rt.ms.temp, paste0('data/modif.data/radio.tag/mainstem.foray/tag.', i,'.mainstem.movement.csv'), row.names=F)
#' }



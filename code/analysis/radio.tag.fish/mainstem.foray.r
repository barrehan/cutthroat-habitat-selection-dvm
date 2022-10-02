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

names(ib.ms.temp)[12]<- 'mainstem.temp'

write.csv(ib.ms.temp, "data/modif.data/radio.tag/mainstem.foray/tag.11.mainstem.movement.csv", row.names=F)


#'visual assessment now? if fish is gone for >= 60min, is registered at
#'antenna 1 or 2 on return, and temperature is warmer than previous by 1.5C, mark 
#'as T for potential MS foray 


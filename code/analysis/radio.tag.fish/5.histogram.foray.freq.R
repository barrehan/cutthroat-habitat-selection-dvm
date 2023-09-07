rm(list=ls())
Sys.setenv(TZ = "America/Los_Angeles")

setwd("C:/Users/barrehan/GitHub/projects/cwa.habitat.selection.dvm")
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(plyr)
library(data.table) #shift/lag function
library(readr)

all.rt<- read.csv("data/modif.data/radio.tag/tag.reads.10.min.interval.csv")
all.rt$date.time <- mdy_hm(all.rt$date.time)
all.rt <-all.rt[all.rt$date.time >= "2021-07-25 00:00:00",]

#'order by tag id then date.time
all.rt<-all.rt[with(all.rt, order(tag.id, date.time)),]

river.temp<-read.csv("data/raw.data/logger.array/blue.ruin.site.0.river.temp.csv")
river.temp<- river.temp[, c('date.time', 'temperature')]
river.temp$date.time <- mdy_hm(river.temp$date.time)

tags <-unique(all.rt$tag.id)

dat <-setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("date.time", "tag.id", "foray"))

for(i in tags){
  t0 <- all.rt[all.rt$tag.id == i,] # df for ith tag
  #Take date.time rows 1 to n-1 and subtract rows 2 to n
  t0$gap <- c(NA, with(t0, date.time[-1] - date.time[-nrow(t0)]))
  #order by date, calculate temperature difference for before/after reads
  
  t0<-t0[order(t0$date.time),]
  temp.diff <- diff(t0$temp.strong)
  diff<-as.data.frame(temp.diff)
  diff<- diff %>% add_row(temp.diff = 0, .before = 1)
  rt.temps<- cbind(t0, diff)
  rt.ms.temp<-merge(rt.temps,river.temp, "date.time")
  names(rt.ms.temp)[12]<- 'mainstem.temp'
  
  #it is a foray if the gap time is greater than 10 minutes, if the temperature difference is 1.5C or greater, and last read
  #was at receiver site 1 or 2 
  rt.ms.temp$foray <- ifelse(rt.ms.temp$gap > 10 & rt.ms.temp$temp.diff >= 1.5 & rt.ms.temp$receiver.site %in% c(1,2), rt.ms.temp$gap, NA)
  #remove non-foray events (NA)
  rt.ms.temp<- rt.ms.temp %>%
    filter(!is.na(foray))
  
  mod <-rt.ms.temp[,c(1,2,13)]
  dat <-rbind(mod, dat)
}

dat$foray.min <- dat$foray
dat$foray.min[dat$foray >780] <-780

q <-ggplot(dat, aes(x = foray.min))+
  geom_histogram(breaks = c(seq(0,780,60)),
                 col = "black",
                 fill = "#535260")+
  labs(x = "Mainstem foray time (minutes)", y = "Count")+
  scale_x_continuous(limits = c(0, 780), breaks = c(seq(0, 780, by= 60)),
                     labels = c(seq(0,720, by= 60), "> 12 hours"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 13, family = "serif"))

ggsave(q, filename = paste("results/figures/radio.tag.figures/mainstem.foray/foray.frequency.png"), width = 18, height = 10, units = "cm")




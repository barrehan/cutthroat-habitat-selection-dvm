rm(list=ls())

library(readr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)

setwd("C:/Users/barrehan/GitHub/projects/cwa.habitat.selection.dvm")

all.array.dat<-read.csv("data/raw.data/logger.array/all.arrays.osu.epa.csv")

#remove error temp and do readings (<0)

all.array.dat <- all.array.dat[all.array.dat$temperature >0,]
all.array.dat <- all.array.dat[all.array.dat$dissolved.oxygen >0,]

#remove data with only temperature (no DO)

do.dat<-all.array.dat %>% drop_na(temperature)
do.dat$sensor.depth <- as.factor(do.dat$sensor.depth)

epa <- do.dat[all.array.dat$collected.by == "usepa",]
epa <- epa %>% drop_na(date.time)

nhb <-epa[epa$location == "north.harrisburg",]
cot <-epa[epa$location == "cottonwood",]
riv <-epa[epa$location == "river.loop",]
nor <-epa[epa$location == "telemetry",]
bear <-epa[epa$location == "bear.island",]
br <-epa[epa$location == "blue.ruin",]

# clean error reads N Harrisburg
# logger.a 0.8m depth, 7/30 12:20-12:30 lines 2015-2016
# logger.b 2.4m (DO) 7/23 12:30-12:40, 7/30 12:20 - 12:30
# logger.c 7/23 12:30-12:40 lines 6045-6046
# 7/25 14:30-15:40, lines 6345-6351
# 7/30 11:40-12:30, lines 7048-7053

nhb<-nhb[-c(2015, 2016, 4030, 5036, 5037, 6043:6046, 6345:6351, 7048:7053),]


ggplot(nhb, aes(date.time, temperature, group = sensor.depth))+
  geom_point(aes(col = sensor.depth))+
  facet_wrap(~logger.site)

ggplot(nhb, aes(date.time, dissolved.oxygen, group = sensor.depth))+
  geom_point(aes(col = sensor.depth))+
  facet_wrap(~logger.site)

# clean error reads Cottonwood
# remove first and last day, still have 3 diel cycles

cot<-cot[cot$date > "7/30/2020"& cot$date < "8/3/2020",]

ggplot(cot, aes(date.time, temperature, group = sensor.depth))+
  geom_point(aes(col = sensor.depth))+
  facet_wrap(~logger.site)

ggplot(cot, aes(date.time, dissolved.oxygen, group = sensor.depth))+
  geom_point(aes(col = sensor.depth))+
  facet_wrap(~logger.site)

# clean error reads River Loop
# remove ~4hr window with spiked temp

riv<-riv[!(riv$date.time >= "8/7/2020 10:00" & riv$date.time <= "8/7/2020 14:00"),]

ggplot(riv, aes(date.time, temperature, group = sensor.depth))+
  geom_point(aes(col = sensor.depth))+
  facet_wrap(~logger.site)

ggplot(riv, aes(date.time, dissolved.oxygen, group = sensor.depth))+
  geom_point(aes(col = sensor.depth))+
  facet_wrap(~logger.site)

# clean error reads Norwood
# remove initial reads and ~2hr time window

nor<-nor[!(nor$date.time < "8/14/2020 12:00"),]
nor<-nor[!(nor$date.time >= "8/17/2020 13:00" & nor$date.time <= "8/17/2020 15:00"),]

nor<- nor %>% 
  mutate(location = str_replace(location, "telemetry", "norwood"))%>%
  mutate(logger.site = str_replace(logger.site, "telemetry", "norwood"))

ggplot(nor, aes(date.time, temperature, group = sensor.depth))+
  geom_point(aes(col = sensor.depth))+
  facet_wrap(~logger.site)

ggplot(nor, aes(date.time, dissolved.oxygen, group = sensor.depth))+
  geom_point(aes(col = sensor.depth))+
  facet_wrap(~logger.site)

# clean error reads Bear Island
# remove intial reads and ~2hr window spike

bear<-bear[!(bear$date.time<= "8/24/2020 13:00"),]
bear<-bear[!(bear$date.time >= "8/27/2020 13:00" & bear$date.time <= "8/27/2020 15:00"),]

ggplot(bear, aes(date.time, temperature, group = sensor.depth))+
  geom_point(aes(col = sensor.depth))+
  facet_wrap(~logger.site)

ggplot(bear, aes(date.time, dissolved.oxygen, group = sensor.depth))+
  geom_point(aes(col = sensor.depth))+
  facet_wrap(~logger.site)

#remove temp spike at end

br<-br[!(br$date.time <"9/3/2020 12:00"),]
br<-br[!(br$date.time >= "9/7/2020 10:00" & br$date.time <= "9/7/2020 12:00"),]

ggplot(br, aes(date.time, temperature, group = sensor.depth))+
  geom_point(aes(col = sensor.depth))+
  facet_wrap(~logger.site)

ggplot(br, aes(date.time, dissolved.oxygen, group = sensor.depth))+
  geom_point(aes(col = sensor.depth))+
  facet_wrap(~logger.site)

arrays.clean<-do.call("rbind", list(bear, br, cot, nhb, nor, riv))

osu <- do.dat[do.dat$collected.by == "osu",]

osu<- osu%>% filter(date >'7/25/2021' & date <'8/14/2021')

ggplot(osu, aes(date.time, dissolved.oxygen, group = sensor.depth))+
  geom_point(aes(col = sensor.depth))+
  facet_wrap(~logger.site)

osu.s5 <-osu[osu$logger.site == "site.5.head",]

ggplot(osu.s5, aes(date.time, dissolved.oxygen, group =sensor.depth))+
  geom_point(aes(col = sensor.depth))+
  facet_wrap(~logger.site)

osu.s4 <-osu[osu$logger.site == "site.4.netpen",]

ggplot(osu.s4, aes(date.time, dissolved.oxygen, group =sensor.depth))+
  geom_point(aes(col = sensor.depth))+
  facet_wrap(~logger.site)


epa.osu.arrays.clean<-rbind(arrays.clean, osu)

mins <- epa.osu.arrays.clean %>%
  group_by(logger.site,date, sensor.depth)%>%
  summarize_at(vars(dissolved.oxygen), list(dissolved.oxygen = min))

max.d<- mins%>%
  group_by(logger.site,date)%>%
  slice(which.max(sensor.depth))

prop <- max.d %>%
  group_by(logger.site)%>%
  summarize_at(vars(dissolved.oxygen), list(dissolved.oxygen = mean))

sites <- prop[prop$dissolved.oxygen <2,]

maxes <- epa.osu.arrays.clean %>%
  group_by(logger.site,date, sensor.depth)%>%
  summarize_at(vars(dissolved.oxygen), list(dissolved.oxygen = max))

min.d<- maxes%>%
  group_by(logger.site,date)%>%
  slice(which.min(sensor.depth))

prop2 <- min.d %>%
  group_by(logger.site)%>%
  summarize_at(vars(dissolved.oxygen), list(dissolved.oxygen = mean))

max.hyp.props <- prop2[prop2$logger.site %in% sites$logger.site,]

  

#write.csv(epa.osu.arrays.clean, "data/modif.data/logger.array/epa.osu.logger.array.cleaned.csv", row.names = F)



library(readr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
setwd("C:/Users/barrehan/GitHub/projects/cwa.habitat.selection")

#'br logger array bottom DO logger had macrophyte fouling issues which spiked the 
#'DO, tried taking the average from days where the logger was clean but it really
#'messed up the temporal signal. I remembered that we had another DO logger array
#'quite close to the netpen where the depth and thermocline are quite similar, so 
#'we decided to combine these two arrays into one super array (old array with original
#'sensor data is named 'ORIGINAL' - this array file is the combination of the two arrays
#'with the bad DO at the bottom sensor removed)
#first checking out combined do/temp data from array site 4 and 5  

################################################################################

logger.array <- read.csv("data/modif.data/logger.array/blue.ruin.netpen.array.do.temp.csv")
logger.array$date.time <- mdy_hm(logger.array$date.time)
logger.array <- logger.array %>% force_tz(logger.array$date.time, tzone = "America/Los_Angeles")
logger.array<-logger.array%>% filter(date.time >'2021-07-25 12:00:00' & date.time < '2021-08-15 08:00:00')

logger.array$date.time<-round_date(logger.array$date.time, "5 minutes")
#logger.array<-logger.array[!(logger.array$sensor.depth=="1.55"),]
unique(logger.array$sensor.depth)
logger.array$sensor.depth<-as.factor(logger.array$sensor.depth)

ggplot(data= logger.array, aes(x = date.time))+
  geom_jitter(aes(y=temperature, colour = sensor.depth))+
  geom_jitter(aes(y=dissolved.oxygen, colour = sensor.depth))+
  scale_colour_manual(values = c("goldenrod2", "aquamarine3", "darkviolet", "darkblue", "darkorange", "red", "brown", "black"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_x_datetime(date_labels = "%b %d", date_breaks = "2 days")+
  xlab(label = "Date") +
  ylab(label = "")+
  theme(text = element_text(size = 15), legend.position = "right")

#YSI profiles from netpen site and logger site 5 to see how thermocline/oxycline
#compare

ysi.netpen <- read.csv("data/temp.do.data/ysi.profile.data/br.netpen.ysi.profile.csv")
ysi.netpen$location <- as.factor(ysi.netpen$location)
ggplot(data = ysi.netpen, aes(x = temp.c, y = depth.m, colour = location))+
  geom_point()+
  facet_wrap(~date)+
  scale_y_reverse()

ggplot(data = ysi.netpen, aes(x = do.mg.l, y = depth.m, colour = location))+
  geom_point()+
  facet_wrap(~date)+
  scale_y_reverse()

#' so DO profiles between the two sites are roughly similar but it is cooler at
#' site 5 which is why the temp at 1.35 depth is reading slightly cooler than temp at
#' 1.55 depth at the netpen (also check out differences in temp at ~0.5 m depth on 
#' the thermocline), makes sense since this site is closer to hyporheic plume -  
#' thinking about only using DO information from the site 5 logger
#' array but not the temps

logger.array$temperature[logger.array$sensor.depth =="1.35"] <- NA

logger.array$temperature[logger.array$sensor.depth =="0.4"] <- NA

ggplot(data= logger.array, aes(x = date.time))+
  geom_jitter(aes(y=temperature, colour = sensor.depth))+
  geom_jitter(aes(y=dissolved.oxygen, colour = sensor.depth))+
  scale_colour_manual(values = c("goldenrod2", "aquamarine3", "darkviolet", "dark blue", "darkorange", "red", "brown", "black"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_x_datetime(date_labels = "%b %d", date_breaks = "2 days")+
  xlab(label = "Date") +
  ylab(label = "")+
  theme(text = element_text(size = 15), legend.position = "right")

#'removing bottom logger DO from afternoon aug 2, eve of the 7th/early am 8th, and eve
#'9th/early am 10th

bot.log <- logger.array[logger.array$logger.position == "1.35_do",]
ggplot(data = bot.log, aes(x = date.time, y = dissolved.oxygen))+
  geom_jitter()

bot.log$dissolved.oxygen[bot.log$dissolved.oxygen < 2.3]<- NA
bot.log$dissolved.oxygen[bot.log$dissolved.oxygen > 5.2]<- NA
bot<-bot.log%>% filter(date.time >'2021-08-10 23:55:00')
bot2<- bot.log%>%filter(date.time <'2021-08-09 12:00:00')
bottom<- rbind(bot,bot2)

ggplot(data = bottom, aes(x = date.time, y = temperature))+
  geom_jitter()

new.array<- logger.array[!logger.array$logger.position == "1.35_do",]
new.array<- rbind(new.array, bottom)

#new.array$sensor.depth<- as.factor(new.array$sensor.depth)

plot <- ggplot(data = new.array, aes(x = date.time))+
  geom_jitter(aes(y = temperature, colour = sensor.depth), size = 1)+
  geom_jitter(aes(y=dissolved.oxygen, colour = sensor.depth), size =1)+
  scale_colour_manual(values = c("goldenrod2", "aquamarine3", "darkviolet", "dark blue", "darkorange", "red", "brown", "black"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_x_datetime(date_labels = "%b %d", date_breaks = "2 days")+
  scale_y_continuous(breaks=seq(0,24,2))+
  xlab(label = "Date") +
  ylab(label = "")+
  ggtitle(label = "Blue Ruin netpen logger array")+
  theme(text = element_text(size = 15), legend.position = "right")

#ggsave(plot, filename = paste("results/figures/logger.array/blue.ruin..netpen.do.temp.png"), width = 15, height = 8, units = "cm")


#write.csv(new.array, "data/temp.do.data/do.temp.combined/blue.ruin.combined.site.5.4.array.csv", row.names = F)

###############################################################################

# Just look at DO determine min/max windows -------------------------------

new.array$hourID <-hour(new.array$date.time)
new.array$hourID <- as.factor(new.array$hourID)

do.25 <- new.array[new.array$sensor.depth == 0.25,]
#discrete 24 hour color selection 
c24 <- c("dodgerblue2", "#E31A1C", "green4","#6A3D9A", "#FF7F00", "black", "gold1","skyblue2", "#FB9A99", "palegreen2", "#CAB2D6","#FDBF6F", "gray70", "khaki2", "maroon","orchid1", "deeppink1", "blue1", "steelblue4", "darkturquoise", "green1", "yellow4","yellow3", "darkorange4","brown")


p<-ggplot(data= do.25, aes(x = date.time, y = dissolved.oxygen, colour = hourID))+
  geom_jitter()+
  scale_colour_manual(values = c24)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_x_datetime(date_labels = "%b %d", date_breaks = "2 days")+
  xlab(label = "Date") +
  ylab(label = "")+
  theme(text = element_text(size = 15), legend.position = "right")

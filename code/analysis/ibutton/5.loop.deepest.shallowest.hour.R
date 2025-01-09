rm(list=ls())

Sys.setenv(TZ = "America/Los_Angeles")

library(ggplot2)
library(tidyverse)
library(lme4)
library(viridis)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggpubr)

setwd("C:/Users/barrehan/GitHub/projects/cwa.habitat.selection.dvm")

fish <-read.csv("data/modif.data/ibutton/all.ib.interp.depth.csv")
fish<-fish%>%
  mutate(date.time = parse_date_time(date.time, orders = "mdy HM"))

#fish<-fish %>% force_tz(fish$date.time, tzone = "America/Los_Angeles")
fish <-fish[fish$case ==1,]

# keeping times and adding fake day to make all reads on same day, 
# couldnt get formatting to work when trying to just do time for forloop

fish$hour <-hour(fish$date.time)
fish$date <-as.Date(fish$date.time)
fish$time <-fish$date.time
fish$time <- format(as.POSIXct(fish$date.time), format = "%H:%M:%S")
fish$fake.date <- "2021-01-01"
fish$time2 <- paste(fish$fake.date, fish$time)
fish$time2 <-ymd_hms(fish$time2)

buttons <-unique(fish$ibutton.id)
dates <- unique(fish$date)

dat <-setNames(data.frame(matrix(ncol = 12, nrow = 0)), c("site", "date.time", "temperature", "depth","dissolved.oxygen", "case", "ibutton.id", "hour", "date", "time", "fake.date", "min.max"))

for(i in 1:length(buttons)){
  ib<-buttons[i]
  f.match <- fish[fish$ibutton.id == ib,]
  for(j in 1:length(dates)){
    dt<-dates[j]
    dt.match <-f.match[f.match$date == dt,]
    if(!nrow(dt.match)){next}
    deep.row <- which(dt.match$depth == max(dt.match$depth, na.rm = TRUE)) # row index for 'max slope' (all slopes negative so max is closest to 0 slope)
    deep<-dt.match[deep.row,] # return row(s)
    deep$min.max <-"deepest"
    shal.row <-which(dt.match$depth == min(dt.match$depth, na.rm = TRUE))
    shal<-dt.match[shal.row,]
    shal$min.max <- "shallowest"
    
    new <-rbind(deep,shal)
    dat<-rbind(new,dat)
    
  }
}




shallow <-dat[dat$min.max == "shallowest" & dat$site == "norwood",]
deep <- dat[dat$min.max == "deepest" & dat$site == "norwood",]

f1<- ggplot(data = shallow, aes(x =hour))+
  geom_histogram(binwidth = 1, boundary = -7.5, colour = "black", fill = "lightsteelblue",size = .2)+
  coord_polar()+
  scale_x_continuous(limits = c(0, 24),
                     breaks = seq(0,24, by = 4),
                     minor_breaks = seq(0,24, by = 1))+
  theme_bw()+
  theme(panel.border = element_blank())+
  ggtitle("Hour of most shallow water column position")

f2<- ggplot(data = deep, aes(x =hour))+
  geom_histogram(binwidth = 1, boundary = -7.5, colour = "black", fill = "red4",size = .2)+
  coord_polar()+
  scale_x_continuous(limits = c(0, 24),
                     breaks = seq(0,24, by = 4),
                     minor_breaks = seq(0,24, by = 1))+
  theme_bw()+
  theme(panel.border = element_blank())+
  ggtitle("Hour of deepest water column position")

f3<-ggarrange(f1, f2, 
              ncol=2)

fish.block<-fish[fish$date.time >="2021-07-30 00:00:00" & fish$date.time < "2021-08-06 00:00:00",]

p<-ggplot(data = fish.block, aes (x = time2, y = depth))+
  #geom_point(alpha = .1, colour = "slategray")+
  geom_smooth(fill = "#7C5467", colour = "#291919")+
  scale_y_reverse()+
  scale_x_datetime(
    breaks = "2 hours",
    date_labels = "%H")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.key=element_rect(fill="white"), text = element_text(size = 20, family = "serif"))+
  #ggtitle("Smoothed fit fish depth across 24-hour period, Blue Ruin netpens")+
  xlab("Hour of Day")+
  ylab("Depth (m)")

p <- ggplot(data = fish.block, aes(x = time2, y = depth)) +
  #geom_point(alpha = .1, colour = "slategray") +
  geom_smooth(fill = "#7C5467", colour = "#291919") +
  scale_y_reverse(limits = c(1.6, 0.2), breaks = seq(1.6, 0.2, by = -0.4)) +
  scale_x_datetime(
    breaks = "2 hours",
    date_labels = "%H"
  ) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.key = element_rect(fill = "white"),
    text = element_text(size = 20, family = "serif")
  ) +
  #ggtitle("Smoothed fit fish depth across 24-hour period, Blue Ruin netpens") +
  xlab("Hour of Day") +
  ylab("Depth (m)")

ggsave(p, filename = paste("results/figures/ibutton.simulation/smoothed.fit.fish.depth.BRib.png"), width = 12, height = 8, units = "cm")

?scale_x_datetime

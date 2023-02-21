#'assign.fish.array.interpolate.depth.do.r : creating blue ruin map and interpolating
#'fish depth/do
#'
#'october 11, 2022
#'
#'receivers/antennas are assigned to logger arrays in an if else statement. 
#'logger array data is imported, cleaned, and combined into a single df, and a 
#'for loop determines fish closest array, and depth using linear interpolation. 
#'this new file is saved as a .csv to the modif data folder

#'packages used
library(readr)
library(tidyverse)
library(lubridate)
library(dplyr)
library(data.table)
library(rgdal)
library(cowplot) #grid ggplots
library(viridis)
#'clear workspace
rm(list = ls())
#close open graphics devices
graphics.off() 

#'working directory
setwd("C:/Users/barrehan/GitHub/projects/cwa.habitat.selection")

# Import fish tag data and array data, clean dates and timestamps ---------
fish.dat <- read.csv('data/modif.data/radio.tag/tag.reads.10.min.interval.csv')
fish.dat$date.time = mdy_hm(fish.dat$date.time)
fish.dat <- fish.dat %>% force_tz(fish.dat$date.time, tzone = "America/Los_Angeles")

#'assign closest logger array id
fish.dat$logger.site<- ifelse(fish.dat$receiver.site == 1 & fish.dat$antenna.number ==1, "site.0.mouth",
                        ifelse(fish.dat$receiver.site == 1 & fish.dat$antenna.number ==2, "site.1",
                               ifelse(fish.dat$receiver.site == 2 & fish.dat$antenna.number == 1, "site.2",
                                      ifelse(fish.dat$receiver.site == 2 & fish.dat$antenna.number == 2, "site.3",
                                             ifelse(fish.dat$receiver.site == 3, "site.3",
                                                    ifelse(fish.dat$receiver.site == 4, "site.4.netpen", NA))))))


#'import all array .csvs and create one data frame of all combined
s0<- read.csv("data/raw.data/logger.array/blue.ruin.site.0.river.temp.csv")
s0$date.time<-mdy_hm(s0$date.time)
s1<- read.csv("data/raw.data/logger.array/blue.ruin.site.1.mouth.temp.do.csv")
s1$date.time<-mdy_hm(s1$date.time)
#'round to nearest 5 min interval
s1$date.time<-round_date(s1$date.time,unit="5 minutes")
s2<-read.csv("data/modif.data/logger.array/blue.ruin.site.2.array.do.temp.csv")
s2$date.time<-mdy_hm(s2$date.time)
s3<-read.csv("data/raw.data/logger.array/blue.ruin.site.3.mid.temp.do.csv") 
s3$date.time<-mdy_hm(s3$date.time)
s4<- read.csv("data/modif.data/logger.array/blue.ruin.netpen.array.do.temp.csv")
s4$date.time<-mdy_hm(s4$date.time)
#rename logger site as s4.netpen - recall two loggers were snagged from the array 5
s4$logger.site[s4$logger.site %in% c("site.5.head")] = "site.4.netpen"

arrays<- do.call("rbind", list(s0, s1, s2, s3, s4))
arrays <- arrays %>% force_tz(arrays$date.time, tzone = "America/Los_Angeles")

#'write.csv(arrays, "data/modif.data/logger.array/all.arrays.5min.interval.csv", row.names= F)
#'curtail fish dat to narrowest array timeframe 7/26 - 8/15
fish <- fish.dat[fish.dat$date.time >= "2021-07-26 00:00:00" & fish.dat$date.time < "2021-08-15 07:50:00",]

#remove NA temp (from site 4/5 do/temp combination)
arrays<-arrays[!is.na(arrays$temperature),]

# For loop to interpolate fish depth using closest array ---------------

#if s0 depth is NA (mixed)
#if s1-s4 use interpolation to determine depth
for(i in 1:nrow(fish)){
  row <- fish[i,]
  t1 <-row$temp.strong                                                    #fish temperature
  t.match <- arrays[arrays$date.time == row$date.time,]                   #find matching time stamp
  a.match <- t.match[t.match$logger.site == row$logger.site,]             #find matching array
  x1 <- max(a.match$temperature[which(a.match$temperature < t1)])         #find the closest temperature less than ibutton temp (deeper sensor)
  x2 <- min(a.match$temperature[which(a.match$temperature > t1)])         #find the closest temperature greater than ibutton temp (shallower sensor)
  d1 <- a.match$sensor.depth[a.match$temperature == x1]                   #depth of deeper sensor (associated with x1)
  d2 <- a.match$sensor.depth[a.match$temperature == x2]                   #depth of shallower sensor (associated with x2)
  d <- (d2-d1)/(x2-x1)*(t1 - x1) + d1                                     #slope equation to calculate fish depth based on sensor depth/temp
                                                                          #what if the fish temperature is warmer than shallowest sensor/colder than deepest sensor?
  deepest <- a.match[which.max(a.match$sensor.depth),]                    #reference line with deepest sensor
  dep<-deepest$sensor.depth                                               #pull sensor depth information
  shallowest <-a.match[which.min(a.match$sensor.depth),]                  #reference line with shallowest sensor
  shal<-shallowest$sensor.depth                                           #pull sensor depth information
  d<-ifelse(is_empty(d1), dep, d)                                         #if fish temp is warmer than shallowest logger temp, assign that logger depth
  d<-ifelse(is_empty(d2), shal, d)                                        #if fish temp is colder than deepest logger temp, assign that logger depth
  m <- 'mouth.mixed'                                                   
  depth <-ifelse(row$receiver.site == 1 & row$antenna.number ==1, m, d)   #if closest array is temp sensor at mouth, there is no fish depth (mixed water column...)
  
  fish[i,11] <-depth            
}

colnames(fish)[11] <- "fish.depth"

# For loop to interpolate fish do using calculated depth ---------------

#' drop DO NAs from logger array df
do.array <- arrays%>%drop_na(dissolved.oxygen)
fish.2 <- fish
fish.2 <-fish.2[!fish.2$fish.depth == "mouth.mixed",]
fish.2 <-transform(fish.2, fish.depth = as.numeric(fish.depth))


for(i in 1:nrow(fish.2)){
  row <- fish.2[i,]
  match <-do.array[do.array$date.time == row$date.time,]
  a.match <- match[match$logger.site == row$logger.site,]
  depth.est <- row$fish.depth
  closest <-match[which.min(abs(depth.est-match$sensor.depth)),]
  closest.do<-closest$dissolved.oxygen
  x1<-max(a.match$sensor.depth[which(a.match$sensor.depth < depth.est)])
  x2<-min(a.match$sensor.depth[which(a.match$sensor.depth > depth.est)])
  d1 <- a.match$dissolved.oxygen[a.match$sensor.depth == x1]
  d2 <- a.match$dissolved.oxygen[a.match$sensor.depth == x2]
  do <- (d2-d1)/(x2-x1)*(depth.est - x1) + d1
  do<-ifelse(is_empty(do), closest.do, do)                                 
  
  fish.2[i,12] <-do
}

colnames(fish.2)[12] <- "fish.do"
fish.3 <- fish[fish$fish.depth == "mouth.mixed",]
fish.3$fish.do <- "NA"

fishes<- rbind(fish.2, fish.3)

#'order df by tag id and then by date

fishes <- fishes[
  order(fishes[,2], fishes[,1] ),
]

#write .csv with do/depth interpolation data
write.csv(fishes, 'data/modif.data/radio.tag/all.rt.depth.do.interpolated.csv', row.names = F)

# Make some plots to check out fish movement/depth/do ---------------------

# pull single tag and look at temp and receiver point plots

#fishes <- read.csv('data/modif.data/radio.tag/all.rt.depth.do.interpolated.csv')
#fishes$date.time<- mdy_hm(fishes$date.time)

plot.fish<-fishes[!fishes$fish.depth == "mouth.mixed",]
plot.fish$fish.depth<- as.numeric(plot.fish$fish.depth)
plot.fish$fish.do <- as.numeric(plot.fish$fish.do)
plot.fish$receiver.site <- as.factor(plot.fish$receiver.site)

unique.tag <- unique(plot.fish$tag.id)

for(i in unique.tag) {
p <- ggplot()+
  geom_point(data = subset(plot.fish, tag.id ==i), aes(date.time, fish.do, color = temp.strong,shape = receiver.site), size = 1)+
  geom_line(data = subset(plot.fish, tag.id ==i), aes(date.time, fish.do, color = temp.strong))+
  scale_color_viridis(option = "turbo", limits = c(10, 25))+
  xlab("Date")+
  ylab("Dissolved oxygen (mg/L)")+
  labs(color = "Fish temperature (°C)", shape = "Receiver site")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs(title = i)

ggsave(p, filename = paste("results/figures/radio.tag.figures/tag.temp.do.receiver/tag.", i,"do.temp.receiver.png"), width = 15, height = 8, units = "cm")

}

ggplot(plot.fish, aes(date.time, fish.do))+
  geom_point(aes(color = temp.strong, shape = receiver.site), size = 1)+
  geom_line()+
  scale_color_viridis(option = "inferno", limits = c(10,25))+
  xlab("Date")+
  ylab("Dissolved oxygen (mg/L)")+
  labs(color = "Fish temperature", shape = "Receiver site")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  facet_wrap(~tag.id)

# s11<-ggplot(t11, aes (date.time, receiver.site, color = temp.strong))+
#   geom_point()+
#   geom_line()+
#   scale_y_reverse()+
#   scale_color_viridis(option = "inferno")
#   
# d11<-ggplot(t11, aes (date.time, fish.depth, color = temp.strong))+
#   geom_point(aes(shape = receiver.site))+
#   geom_line(aes(date.time))+
#   scale_y_reverse()+
#   scale_color_viridis(option = "inferno")
# 
# do11<- ggplot(t11, aes (date.time, fish.do, color = temp.strong))+
#   geom_point()+
#   geom_line()+
#   scale_color_viridis(option = "inferno")
# 
# plot_grid(s11, d11, do11,labels = c("Receiver site", "Fish depth", "Fish do"), ncol = 3, nrow =1)
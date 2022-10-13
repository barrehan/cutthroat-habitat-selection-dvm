#'assign.fish.array.interpolate.depth.do.r : creating blue ruin map and interpolating
#'fish depth/do
#'
#'october 11, 2022
#'
#'this script first creates a figure of the blue ruin polygon with points 
#'representing receiver/array locations. receivers/antennas are then assigned to
#'logger arrays in an if else statement. logger array data is imported, cleaned, 
#'and combined into a single df, and a for loop determines fish closest array, and 
#'depth using linear interpolation. this new file is saved as a .csv to the modif
#'data folder

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

# Create Blue Ruin polygon with points for arrays/receivers ---------------
#'import all fish tag data corrected to all occur at ten minute intervals
fish.dat <- read.csv('data/modif.data/radio.tag/tag.reads.10.min.interval.csv')
fish.dat$date.time = mdy_hm(fish.dat$date.time)
fish.dat <- fish.dat %>% force_tz(fish.dat$date.time, tzone = "America/Los_Angeles")
#'import array and receiver location data
receiver.dat <- unique(fish.dat[c("receiver.site", "antenna.number", "lat", 'long')])
receiver.dat<- receiver.dat %>%arrange(receiver.site, antenna.number)
receiver.dat$ID <- 1:nrow(receiver.dat)

logger.dat<- read.csv('data/raw.data/logger.array/logger.array.setup.csv')
logger.dat<- logger.dat[!(logger.dat$site == "telemetry.slough"),]
logger.dat <- logger.dat[-c(1,3:7,10:12)]
logger.dat<-unique(logger.dat[c(1:3)])

#plot all points on map
br.poly <- readOGR("data/modif.data/blue.ruin.polygon/brpolygon.shp")

##figure out what the projection is so I can use it for my radio tag data below
br.poly@proj4string

plot(br.poly, axes = TRUE)

plot(br.poly, height = 700, width = 900) 


##you have to fortify your shapefile to make it work with ggplot2
br.poly <- fortify(br.poly)

# Now the shape file can be plotted as either a geom_path or a geom_polygon.
# Paths handle clipping better. Polygons can be filled.
# You need the aesthetics long, lat, and group.
receiver.dat$ID<-as.factor(receiver.dat$ID)

ggplot() +
  geom_polygon(data = br.poly, 
               aes(x = long, y = lat, group = group), fill = "light grey")
colors <- c("#D8B70A", "#D67236", "#02401B", "#A2A475", "#81A88D", "#972D15")

map.points <- ggplot() +
  geom_polygon(data =br.poly, 
               aes(x = long, y = lat, group = group), fill = "light grey") +
  geom_jitter(data = receiver.dat, 
             aes(x=long, y = lat, colour = ID), width = 0.00001, size = 2, shape = 16)+
  scale_color_manual(values = colors)+
  geom_point(data = logger.dat, 
             aes(x = lon, y = lat), size = 2, shape = 6)+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  labs(color = "Antenna")
  
#ggsave("results/figures/radio.tag.figures/blue.ruin.arrays.receivers.png", 
#width = 15, height = 10, units = "cm")


# Import fish tag data and array data, clean dates and timestamps ---------

#' take fish radio tag data and array data to interpolate fish depth/do using 
#' fish temp and nearest array



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

#'curtail fish dat to narrowest array timeframe 7/26 - 8/15
fish <- fish.dat[fish.dat$date.time >= "2021-07-26 00:00:00" & fish.dat$date.time < "2021-08-15 07:50:00",]

#remove NA temp (from site 4/5 do/temp combination)
arrays<-arrays[!is.na(arrays$temperature),]

# For loop to interpolate fish depth/do using closest array ---------------

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

colnames(fish)[11] <- "interp.fish.depth"

#'order df by tag id and then by date

fish <- fish[
  order(fish[,2], fish[,1] ),
]

fishes<- fish
fishes <- mutate_at(fishes, vars(receiver.site), as.factor)
t11<- fishes[fishes$tag.id == 11,]
t11<-t11[!t11$interp.fish.depth == "mouth.mixed",]
t11$interp.fish.depth<- as.numeric(t11$interp.fish.depth)
t11$receiver.site <-fct_rev(t11$receiver.site)

s11<-ggplot(t11, aes (date.time, receiver.site, color = temp.strong))+
  geom_point()+
  geom_line()+
  scale_y_reverse()+
  scale_color_viridis(option = "inferno")
  
d11<-ggplot(t11, aes (date.time, interp.fish.depth, color = temp.strong))+
  geom_point()+
  geom_line()+
  scale_y_reverse()+
  scale_color_viridis(option = "inferno")

plot_grid(s11,d11,labels = c("Receiver site", "Fish depth"), ncol = 2, nrow =1)


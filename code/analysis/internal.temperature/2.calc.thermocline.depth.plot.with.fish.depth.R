rm(list=ls())

library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(viridis)
library(nlme)
library(emmeans)
library(lme4)
library(rstatix)
library(lubridate)
library(wesanderson)

setwd("C:/Users/barrehan/GitHub/projects/cwa.habitat.selection")

ysi <- read.csv("data/raw.data/internal.temperature/2019.2021.thermocline.fish.internal.temp.cwa.csv")
ysi$date.time <- mdy_hms(ysi$date.time)

#create columns for next depth and temp at that depth to calculate slope

ysi.mod<- ysi %>%
  group_by(date.time)%>%
  mutate(next.temp = lead(temperature.c)) %>%
  mutate(next.depth = lead(depth.m))%>%
  ungroup()

ysi.mod<-ysi.mod[,c(1,4,5,8:10,12:13)]

# calculate slopes between points

for(i in 1:nrow(ysi.mod)){
  row <-ysi.mod[i,]
  x1 <- row$next.temp
  x2 <- row$temperature.c
  y1 <-row$next.depth
  y2<- row$depth.m
  slope <- ((y1-y2)/(x1-x2))
  s2 <- 'NA'
  slope<-ifelse(slope =="NA", s2, slope)
  ysi.mod[i,9] <- slope
}

names(ysi.mod)[9] <- "slope"

ysi.mod$abs.slope <-abs(ysi.mod$slope)

#create blank data frame
dat <-setNames(data.frame(matrix(ncol = 10, nrow = 0)), c("site","date.time", "thermo.measure.loc", "depth", "temperature", "dissolved.oxygen", "next.temp", "next.depth", "slope", "abs.slope"))

prof <- unique(ysi.mod$thermo.measure.loc)
#find least slope, find depth at slope midpoint

for(i in 1:length(prof)){
  t<-prof[i]
  t1 <- ysi.mod %>% filter(thermo.measure.loc == prof[i]) ## Filtering out the logger data for this "Time"
  m <- which(t1$abs.slope == min(t1$abs.slope, na.rm = TRUE)) # row index for 'max slope' (all slopes negative so max is closest to 0 slope)
  df<-t1[m,] # return row(s)
  mt<-which(df$depth.m == max(df$depth.m)) # which row has the min temp
  d<-df[mt,] # add to new df
  
  dat <-rbind(dat,d)
}

thermocline.depth <-dat

#check out one of the ysi profiles and the calculated thermocline depth
step <-ysi.mod[ysi.mod$thermo.measure.loc == "cottonwood.site.4",]
thermo.step <-dat[dat$thermo.measure.loc == "cottonwood.site.4",]

ggplot()+
  geom_point(data = step, aes(x = temperature.c, y= depth.m), color = "red")+
  geom_point(data = thermo.step, aes(x = temperature.c, y= depth.m), color = "blue")+
  scale_y_reverse()

#bring in fish internal temp data and associated depths
fish <- read.csv("data/raw.data/internal.temperature/2019.2021.fish.internal.temp.cwa.csv")
fish$date.time <- mdy_hms(fish$date.time)

fish <-fish[-c(10)]

for(i in 1:nrow(fish)){
  row <- fish[i,]
  match <- ysi[ysi$thermo.measure.loc == row$thermo.measure.loc,]
  t1 <- row$fish.temp
  x1 <- max(match$temperature.c[which(match$temperature.c < t1)])
  x2 <- min(match$temperature.c[which(match$temperature.c > t1)])
  d1 <- match$depth.m[match$temperature.c == x1]
  d2 <- match$depth.m[match$temperature.c == x2]
  d <- (d2-d1)/(x2-x1)*(t1 - x1) + d1
  depth <- ifelse(length(d > 1), mean(d), d)
  #if there are two value options take the average between the two...
  fish[i,10] <- depth
}
colnames(fish)[10] <- "fish.depth"

fd<-fish[c(8, 10)]
sd<-thermocline.depth[c(3,4)]

depths<-merge(fd,sd, by ="thermo.measure.loc")

#write.csv(depths, "data/modif.data/internal.temp/fish.depth.thermo.depth.csv", row.names = F)

depths$thermo.measure.loc <-factor(depths$thermo.measure.loc, levels = c("harrisburg.site.4", "br.0822.thermo.1", "cottonwood.site.4", "nor.0827.thermo.2", "cottonwood.site.1", "nor.0827.thermo.1", "br.0822.thermo.4", "harrisburg.site.3", "br.0822.thermo.2", "br.0822.thermo.3", "harrisburg.site.2", "harrisburg.site.1"), labels = c("South Harrisburg site 4", "Blue Ruin site 1", "Cottonwood site 4", "Norwood site 2", "Cottonwood site 1", "Norwood site 1", "Blue Ruin site 4", "South Harrisburg site 3", "Blue Ruin site 2", "Blue Ruin site 3", "South Harrisburg site 2", "Sough Harrisburg site 1"))

depths$thermo.measure.loc

depths$thermo.measure.loc

depths$thermo.measure.loc <- factor(depths$thermo.measure.loc)

depths$thermo.measure.loc <- factor(depths$thermo.measure.loc, levels = c("harrisburg.site.4", "br.0822.thermo.1", "cottonwood.site.4", "nor.0827.thermo.2", "cottonwood.site.1", "nor.0827.thermo.1", "br.0822.thermo.4", "harrisburg.site.3",
                                                                         "br.0822.thermo.2", "br.0822.thermo.3", "harrisburg.site.2", "harrisburg.site.1"))

wes_palette("Cavalcanti1")

colors <- c("#273046", "#D8B70A", "#02401B", "#A2A475", "#81A88D", "#03436A", "#78B7C5", "#FD6467","#CB2314", "#9986A5", "#F1BB7B", "#79402E", "#D67236", "#972D15")

ggplot(depths, aes(depth.m, fish.depth, color = thermo.measure.loc))+
  geom_point()+
  geom_abline(aes(slope = 1, intercept = 0, colour = "1:1 line"), size= 1.5, linetype = "dashed", show.legend = F, alpha = 0.75)+
  scale_color_manual(values = colors, name = "")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "right", legend.key = element_rect(fill = "transparent"),
        text = element_text(size = 12, family = "serif"))+
  xlab("Thermocline depth (m)")+
  ylab("Fish depth (m)")
  
  


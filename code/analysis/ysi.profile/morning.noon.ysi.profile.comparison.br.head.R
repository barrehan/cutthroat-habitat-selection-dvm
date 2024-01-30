#do/temp profiles from blue ruin terminus early morning and late afternoon
#figure 1 for dvm paper

#TO DO: interpolate between points for noon reading to estimate temp and do
#at 0.1 m intervals
rm(list=ls())

library(readr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggpubr)

setwd("C:/Users/barrehan/GitHub/projects/cwa.habitat.selection.dvm")

br<-read.csv("data/raw.data/ysi.profile/br.netpen.ysi.profile.csv")

#factor daytime period (morning/noon)
br$day.segment <-as.factor(br$day.segment)
#profiles for site 5 head look best, pull these rows
log5 <- br[br$location == "site.5.logger.5",]

#interpolate between depths for noon reading to estimate temp and do
#at 0.1m intervals

log5.noon <-log5[log5$day.segment == "noon",]
log5.morning<-log5[log5$day.segment == "morning",] %>%
  select(depth.m, temp.c, do.mg.l, day.segment)
  


new.dat<-as.data.frame(matrix(ncol=4,nrow=17))

di<- as.data.frame(seq(from = 0.0, to = 1.6, by = 0.1))
cntr = 0

#'x= (y-b)/m

for(i in 1:nrow(di)){
  # dist <- as.data.frame(seq(from = 0.0, to = 1.6, by = 0.1)) #max depth 0.05 less than deepest sensor so we can interpolate
  # for(i in 1:nrow(dist)){
    d.unif<- di[i,]
    d1 <- max(log5.noon$depth.m[which(log5.noon$depth.m < d.unif)])
    d2 <- min(log5.noon$depth.m[which(log5.noon$depth.m > d.unif)])
    t1 <- log5.noon$temp.c[log5.noon$depth.m == d1]
    t2 <- log5.noon$temp.c[log5.noon$depth.m == d2]
    t <- (((d.unif-d1)*(t2-t1))/(d2-d1))+t1
    match.row <- log5.noon[log5.noon$depth.m == d.unif,]
    match.d <-match.row$depth.m
    match.t <-match.row$temp.c
    tf<-ifelse(is_empty(match.d), t, match.t)
    
    do1<- log5.noon$do.mg.l[log5.noon$depth.m == d1]
    do2<- log5.noon$do.mg.l[log5.noon$depth.m == d2]
    do <- (((d.unif-d1)*(do2-do1))/(d2-d1))+do1
    
    match.do <-match.row$do.mg.l
    dof <- ifelse(is_empty(match.d), do, match.do)
   
    cntr<-cntr+1 #start a new row
    new.dat[cntr,1]<-d.unif 
    new.dat[cntr,2]<-tf 
    new.dat[cntr,3] <-dof
    new.dat[cntr,4]<- "noon" 
  }

colnames(new.dat)<- c("depth.m", "temp.c", "do.mg.l", "day.segment")

log5.new <-rbind(new.dat, log5.morning)

log5.new <- log5.new[order(log5.new$depth.m),]  

f1 <- ggplot(log5.new, aes(x = temp.c, y = depth.m, col = day.segment))+
  geom_point(size = 1.5, alpha = 0.9)+
  geom_path(linewidth = 1)+
  scale_y_reverse()+
  scale_color_manual(values = c("#8C2B0E", "#FEB359"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "right", legend.key = element_rect(fill = "transparent"),
        text = element_text(size = 15, family = "serif"), legend.title = element_blank())+
  ylab("Depth (m)")+
  xlab("Temperature (\u00B0C)")

f2 <- ggplot(log5.new, aes(x = do.mg.l, y = depth.m, col = day.segment))+
  geom_point(size = 1.5, alpha = 0.9)+
  geom_path(linewidth = 1)+
  scale_y_reverse()+
  scale_color_manual(values = c("#8C2B0E", "#FEB359"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "right", legend.key = element_rect(fill = "transparent"),
        text = element_text(size = 15, family = "serif"), legend.title = element_blank())+
  ylab("Depth (m)")+
  xlab("Dissolved oxygen (mg/L)")

f <-ggarrange(f1, f2,
                   ncol =2,
                   common.legend = T,
                   legend = "bottom")

ggsave(f, filename = paste("results/figures/ysi.profile/br.head.morning.noon.ysi.png"), width = 15, height = 10, units = "cm")



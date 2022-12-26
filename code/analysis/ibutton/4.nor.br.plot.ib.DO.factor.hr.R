library(readr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(RColorBrewer)
library(viridis)
library(grid)
library(gridExtra)
library(data.table)
library(ggpubr)

setwd("C:/Users/barrehan/GitHub/projects/cwa.habitat.selection/data/modif.data/ibutton/do.depth.interpolation")

list_csv_files <- list.files(path = "/Users/barrehan/GitHub/projects/cwa.habitat.selection/data/modif.data/ibutton/do.depth.interpolation/")
df <- do.call(rbind, lapply(list_csv_files, function(x) read.csv(x, stringsAsFactors = FALSE)))

setwd("C:/Users/barrehan/GitHub/projects/cwa.habitat.selection")

df$date.time<- ymd_hms(df$date.time)
df$hour <- hour(df$date.time)
df$hour <- as.factor(df$hour)

nor <- df[df$site == "norwood.mouth",]
br <- df[df$site == "blue.ruin.1" | df$site == "blue.ruin.2",]

#discrete 24 hour color selection 
c24 <- c("dodgerblue2", "#E31A1C", "green4","#6A3D9A", "#FF7F00", "black", "gold1","skyblue2", "#FB9A99", "palegreen2", "#CAB2D6","#FDBF6F", "gray70", "khaki2", "maroon","orchid1", "deeppink1", "blue1", "steelblue4", "darkturquoise", "green1", "yellow4","yellow3", "darkorange4","brown")

#figure out horizontal line for mean temperature
nor.avg<-mean(nor$dissolved.oxygen)
nor.sd<- sd(nor$dissolved.oxygen)
nor.stdev <-nor.avg-nor.sd
#Norwood ibuttons DO

tag.id<- unique(nor$ibutton)

for(i in tag.id){
  
  p<- ggplot(data =subset(nor, ibutton == i), aes(date.time, dissolved.oxygen))+
    geom_point(aes(colour = hour), size = 2)+
    scale_color_manual(values = c24)+
    geom_hline(yintercept = nor.avg)+
    geom_hline(yintercept = nor.stdev)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))+
    scale_x_datetime(date_labels = "%b %d", date_breaks = "2 days")+
    xlab(label = "Date") +
    ylab(label = "Dissolved oxygen (mg/l)")+
    theme(text = element_text(size = 15))
  
  fig<-annotate_figure(p,
                       top = text_grob(i, face = "bold", size = 16),
                       fig.lab = "Norwood")
  
  ggsave(fig, file = paste0("results/figures/ibutton/norwood.ibutton.", i,".do.temp.depth.hourly.jpg"), width = 12, height = 8, units = "in")
  
}

br.avg<-mean(br$dissolved.oxygen)
br.sd<- sd(br$dissolved.oxygen)
br.stdev <-br.avg-nor.sd

tag.id<- unique(br$ibutton)

for(i in tag.id){
  
  p<- ggplot(data =subset(br, ibutton == i), aes(date.time, dissolved.oxygen))+
    geom_point(aes(colour = hour), size = 2)+
    scale_color_manual(values = c24)+
    geom_hline(yintercept = br.avg)+
    geom_hline(yintercept = br.stdev)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))+
    scale_x_datetime(date_labels = "%b %d", date_breaks = "2 days")+
    xlab(label = "Date") +
    ylab(label = "Dissolved oxygen (mg/l)")+
    theme(text = element_text(size = 15))
  
  fig<-annotate_figure(p,
                       top = text_grob(i, face = "bold", size = 16),
                       fig.lab = "Blue Ruin")
  #ggsave(ibutton.plot, file=paste0("figures/ibutton.figures/br.netpen1/br.netpen1.ibutton.", i,".png"), width = 20, height = 10, units = "cm")
  ggsave(fig, file = paste0("results/figures/ibutton/br.ibutton.", i,".do.temp.depth.hourly.jpg"), width = 12, height = 8, units = "in")
  
}





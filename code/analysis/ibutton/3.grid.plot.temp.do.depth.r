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

setwd("C:/Users/barrehan/GitHub/projects/cwa.habitat.selection.dvm/data/modif.data/ibutton/do.depth.interpolation")

list_csv_files <- list.files(path = "/Users/barrehan/GitHub/projects/cwa.habitat.selection.dvm/data/modif.data/ibutton/do.depth.interpolation/")
df <- do.call(rbind, lapply(list_csv_files, function(x) read.csv(x, stringsAsFactors = FALSE)))

setwd("C:/Users/barrehan/GitHub/projects/cwa.habitat.selection.dvm")

df$date.time<- ymd_hms(df$date.time)
df$hour <- hour(df$date.time)

df<-df[df$date.time >="2021-07-30 00:00:00" & df$date.time < "2021-08-06 00:00:00",]

nor <- df[df$site == "norwood.mouth",]
br <- df[df$site == "blue.ruin.1" | df$site == "blue.ruin.2",]

#discrete 24 hour color selection could maybe use at some point
#c24 <- c("dodgerblue2", "#E31A1C", "green4","#6A3D9A", "#FF7F00", "black", "gold1","skyblue2", "#FB9A99", "palegreen2", "#CAB2D6","#FDBF6F", "gray70", "khaki2", "maroon","orchid1", "deeppink1", "blue1", "steelblue4", "darkturquoise", "green1", "yellow4","yellow3", "darkorange4","brown)

tag.id<- unique(nor$ibutton)

for(i in tag.id){
p1<- ggplot(data =subset(nor, ibutton == i), aes(date.time, fish.depth))+
  geom_point(aes(colour = hour), size = 2)+
 # scale_color_viridis(option = "A")+
  scale_color_gradientn(colours = c("#291919", "#532A34", "#7C5467", "#878195", "#AEB2B7", "#D4D9DD"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.key=element_rect(fill="white"), text = element_text(size = 15, family = "serif"))+
  scale_y_reverse()+
  xlab(label = "Date") +
  ylab (label = "Fish depth (m)")

p2<- ggplot(data =subset(nor, ibutton == i), aes(date.time, ibutton.temp))+
  geom_point(aes(colour = hour), size = 2)+
  scale_color_gradientn(colours = c("#291919", "#532A34", "#7C5467", "#878195", "#AEB2B7", "#D4D9DD"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.key=element_rect(fill="white"), text = element_text(size = 15, family = "serif"))+
  scale_x_datetime(date_labels = "%b %d", date_breaks = "2 days")+
  xlab(label = "Date") +
  ylab (label = "Fish temperature (\u00B0C)")

p3<- ggplot(data =subset(nor, ibutton == i), aes(date.time, dissolved.oxygen))+
  geom_point(aes(colour = hour), size = 2)+
  scale_color_gradientn(colours = c("#291919", "#532A34", "#7C5467", "#878195", "#AEB2B7", "#D4D9DD"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.key=element_rect(fill="white"), text = element_text(size = 15, family = "serif"))+
  scale_x_datetime(date_labels = "%b %d", date_breaks = "2 days")+
  xlab(label = "Date") +
  ylab(label = "Dissolved oxygen (mg/l)")

figure <- ggarrange(p2, p1, p3 + rremove("x.text"),
                    ncol = 1, nrow = 3)

fig<-annotate_figure(figure,
                top = text_grob(i, face = "bold", size = 16),
                fig.lab = "Norwood")
#ggsave(ibutton.plot, file=paste0("figures/ibutton.figures/br.netpen1/br.netpen1.ibutton.", i,".png"), width = 20, height = 10, units = "cm")
ggsave(fig, file = paste0("results/figures/ibutton/norwood.ibutton.", i,".do.temp.depth.jpg"), width = 12, height = 8, units = "in")

}



tag.id<- unique(br$ibutton)

for(i in tag.id){
  p1<- ggplot(data =subset(br, ibutton == i), aes(date.time, fish.depth))+
    geom_point(aes(colour = hour), size = 2)+
    scale_color_gradientn(colours = c("#291919", "#532A34", "#7C5467", "#878195", "#AEB2B7", "#D4D9DD"))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          legend.key=element_rect(fill="white"), text = element_text(size = 15, family = "serif"))+
    scale_x_datetime(date_labels = "%b %d", date_breaks = "2 days")+
    scale_y_reverse()+
    xlab(label = "Date") +
    ylab (label = "Fish depth (m)")+
    theme(text = element_text(size = 15)) 
  
  p2<- ggplot(data =subset(br, ibutton == i), aes(date.time, ibutton.temp))+
    geom_point(aes(colour = hour), size = 2)+
    #scale_color_viridis(option = "A")+
    scale_color_gradientn(colours = c("#291919", "#532A34", "#7C5467", "#878195", "#AEB2B7", "#D4D9DD"))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          legend.key=element_rect(fill="white"), text = element_text(size = 15, family = "serif"))+
    scale_x_datetime(date_labels = "%b %d", date_breaks = "2 days")+
    xlab(label = "Date") +
    ylab (label = "Fish temperature (\u00B0C)")
  
  p3<- ggplot(data =subset(br, ibutton == i), aes(date.time, dissolved.oxygen))+
    geom_point(aes(colour = hour), size = 2)+
    scale_color_gradientn(colours = c("#291919", "#532A34", "#7C5467", "#878195", "#AEB2B7", "#D4D9DD"))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          legend.key=element_rect(fill="white"), text = element_text(size = 15, family = "serif"))+
    scale_x_datetime(date_labels = "%b %d", date_breaks = "2 days")+
    xlab(label = "Date") +
    ylab(label = "Dissolved oxygen (mg/l)")
  
  figure <- ggarrange(p2, p1, p3 + rremove("x.text"),
                      ncol = 1, nrow = 3)
  
  fig<-annotate_figure(figure,
                       top = text_grob(i, face = "bold", size = 16),
                       fig.lab = "Blue ruin")
  #ggsave(ibutton.plot, file=paste0("figures/ibutton.figures/br.netpen1/br.netpen1.ibutton.", i,".png"), width = 20, height = 10, units = "cm")
  ggsave(fig, file = paste0("results/figures/ibutton/br.ibutton.", i,".do.temp.depth.jpg"), width = 12, height = 8, units = "in")
  
}

array <- read.csv("data/raw.data/logger.array/norwood.mouth.temp.do.csv")
unique(array$sensor.depth)
array$sensor.depth<- as.factor(array$sensor.depth)
array$date.time<- mdy_hm(array$date.time) 
array<- array[array$date.time >"2021-07-25 00:00:00",]
array<- array[array$date.time < "2021-08-14 00:00:00",]

pp<-ggplot(data= array, aes(x = date.time))+
  geom_jitter(aes(y=temperature, colour = sensor.depth))+
  geom_jitter(aes(y=dissolved.oxygen, colour = sensor.depth))+
  scale_colour_manual(values = c("goldenrod2", "aquamarine3", "deeppink4", "darkviolet", "darkorange", "dark blue",  "red"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_x_datetime(date_labels = "%b %d", date_breaks = "2 days")+
  xlab(label = "Date") +
  ylab(label = "")+
  theme(text = element_text(size = 15), legend.position = "right")

ggsave(pp, file=paste0("results/figures/logger.array/norwood.temp.do.depth.time.png"), width = 50, height = 30, units = "cm") 
 

  


library(ggplot2)
library(lubridate)
library(dplyr)
library(colorRamps)
library(viridis)

 
setwd("C:/Users/barrehan/GitHub/projects/cwa.habitat.selection")
all.tags <- read.csv("data/modif.data/radio.tag/tag.reads.10.min.interval.csv")
all.tags$date.time = mdy_hm(all.tags$date.time)
##add a column where time is grouped into quarters so we can graph
##each time interval as a different color to see if there is any
##movement associated with day/night low/high DO
 
all.tags <- all.tags %>% mutate(time.interval = case_when(format(date.time, '%H:%M') <= "06:00" ~ "00:00-06:00",
                                                          format(date.time, '%H:%M') <= "12:00" ~ "06:00-12:00",
                                                          format(date.time, '%H:%M') <= "18:00" ~ "12:00-18:00",
                                                          format(date.time, '%H:%M') <= "24:00" ~ "18:00-24:00"))

####added another column where day and night are identified (6am-6pm, 6pm-6am)

all.tags <- all.tags %>% mutate(day.night = ifelse(format(date.time, '%H:%M') >= "06:00" & format(date.time, '%H:%M') <= "18:00", "day", "night" ))

View(all.tags)


ggplot(data = all.tags, aes(x = date.time, y = receiver.site, color = time.interval, shape = day.night)) +
  geom_jitter(height = .1) +
  theme_bw() +
  scale_color_manual(values = c("00:00-06:00" = "blue", "06:00-12:00" = "dark green",'12:00-18:00' = "red", '18:00-24:00' = "yellow"  ))+
  facet_wrap(~tag.id)

################################################################################
#################forloop prints out graph for each tag id#######################

 uniqe_tag = unique(all.tags$tag.id)
 
for (i in uniqe_tag) {
    movement.plot <- ggplot(data = subset(all.tags, tag.id == i), 
                            aes(x= date.time, y = receiver.site, color = time.interval, 
                                shape = day.night)) +
      geom_jitter(height = 0.1)+
      theme_bw() +
      scale_color_manual(values = c("00:00-06:00" = "blue", "06:00-12:00" = "dark green",
                                    "12:00-18:00" = "red", "18:00-24:00" = "yellow"))+
      ggtitle(i)
        
      ggsave(movement.plot, file=paste0("figures/rt.movement.plot/diel.plot.tag.", i,".png"), width = 14, height = 10, units = "cm")
         
}

#################################################################################
##########look at movement in association with external temperature##############


ggplot(data = all.tags, aes(x = date.time, y = receiver.site, color = temp.strong)) +
  geom_jitter(height = .1) +
  theme_bw() +
  scale_colour_gradient(low = "blue", high = "firebrick3")+
  facet_wrap(~tag.id)

ggplot(data = all.tags, aes(x = date.time, y = receiver.site, color = temp.strong)) +
  geom_jitter(height = .1) +
  theme_bw() +
  scale_colour_viridis()+
  facet_wrap(~tag.id)

all.tags$tag.id <- as.factor(all.tags$tag.id)

ggplot(data = all.tags, aes(y = temp.strong, x= tag.id, group = tag.id))+
  geom_boxplot()+
  theme_bw() +
  theme(legend.position="none") +
  xlab("Tag ID") + ylab(expression("Temperature " ( degree~C)))+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))

##forloop to print graph for each tag id with temperature as the color variable

for (i in uniqe_tag) {
  movement.plot <- ggplot(data = subset(all.tags, tag.id == i), 
                          aes(x= date.time, y = receiver.site, color = temp.strong)) +
    geom_jitter(height = 0.1)+
    theme_bw() +
    scale_colour_viridis()+
    ggtitle(i)
  
  ggsave(movement.plot, file=paste0("figures/rt.movement.plot/temp.plot.tag.", i,".png"), width = 14, height = 10, units = "cm")
  
}

?colorRamps

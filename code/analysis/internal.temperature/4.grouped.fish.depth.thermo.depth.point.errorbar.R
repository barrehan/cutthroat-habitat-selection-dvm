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
library(ggstatsplot)
library(gridExtra)
library(data.table)
library(ggpubr)
library(reshape)

setwd("C:/Users/barrehan/GitHub/projects/cwa.habitat.selection")

dater <- read.csv("data/modif.data/internal.temp/fish.depth.thermo.depth.csv")
dater$difference <-dater$depth.m - dater$fish.depth

sum<-dater%>%
  group_by(thermo.measure.loc)%>%
  summarize(min = min(difference),
            q1 = quantile(difference, 0.25),
            median = median(difference),
            mean = mean(difference),
            q3 = quantile(difference, 0.75),
            max = max(difference))

#"#273046" 1:1 line color
sum$thermo.measure.loc <-factor(sum$thermo.measure.loc, levels = c("br.0822.thermo.1", "br.0822.thermo.2", "br.0822.thermo.3", "br.0822.thermo.4", "cottonwood.site.1", "cottonwood.site.4", "nor.0827.thermo.1", "nor.0827.thermo.2", "harrisburg.site.1", "harrisburg.site.2",  "harrisburg.site.3", "harrisburg.site.4"), labels = c("Blue Ruin site 1", "Blue Ruin site 2", "Blue Ruin site 3", "Blue Ruin site 4", "Cottonwood site 1", "Cottonwood site 2", "Norwood site 1", "Norwood site 2", "South Harrisburg site 1", "South Harrisburg site 2", "South Harrisburg site 3", "South Harrisburg site 4"))

colors <- c("#D8B70A", "#02401B", "#A2A475", "seagreen4", "#78B7C5","#03436A",  "#FD6467","#CB2314", "#9986A5", "#F1BB7B", "#79402E", "#D67236", "#972D15")


ggplot(data = sum, aes(x = thermo.measure.loc, y = mean, color = thermo.measure.loc))+
  geom_point(size = 2)+
  scale_color_manual(values = colors, name = "")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  geom_errorbar(aes(ymin = q1, ymax = q3, width = .25))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "right", legend.key = element_rect(fill = "transparent"),
        text = element_text(size = 12, family = "serif"))+
  xlab("")+
  ylab("Average difference between fish depth and thermocline depth with 1st and 3rd interquartiles")+
  geom_vline(xintercept = c(4.5, 6.5, 8.5), linetype= "dashed")

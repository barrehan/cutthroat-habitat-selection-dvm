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

setwd("C:/Users/barrehan/GitHub/projects/cwa.habitat.selection")
dater<-read.csv("data/modif.data/internal.temp/fish.depth.thermo.depth.csv")
names(dater)[4]<-"thermocline.depth"
names(dater)[2] <- "profile"

dater<-na.omit(dater)

mod1<-lme(fish.depth ~thermocline.depth, data = dater,
          random = ~1|location)
summary(mod1)

mod2<-lm(fish.depth~thermocline.depth, data = dater)
summary(mod2)

anova(mod1, mod2)

#model 1 with random effect is better

effects<-effects::effect(term = "thermocline.depth", mod = mod1)
x_temp <-as.data.frame(effects)

ggplot()+
  geom_point(data = dater, aes(x =thermocline.depth, y = fish.depth))+
  geom_line(data =x_temp, aes(thermocline.depth, y =fit), colour = "#D67236", linewidth = 1)+
  geom_ribbon(data = x_temp, aes(x = thermocline.depth, ymin = lower, ymax = upper), alpha = 0.3, fill =  "#F1BB7B")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 12, family = "serif"))+
  #scale_y_reverse()+
  xlab("Temperature °C")+
  ylab("Dissolved oxygen (mg/L)")



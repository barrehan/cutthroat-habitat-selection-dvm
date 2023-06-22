rm(list=ls())

library(readr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(nlme)
library(emmeans)
library(lme4)
library(sjPlot)

setwd("C:/Users/barrehan/GitHub/projects/cwa.habitat.selection.dvm")
all.array.dat<-read.csv("data/modif.data/logger.array/epa.osu.logger.array.cleaned.csv")

osu<-all.array.dat[all.array.dat$collected.by == "osu",]

osu$location <- as.factor(osu$location)

mod1 <- lmer(dissolved.oxygen~temperature +(1|location), data = osu)
summary(mod1)

tab_model(mod1, show.re.var = T,
          pred.labels = c("(Intercept)", "Temperature °C"),
          dv.labels = "Linear relationship between temperature and dissolved oxgyen, Blue Ruin and Norwood")

mod2 <- lmer(dissolved.oxygen~temperature +(1|location), data = all.array.dat)
summary(mod2)

tab_model(mod2, show.re.var = T,
          pred.labels = c("(Intercept)", "Temperature °C"),
          dv.labels = "Linear relationship between temperature and dissolved oxgyen, all CWA")

effects.temp<-effects::effect(term = "temperature", mod = mod2)
x_temp <-as.data.frame(effects.temp)

ggplot()+
  geom_point(data = all.array.dat, aes(x =temperature, y = dissolved.oxygen), colour = "lightgrey", alpha = 0.5)+
  geom_line(data =x_temp, aes(temperature, y =fit), colour = "#972D15", linewidth = 1)+
  geom_ribbon(data = x_temp, aes(x = temperature, ymin = lower, ymax = upper), alpha = 0.3, fill =  "#E1BD6D")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 18, family = "serif"))+
  #scale_y_reverse()+
  xlab("Temperature (\u00B0C)")+
  ylab("Dissolved oxygen (mg/L)")

data.mod <-all.array.dat[!(all.array.dat$logger.site == "north.harrisburg.site.b" & all.array.dat$sensor.depth == "2.4"),]

mod3 <- lmer(dissolved.oxygen~temperature +(1|location), data = data.mod)
summary(mod2)

tab_model(mod3, show.re.var = T,
          pred.labels = c("(Intercept)", "Temperature °C"),
          dv.labels = "Linear relationship between temperature and dissolved oxgyen, all CWA, NHB bottom logger removed")

effects.temp<-effects::effect(term = "temperature", mod = mod3)
x_temp <-as.data.frame(effects.temp)

ggplot()+
  geom_point(data = data.mod, aes(x =temperature, y = dissolved.oxygen), colour = "lightgrey", alpha = 0.5)+
  geom_line(data =x_temp, aes(temperature, y =fit), colour = "#972D15", linewidth = 1)+
  geom_ribbon(data = x_temp, aes(x = temperature, ymin = lower, ymax = upper), alpha = 0.3, fill =  "#E1BD6D")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 18, family = "serif"))+
  #scale_y_reverse()+
  xlab("Temperature (\u00B0C)")+
  ylab("Dissolved oxygen (mg/L)")

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
library(ggpubr)

setwd("C:/Users/barrehan/GitHub/projects/cwa.habitat.selection.dvm")
all.array.dat<-read.csv("data/modif.data/logger.array/epa.osu.logger.array.cleaned.csv")
all.array.dat$date.time <-mdy_hm(all.array.dat$date.time)
all.array.dat$hour <-as.numeric(hour(all.array.dat$date.time))


osu<-all.array.dat[all.array.dat$collected.by == "osu",]
osu$location <- as.factor(osu$location)


mod1 <- lmer(dissolved.oxygen~temperature +(1|location), data = osu)
summary(mod1)

tab_model(mod1, show.re.var = T,
          pred.labels = c("(Intercept)", "Temperature °C"),
          dv.labels = "Linear relationship between temperature and dissolved oxgyen, Blue Ruin and Norwood")

effects.temp<-effects::effect(term = "temperature", mod = mod1)
x_temp <-as.data.frame(effects.temp)

p<-ggplot()+
  geom_point(data = osu, aes(x =temperature, y = dissolved.oxygen), colour = "lightgrey", alpha = 0.3)+
  geom_line(data =x_temp, aes(temperature, y =fit), colour = "#5E3B49", linewidth = 1)+
  geom_ribbon(data = x_temp, aes(x = temperature, ymin = lower, ymax = upper), alpha = 0.3, fill =  "#BA817D")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 15, family = "serif"))+
  #scale_y_reverse()+
  xlab("Temperature (\u00B0C)")+
  ylab("Dissolved oxygen (mg/L)")

#ggsave(p, filename = paste("results/figures/logger.array/temp.do.mixed.effects.br.nor.png"), width = 12, height = 8, units = "cm")

mod2 <- lmer(dissolved.oxygen~temperature +(1|location), data = all.array.dat)
summary(mod2)

tab_model(mod2, show.re.var = T,
          pred.labels = c("(Intercept)", "Temperature °C"),
          dv.labels = "Linear relationship between temperature and dissolved oxgyen, all CWA")

effects.temp<-effects::effect(term = "temperature", mod = mod2)
x_temp2 <-as.data.frame(effects.temp)

ggplot()+
  geom_point(data = all.array.dat, aes(x =temperature, y = dissolved.oxygen), colour = "lightgrey", alpha = 0.3)+
  geom_line(data =x_temp, aes(temperature, y =fit), colour = "#212E52", linewidth = 1)+
  geom_ribbon(data = x_temp2, aes(x = temperature, ymin = lower, ymax = upper), alpha = 0.3, fill =  "#8087AA")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 15, family = "serif"))+
  #scale_y_reverse()+
  xlab("Temperature (\u00B0C)")+
  ylab("Dissolved oxygen (mg/L)")

data.mod <-all.array.dat[!(all.array.dat$logger.site == "north.harrisburg.site.b" & all.array.dat$sensor.depth == "2.4"),]

mod3 <- lmer(dissolved.oxygen~temperature +(1|location), data = data.mod)
summary(mod3)

tab_model(mod3, show.re.var = T,
          pred.labels = c("(Intercept)", "Temperature °C"),
          dv.labels = "Linear relationship between temperature and dissolved oxgyen, all CWA, NHB bottom logger removed")

effects.temp3<-effects::effect(term = "temperature", mod = mod3)
x_temp3 <-as.data.frame(effects.temp3)

q<-ggplot()+
  geom_point(data = data.mod, aes(x =temperature, y = dissolved.oxygen), colour = "lightgrey", alpha = 0.3)+
  geom_line(data =x_temp3, aes(temperature, y =fit), colour = "#5E3B49", linewidth = 1)+
  geom_ribbon(data = x_temp, aes(x = temperature, ymin = lower, ymax = upper), alpha = 0.3, fill =  "#BA817D")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 15, family = "serif"))+
  #scale_y_reverse()+
  xlab("Temperature (\u00B0C)")+
  ylab("Dissolved oxygen (mg/L)")

ggplot()+
  #geom_point(data = osu, aes(x =temperature, y = dissolved.oxygen), colour = "lightgrey", alpha = 0.3)+
  geom_line(data =x_temp, aes(temperature, y =fit), colour = "#5E3B49", linewidth = 1)+
  #geom_ribbon(data = x_temp, aes(x = temperature, ymin = lower, ymax = upper), alpha = 0.3, fill =  "#BA817D")+
  #geom_point(data = data.mod, aes(x =temperature, y = dissolved.oxygen), colour = "lightgrey", alpha = 0.3)+
  geom_line(data =x_temp2, aes(temperature, y =fit), colour = "#5E3B49", linewidth = 1)+
  #geom_ribbon(data = x_temp2, aes(x = temperature, ymin = lower, ymax = upper), alpha = 0.3, fill =  "#BA817D")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 15, family = "serif"))+
  #scale_y_reverse()+
  xlab("Temperature (\u00B0C)")+
  ylab("Dissolved oxygen (mg/L)")

  

#ggsave(q, filename = paste("results/figures/logger.array/temp.do.mixed.effects.all.alc.png"), width = 12, height = 8, units = "cm")



# Pull OSU data into 2 dfs, 2hr high and 2hr low DO -----------------------
#6am-6pm

osu.lowdo<-osu[osu$hour >= 6 & osu$hour < 8,]
osu.highdo <-osu[osu$hour >=18 & osu$hour < 20,]

mod.lowdo <- lmer(dissolved.oxygen~temperature +(1|location), data = osu.lowdo)
summary(mod.lowdo)

tab_model(mod.lowdo, show.re.var = T,
          pred.labels = c("(Intercept)", "Temperature °C"),
          dv.labels = "Linear relationship between temperature and dissolved oxgyen, Blue Ruin and Norwood 6am-8am (low DO)")

effects.temp<-effects::effect(term = "temperature", mod = mod.lowdo)
x_temp <-as.data.frame(effects.temp)

a<-ggplot()+
  geom_point(data = osu.lowdo, aes(x =temperature, y = dissolved.oxygen), colour = "lightgrey", alpha = 0.3)+
  geom_line(data =x_temp, aes(temperature, y =fit), colour = "#5E3B49", linewidth = 1)+
  geom_ribbon(data = x_temp, aes(x = temperature, ymin = lower, ymax = upper), alpha = 0.3, fill =  "#BA817D")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 15, family = "serif"))+
  #scale_y_reverse()+
  xlab("Temperature (\u00B0C)")+
  ylab("Dissolved oxygen (mg/L)")


mod.highdo <- lmer(dissolved.oxygen~temperature +(1|location), data = osu.highdo)
summary(mod.highdo)

tab_model(mod.highdo, show.re.var = T,
          pred.labels = c("(Intercept)", "Temperature °C"),
          dv.labels = "Linear relationship between temperature and dissolved oxgyen, Blue Ruin and Norwood 6pm-8pm (high DO)")

effects.temp<-effects::effect(term = "temperature", mod = mod.highdo)
x_temp <-as.data.frame(effects.temp)

b<-ggplot()+
  geom_point(data = osu.highdo, aes(x =temperature, y = dissolved.oxygen), colour = "lightgrey", alpha = 0.3)+
  geom_line(data =x_temp, aes(temperature, y =fit), colour = "#5E3B49", linewidth = 1)+
  geom_ribbon(data = x_temp, aes(x = temperature, ymin = lower, ymax = upper), alpha = 0.3, fill =  "#BA817D")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 15, family = "serif"))+
  #scale_y_reverse()+
  xlab("Temperature (\u00B0C)")+
  ylab("Dissolved oxygen (mg/L)")

c<- ggarrange(a,
          b,
          ncol = 2)

#ggsave(c, filename = paste("results/figures/logger.array/temp.do.mixed.effects.br.nor.2hr.png"), width = 20, height = 10, units = "cm")


# Pull all logger data into 2 dfs, 2hr high and 2hr low DO -----------------------
#6am-6pm

array.lowdo<-data.mod[data.mod$hour >= 6 & data.mod$hour < 8,]
array.highdo <-data.mod[data.mod$hour >=18 & data.mod$hour < 20,]

mod.lowdo <- lmer(dissolved.oxygen~temperature +(1|location), data = array.lowdo)
summary(mod.lowdo)

tab_model(mod.lowdo, show.re.var = T,
          pred.labels = c("(Intercept)", "Temperature °C"),
          dv.labels = "Linear relationship between temperature and dissolved oxgyen, all arrays 6am-8am (low DO)")

effects.temp<-effects::effect(term = "temperature", mod = mod.lowdo)
x_temp <-as.data.frame(effects.temp)

d<-ggplot()+
  geom_point(data = array.lowdo, aes(x =temperature, y = dissolved.oxygen), colour = "lightgrey", alpha = 0.3)+
  geom_line(data =x_temp, aes(temperature, y =fit), colour = "#5E3B49", linewidth = 1)+
  geom_ribbon(data = x_temp, aes(x = temperature, ymin = lower, ymax = upper), alpha = 0.3, fill =  "#BA817D")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 15, family = "serif"))+
  #scale_y_reverse()+
  xlab("Temperature (\u00B0C)")+
  ylab("Dissolved oxygen (mg/L)")+
  scale_y_continuous(breaks = seq(0,8,2))+
  scale_x_continuous(breaks = seq (8, 22, 2))


mod.highdo <- lmer(dissolved.oxygen~temperature +(1|location), data = array.highdo)
summary(mod.highdo)

tab_model(mod.highdo, show.re.var = T,
          pred.labels = c("(Intercept)", "Temperature °C"),
          dv.labels = "Linear relationship between temperature and dissolved oxgyen, all arrays 6pm-8pm (high DO)")

effects.temp2<-effects::effect(term = "temperature", mod = mod.highdo)
x_temp2 <-as.data.frame(effects.temp2)

e<-ggplot()+
  geom_point(data = array.highdo, aes(x =temperature, y = dissolved.oxygen), colour = "lightgrey", alpha = 0.3)+
  geom_line(data =x_temp2, aes(temperature, y =fit), colour = "#5E3B49", linewidth = 1)+
  geom_ribbon(data = x_temp, aes(x = temperature, ymin = lower, ymax = upper), alpha = 0.3, fill =  "#BA817D")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 15, family = "serif"))+
  #scale_y_reverse()+
  xlab("Temperature (\u00B0C)")+
  ylab("Dissolved oxygen (mg/L)")

f<- ggarrange(d,
              e,
              ncol = 2)

j<- ggplot()+
  #geom_point(data = array.lowdo, aes(x =temperature, y = dissolved.oxygen), colour = "lightgrey", alpha = 0.3)+
  geom_line(data =x_temp, aes(temperature, y =fit), colour = "#289A84", linewidth = 1)+
  geom_ribbon(data = x_temp, aes(x = temperature, ymin = lower, ymax = upper), alpha = 0.3, fill =  "#289A84")+
  #geom_point(data = array.highdo, aes(x =temperature, y = dissolved.oxygen), colour = "lightgrey", alpha = 0.3)+
  geom_line(data =x_temp2, aes(temperature, y =fit), colour = "#8FF7BD", linewidth = 1)+
  geom_ribbon(data = x_temp2, aes(x = temperature, ymin = lower, ymax = upper), alpha = 0.3, fill = "#8FF7BD")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 15, family = "serif"))+
  #scale_y_reverse()+
  xlab("Temperature (\u00B0C)")+
  ylab("Dissolved oxygen (mg/L)")+
  xlim(10,25)+
  ylim(0,15)


#ggsave(f, filename = paste("results/figures/logger.array/temp.do.mixed.effects.all.array.2hr.png"), width = 20, height = 10, units = "cm")

br.netpen<-osu[osu$logger.site == "site.4.netpen",]
br.lowdo<-br.netpen[br.netpen$hour >= 5 & br.netpen$hour < 7,]

br.mod.lowdo <- lm(dissolved.oxygen~temperature, data = br.lowdo)
summary(br.mod.lowdo)

tab_model(br.mod.lowdo, show.re.var = T,
          pred.labels = c("(Intercept)", "Temperature °C"),
          dv.labels = "Linear relationship between temperature and dissolved oxgyen, br netpen 6am-8am (low DO)")

br.low.effects.temp<-effects::effect(term = "temperature", mod = br.mod.lowdo)
br.low.x_temp <-as.data.frame(br.low.effects.temp)

br.highdo <-br.netpen[br.netpen$hour >=17 & br.netpen$hour < 19,]

br.mod.highdo <- lm(dissolved.oxygen~temperature, data = br.highdo)
summary(br.mod.highdo)

tab_model(br.mod.highdo, show.re.var = T,
          pred.labels = c("(Intercept)", "Temperature °C"),
          dv.labels = "Linear relationship between temperature and dissolved oxgyen, br netpen 6pm-8pm (high DO)")

br.high.effects.temp<-effects::effect(term = "temperature", mod = br.mod.highdo)
br.high.x_temp <-as.data.frame(br.high.effects.temp)

a <- ggplot()+
  #geom_point(data = br.netpen, aes(x =temperature, y = dissolved.oxygen), colour = "lightgrey", alpha = 0.2)+
  geom_line(data =br.low.x_temp, aes(temperature, y =fit), colour = "#289A84", linewidth = 1)+
  geom_ribbon(data = br.low.x_temp, aes(x = temperature, ymin = lower, ymax = upper), alpha = 0.4, fill =  "#289A84")+
  geom_line(data = br.high.x_temp, aes(x= temperature, y = fit), colour = "#8FF7BD", linewidth = 1)+
  geom_ribbon(data = br.high.x_temp, aes(x = temperature, ymin = lower, ymax= upper), alpha = 0.4, fill = "#8FF7BD")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 15, family = "serif"))+
  #scale_y_reverse()+
  xlab("Temperature (\u00B0C)")+
  ylab("Dissolved oxygen (mg/L)")+
  xlim(10,25)+
  ylim(0,15)


nor.netpen<-osu[osu$location == "norwood",]
nor.lowdo<-nor.netpen[nor.netpen$hour >= 6 & nor.netpen$hour < 8,]

nor.mod.lowdo <- lm(dissolved.oxygen~temperature, data = nor.lowdo)
summary(nor.mod.lowdo)

tab_model(nor.mod.lowdo, show.re.var = T,
          pred.labels = c("(Intercept)", "Temperature °C"),
          dv.labels = "Linear relationship between temperature and dissolved oxgyen, nor netpen 6am-8am (low DO)")

nor.low.effects.temp<-effects::effect(term = "temperature", mod = nor.mod.lowdo)
nor.low.x_temp <-as.data.frame(nor.low.effects.temp)

nor.highdo <-nor.netpen[nor.netpen$hour >=18 & nor.netpen$hour < 20,]

nor.mod.highdo <- lm(dissolved.oxygen~temperature, data = nor.highdo)
summary(nor.mod.highdo)

tab_model(nor.mod.highdo, show.re.var = T,
          pred.labels = c("(Intercept)", "Temperature °C"),
          dv.labels = "Linear relationship between temperature and dissolved oxgyen, nor netpen 6pm-8pm (high DO)")

nor.high.effects.temp<-effects::effect(term = "temperature", mod = nor.mod.highdo)
nor.high.x_temp <-as.data.frame(nor.high.effects.temp)

b <- ggplot()+
  #geom_point(data = nor.netpen, aes(x =temperature, y = dissolved.oxygen), colour = "lightgrey", alpha = 0.2)+
  geom_line(data =nor.low.x_temp, aes(temperature, y =fit), colour = "#289A84", linewidth = 1)+
  geom_ribbon(data = nor.low.x_temp, aes(x = temperature, ymin = lower, ymax = upper), alpha = 0.4, fill =  "#289A84")+
  geom_line(data = nor.high.x_temp, aes(x= temperature, y = fit), colour = "#8FF7BD", linewidth = 1)+
  geom_ribbon(data = nor.high.x_temp, aes(x = temperature, ymin = lower, ymax= upper), alpha = 0.4, fill = "#8FF7BD")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 15, family = "serif"))+
  #scale_y_reverse()+
  xlab("Temperature (\u00B0C)")+
  ylab("Dissolved oxygen (mg/L)")+
  xlim(10,25)+
  ylim(0,15)

c<- ggarrange(a,
          b,
          j,
          ncol = 2,
          nrow = 2,
          align = "hv")

ggsave(c, filename = paste("results/figures/logger.array/temp.do.mixed.effects.3.panel.2hr.png"), width = 20, height = 18, units = "cm")


array.lowdo$logger.site<-as.factor(array.lowdo$logger.site)

ggplot(data = array.lowdo, aes(x = logger.site, y = dissolved.oxygen, colour = logger.site))+
  geom_boxplot()+
  facet_wrap(~location)+
  scale_y_continuous(limits=c(0,10), breaks = seq(0, 10, 1))
  
ggplot(data = array.highdo, aes(x = logger.site, y = dissolved.oxygen, colour = logger.site))+
  geom_boxplot()+
  facet_wrap(~location)+
  scale_y_continuous(limits=c(0,10), breaks = seq(0, 10, 1))


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
names(dater)[3]<-"thermocline.depth"
names(dater)[1] <- "profile"

dater<-na.omit(dater)


mod1<-lme(fish.depth ~thermocline.depth, data = dater,
          random = ~1|profile)
summary(mod1)

mod1.1<-lmer(fish.depth~thermocline.depth +(1|profile), data = dater)
summary(mod1.1)

mod2<-lm(fish.depth~thermocline.depth, data = dater)
summary(mod2)

anova(mod1, mod2)

#model 1 with random effect is better

confint(mod1.1)
fixef(mod1.1)
ranef(mod1.1)

dater$Mod1resid <-resid(mod1, type = "normalized")
dater$Mod1fitted <-fitted(mod1)

dater$Mod2resid <-resid(mod2)
dater$Mod2fitted <-fitted(mod2)

ggplot(dater, aes(Mod1fitted, Mod1resid))+
  geom_point()

ggplot(dater, aes(Mod2fitted, Mod2resid))+
  geom_point()


## to get coefficients of fixed effects in lme use fixef(), and intervals() to get CI of random and fixed effects 

fixef(mod1)

intervals(mod1)

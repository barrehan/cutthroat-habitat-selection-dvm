rm(list=ls())
library(survival)
library(ggplot2)
library(ggforce)

setwd("C:/Users/barrehan/GitHub/projects/cwa.habitat.selection.dvm")
nor.ib<- read.csv("data/modif.data/hab.select.mod/norwood.ibutton.data/norwood.ibutton.pooled.csv")
nor.ib <- nor.ib[nor.ib$case ==1,]
br.ib<- read.csv("data/modif.data/hab.select.mod/br.ibutton.data/br.ibutton.pooled.csv")
br.ib <- br.ib[br.ib$case == 1,]

nor.mean <- mean(nor.ib$temperature)
nor.sd <- sd(nor.ib$temperature)
br.mean <- mean(br.ib$temperature)
br.sd <- sd(br.ib$temperature)

nor.mean.do <- mean(nor.ib$dissolved.oxygen)
nor.sd.do <- sd(nor.ib$dissolved.oxygen)
br.mean.do <- mean(br.ib$dissolved.oxygen)
br.sd.do <- sd(br.ib$dissolved.oxygen)

nor.deg17 <- (17-nor.mean)/nor.sd
nor.deg15 <- (15-nor.mean)/nor.sd
nor.deg13 <- (13-nor.mean)/nor.sd

br.deg17 <-(17-br.mean)/br.sd
br.deg15 <-(15-br.mean)/br.sd
br.deg13 <-(13-br.mean)/br.sd

#DO of 2, 5, 8

nor.do2<- (2-nor.mean.do)/nor.sd.do
nor.do5<- (5-nor.mean.do)/nor.sd.do
nor.do8<- (8-nor.mean.do)/nor.sd.do

br.do2 <- (2-br.mean.do)/br.sd.do
br.do5 <- (5-br.mean.do)/br.sd.do
br.do8 <- (8-br.mean.do)/br.sd.do

## find average DO of bottom logger during 2-hour window, this
##is what we'll set line plot to
nor.hypo <-nor.ib[nor.ib$depth >= 1 & nor.ib$highlowDO.2hours == "high",]
mean(nor.hypo$dissolved.oxygen, na.rm = T)

nor.hypo <-nor.ib[nor.ib$depth >= 1 & nor.ib$highlowDO.2hours == "low",]
mean(nor.hypo$dissolved.oxygen, na.rm = T)

nor.do.high<- (8.4-nor.mean.do)/nor.sd.do
nor.do.low<- (1.3-nor.mean.do)/nor.sd.do

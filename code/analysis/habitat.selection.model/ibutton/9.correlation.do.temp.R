#'2022-11-09
# Check for correlation between DO and Temp -------------------------------

rm(list=ls())
setwd("C:/Users/barrehan/GitHub/projects/cwa.habitat.selection.dvm")
library(readr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)

#cor(day$standardized.do, day$standardized.temp, na.remove = T)
#'R value >.8 don't include both variables in the model

br.ib<- read.csv("data/modif.data/hab.select.mod/br.ibutton.data/br.ibutton.pooled.csv")

# Make time ID columns factors --------------------------------------------

br.ib$highlowDO.2hr <- as.factor(br.ib$highlowDO.2hr)
br.ib$dayID <-as.factor(br.ib$dayID)
br.ib$hourID <-as.factor(br.ib$hourID)
br.ib$quarterID <-as.factor(br.ib$quarterID)


# Data frames by 2-hour window --------------------------------------------
high.br <-br.ib[br.ib$highlowDO.2hr == "high",]
high.br <-na.omit(high.br)
low.br <-br.ib[br.ib$highlowDO.2hr == "low",]
low.br <- na.omit(low.br)

cor(high.br$standardized.do, high.br$standardized.temp)
cor(low.br$standardized.do, low.br$standardized.temp)

both.br <-br.ib[br.ib$highlowDO.2hr == "high" | br.ib$highlowDO.2hr == "low",]
both.br <- na.omit(both.br)

cor(both.br$standardized.do, both.br$standardized.temp)
# Data frames by day and night --------------------------------------------

day.br <- br.ib[br.ib$dayID == 0,]
night.br <-br.ib[br.ib$dayID == 1,]

cor(br.ib$standardized.do, br.ib$standardized.temp)
cor(day.br$standardized.do, day.br$standardized.temp)
cor(night.br$standardized.do, night.br$standardized.temp)


# Data frames by quarter --------------------------------------------------

quart1.br <- br.ib[br.ib$quarterID == 1,]
quart2.br <- br.ib[br.ib$quarterID == 2,]
quart3.br <- br.ib[br.ib$quarterID == 3,]
quart4.br <- br.ib[br.ib$quarterID == 4,]

cor(quart1.br$standardized.do, quart1.br$standardized.temp) #midnight - 6am
cor(quart2.br$standardized.do, quart2.br$standardized.temp) #6am - noon
cor(quart3.br$standardized.do, quart3.br$standardized.temp) #noon - 6pm
cor(quart4.br$standardized.do, quart4.br$standardized.temp) #6pm - midnight
#quart3 and quart 4 have correlation value > .8

nor.ib<-read.csv("data/modif.data/hab.select.mod/norwood.ibutton.data/norwood.ibutton.pooled.csv")  

nor.ib$dayID <-as.factor(nor.ib$dayID)
nor.ib$hourID <-as.factor(nor.ib$hourID)
nor.ib$quarterID <-as.factor(nor.ib$quarterID)

# Data frames by day and night --------------------------------------------

day.nor <- nor.ib[nor.ib$dayID == 0,]
night.nor <-nor.ib[nor.ib$dayID == 1,]

cor(nor.ib$standardized.do, nor.ib$standardized.temp)
cor(day.nor$standardized.do, day.nor$standardized.temp)
cor(night.nor$standardized.do, night.nor$standardized.temp)

# Data frames by quarter --------------------------------------------------

quart1.nor <- nor.ib[nor.ib$quarterID == 1,]
quart2.nor <- nor.ib[nor.ib$quarterID == 2,]
quart3.nor <- nor.ib[nor.ib$quarterID == 3,]
quart4.nor <- nor.ib[nor.ib$quarterID == 4,]

cor(quart1.nor$standardized.do, quart1.nor$standardized.temp) #midnight - 6am
cor(quart2.nor$standardized.do, quart2.nor$standardized.temp) #6am - noon
cor(quart3.nor$standardized.do, quart3.nor$standardized.temp) #noon - 6pm
cor(quart4.nor$standardized.do, quart4.nor$standardized.temp) #6pm - midnight
#all are correlated (>.8) except quarter 4 (6pm-midnight)

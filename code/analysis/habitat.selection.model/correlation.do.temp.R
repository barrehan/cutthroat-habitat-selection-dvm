#'2022-11-09
# Check for correlation between DO and Temp -------------------------------

rm(list=ls())
setwd("C:/Users/barrehan/GitHub/projects/cwa.habitat.selection")
library(readr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)

#cor(day$standardized.do, day$standardized.temp, na.remove = T)
#'R value >.8 don't include both variables in the model

br.ib<- read.csv("data/modif.data/hab.select.mod/br.ibutton.data/br.ibutton.pooled.csv")

# Make time ID columns factors --------------------------------------------

br.ib$dayID <-as.factor(br.ib$dayID)
br.ib$hourID <-as.factor(br.ib$hourID)
br.ib$quarterID <-as.factor(br.ib$quarterID)

# Data frames by day and night --------------------------------------------

day.br <- br.ib[br.ib$dayID == 0,]
night.br <-br.ib[br.ib$dayID == 1,]

cor(br.ib$standardized.do, br.ib$standardized.temp)
cor(day.br$standardized.do, day.br$standardized.temp)
cor(night.br$standardized.do, night.br$standardized.temp)

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


#'10-19-2022
#'pool ibuttons for br, create stratum.ID for individual and for pooled data

setwd("C:/Users/barrehan/GitHub/projects/cwa.habitat.selection")

library(readr)
library(dplyr)
library(lubridate)
library(survival) #'clogit function

rm(list=ls())

# Bring in all dfs for blue ruin ibutton fish ------------------------------

f01 <- read.csv("data/modif.data/ibutton/hab.select.mod/button.01.hab.select.mod.csv")
f03 <- read.csv("data/modif.data/ibutton/hab.select.mod/button.03.hab.select.mod.csv")
f05 <- read.csv("data/modif.data/ibutton/hab.select.mod/button.05.hab.select.mod.csv")
f07 <- read.csv("data/modif.data/ibutton/hab.select.mod/button.07.hab.select.mod.csv")
f08 <- read.csv("data/modif.data/ibutton/hab.select.mod/button.08.hab.select.mod.csv")
f10 <- read.csv("data/modif.data/ibutton/hab.select.mod/button.10.hab.select.mod.csv")
f11 <- read.csv("data/modif.data/ibutton/hab.select.mod/button.11.hab.select.mod.csv")
f15 <- read.csv("data/modif.data/ibutton/hab.select.mod/button.15.hab.select.mod.csv")
f17 <- read.csv("data/modif.data/ibutton/hab.select.mod/button.17.hab.select.mod.csv")
f21 <- read.csv("data/modif.data/ibutton/hab.select.mod/button.21.hab.select.mod.csv")
f22 <- read.csv("data/modif.data/ibutton/hab.select.mod/button.22.hab.select.mod.csv")
f23 <- read.csv("data/modif.data/ibutton/hab.select.mod/button.23.hab.select.mod.csv")
f30 <- read.csv("data/modif.data/ibutton/hab.select.mod/button.30.hab.select.mod.csv")
f31 <- read.csv("data/modif.data/ibutton/hab.select.mod/button.31.hab.select.mod.csv")

# Pool all dfs into one ---------------------------------------------------

br.fish <- do.call("rbind", list(f01, f03, f05, f07, f08, f10, f15, f17, f21, f22, f23, f30, f31))
br.fish$date.time <- ymd_hms(br.fish$date.time) 

# Remove stratID and timeID and create as pooled vectors ------------------
br.fish <-subset(br.fish, select = -c(7,12))

#'order by id and then by date.time
br.fish <- br.fish[
  order(br.fish[,8], br.fish[,1] ),
]

br.fish <- br.fish %>% 
  mutate(stratID = group_indices(.,ibutton.id, date.time))

br.fish <- transform(br.fish,                                 
                       timeID = as.numeric(factor(time)))
                                            
# Separate pooled df into day and night (6am, 6pm) ------------------------

data.night.clogit <- br.fish[br.fish$timeID < 73 | br.fish$timeID >=217,]

data.day.clogit <- br.fish[br.fish$timeID < 217 & br.fish$timeID >=73 ,]


# Make time and timeID factors --------------------------------------------

data.night.clogit$time <-as.factor(data.night.clogit$time)
data.day.clogit$time <-as.factor(data.day.clogit$time)

data.night.clogit$timeID <-as.factor(data.night.clogit$timeID)
data.day.clogit$timeID <-as.factor(data.day.clogit$timeID)

# Fit the model -----------------------------------------------------------

night.clogit<-clogit(formula = case ~
                       standardized.do+
                       standardized.temp+
                       standardized.do:time+
                       standardized.temp:time+
                       standardized.do:standardized.temp+
                       standardized.do:standardized.temp:time+
                       strata(stratID),
                       data=data.night.clogit)
summary(night.clogit) 

day.clogit<-clogit(formula = case ~
                       standardized.do+
                       standardized.temp+
                       standardized.do:time+
                       standardized.temp:time+
                       standardized.do:standardized.temp+
                       standardized.do:standardized.temp:time+
                       strata(stratID),
                     data=data.day.clogit)
summary(night.clogit) 



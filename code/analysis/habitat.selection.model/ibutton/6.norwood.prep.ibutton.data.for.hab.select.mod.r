#'2022-11-07
# Use uniform distribution estimates of do and temp to create ibutton.csvs -----
rm(list=ls())
setwd("C:/Users/barrehan/GitHub/projects/cwa.habitat.selection")
library(readr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)

array<- read.csv("data/modif.data/hab.select.mod/norwood.unif.do.temp.csv")
array$date.time <-ymd_hms(array$date.time)
array <- array %>% force_tz(array$date.time, tzone = "America/Los_Angeles")

#'bring in ibutton data from norwood netpen
#'02, 04, 06, 13, 14, 16, 18

ib <- read.csv("data/modif.data/ibutton/do.depth.interpolation/norwood.ibutton.18.depth.do.interpolation.csv")
ib$date.time <- ymd_hms(ib$date.time)
ib <- ib %>% force_tz(ib$date.time, tzone = "America/Los_Angeles")
ib <- subset(ib, select = c(2,3,5,6))
ib <- rename(ib, temperature = ibutton.temp)
ib <- rename(ib, depth = fish.depth)
ib$case <- 1

#find matching date.time between two df
df <- array %>% filter(array$date.time %in% ib$date.time)
merge <-rbind(ib,df)

##############################################
#add ibutton id and netpen id
merge$ibutton.id <- 18
merge$netpen <- "norwood.netpen"
##############################################



new.dater<- merge %>%arrange(date.time)
new.dater$standardized.do <- scale(new.dater$dissolved.oxygen)
new.dater$standardized.temp <-scale(new.dater$temperature)

#'stratum column
new.dater <- transform(new.dater,                                 # Create ID by group
                       stratID = as.numeric(factor(date.time)))

write.csv(new.dater, file = "data/modif.data/hab.select.mod/norwood.ibutton.data/button.18.hab.select.mod.csv", row.names = F)

rm(list=ls())

library(ggplot2)
library(tidyverse)
library(lme4)
library(viridis)
library(dplyr)
library(tidyverse)
library(lubridate)

setwd("C:/Users/barrehan/GitHub/projects/cwa.habitat.selection")

array <- read.csv("data/modif.data/logger.array/br.netpen.array.depths.temps.thermo.calc.csv")
array$date.time <-ymd_hms(array $date.time)
unif.dist.temp <- unif.dist.temp%>% force_tz(unif.dist.temp$date.time, tzone = "America/Los_Angeles")

times = unique(unif.dist.temp$date.time) # Storing the unique "times"

#find least slope, find depth at slope midpoint

for(i in 1:length(unif.dist.temp)){
  t1 <- unif.dist.temp %>% filter(date.time == times[i]) ## Filtering out the logger data for this "Time"
  therm <- min(t1$slope)
}
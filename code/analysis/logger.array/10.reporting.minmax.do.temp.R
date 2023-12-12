library(readr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(RColorBrewer)
library(viridis)
library(scales)

setwd("C:/Users/barrehan/GitHub/projects/cwa.habitat.selection.dvm")

nor <- read.csv("data/raw.data/logger.array/norwood.mouth.temp.do.csv")

nor$date.time<- mdy_hm(nor$date.time) 
nor <- nor %>% force_tz(nor$date.time, tzone = "America/Los_Angeles")

nor <- nor[nor$date.time >="2021-07-30 00:00:00" & nor$date.time < "2021-08-05 12:00:00",]
nor$date <-as.Date(nor$date.time)

nor.contrasts<- nor[nor$sensor.depth == 1.45 | nor$sensor.depth == 0.25,]

nor.temp <- nor.contrasts %>%
  group_by(date, sensor.depth)%>%
  slice(which.min(temperature), which.max(temperature))

nor.do <- nor.constrasts %>%
  group_by(date, sensor.depth)%>%
  slice(which.min(dissolved.oxygen), which.max(dissolved.oxygen))

br <-read.csv("data/modif.data/logger.array/blue.ruin.netpen.array.do.temp.csv")
br$date.time<- mdy_hm(br$date.time) 
br <- br %>% force_tz(br$date.time, tzone = "America/Los_Angeles")
br <- br[br$date.time>="2021-07-30 00:00:00" & br$date.time < "2021-08-05 12:00:00",]

br$date <-as.Date(br$date.time)

br.contrasts<- br[br$sensor.depth == 1.35 | br$sensor.depth == 0.25,]

br.temp <- br.contrasts %>%
  group_by(date, sensor.depth)%>%
  slice(which.min(temperature), which.max(temperature))

br.do <- br.contrasts %>%
  group_by(date, sensor.depth)%>%
  slice(which.min(dissolved.oxygen), which.max(dissolved.oxygen))


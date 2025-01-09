# check out average maximum temperature selected by fish in br during the timespan
rm(list=ls())
library(readr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)

setwd("C:/Users/barrehan/Documents/GitHub/cwa.habitat.selection.dvm")

ib <- read.csv("data/modif.data/ibutton/all.ib.interp.depth.csv")
ib$date<- mdy(ib$date)
ib$date.time <-mdy_hm(ib$date.time)

# Filter the data
br <- ib %>% 
  filter(site == "blue.ruin", case == 1) %>% 
  mutate(ibutton.id = as.factor(ibutton.id))

br<- br[br$date.time >="2021-07-30 00:00:00" & br$date.time < "2021-08-07 00:00:00",]
t<-max(br$temperature)

# Calculate the average maximum temperature for the selected tag IDs
avg_max_temp <- br %>%
  group_by(ibutton.id) %>%
  summarize(max_temp = max(temperature, na.rm = TRUE)) %>%
  summarize(avg_max_temp = mean(max_temp, na.rm = TRUE)) %>%
  pull(avg_max_temp)

# Calculate the daily maximum temperature for each tag ID
daily_max_temp <- br %>%
  group_by(date, ibutton.id) %>%
  summarize(daily_max = max(temperature, na.rm = TRUE), .groups = "drop")

# Calculate the average of daily maximum temperatures across all tag IDs and dates
avg_daily_max_temp <- daily_max_temp %>%
  summarize(avg_max_temp = mean(daily_max, na.rm = TRUE)) %>%
  pull(avg_max_temp)

# now calculate average maximum temperature at br array for same timeframe
br.array <- read.csv("data/modif.data/logger.array/blue.ruin.netpen.array.do.temp.csv")
br.array$date.time<- mdy_hm(br.array$date.time)
br.array<- br.array[br.array$date.time >="2021-07-30 00:00:00" & br.array$date.time < "2021-08-07 00:00:00",]
br.array$date <-as.Date(br.array$date.time)

# maximum temperature during timeframe
t.max <-max(br.array$temperature, na.rm = T)

# Calculate daily maximum temperatures
daily_max_temp_array <- br.array %>%
  group_by(date) %>%
  summarize(daily_max = max(temperature, na.rm = TRUE), .groups = "drop")

# Calculate the average of daily maximum temperatures
avg_max_daily_temp_array <- daily_max_temp_array %>%
  summarize(avg_max_temp = mean(daily_max, na.rm = TRUE)) %>%
  pull(avg_max_temp)

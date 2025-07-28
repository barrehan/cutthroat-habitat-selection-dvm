library(readr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)

setwd("C:/Users/barrehan/Documents/GitHub/cwa.habitat.selection.dvm")

df <- read.csv("data/raw.data/ibutton/2021.fish.tag.deployment.0722.0723.csv")
df_subset <- df %>%
  select(location, mass, forklength, tag.type, tag.id)
ibutton_fish <- df %>%
  filter(tag.type == "ibutton")

summary_stats_m <- ibutton_fish %>%
  summarize(mean_mass = mean(mass, na.rm = TRUE),
            sd_mass = sd(mass, na.rm = TRUE))
print(summary_stats_m)

summary_stats_fl <- ibutton_fish %>%
  summarize(mean_fl = mean(forklength, na.rm = TRUE),
            sd_fl = sd(forklength, na.rm = TRUE))
print(summary_stats_fl)

summary_stats_by_location <- ibutton_fish %>%
  group_by(location) %>%
  summarize(mean_mass = mean(mass, na.rm = TRUE),
            sd_mass = sd(mass, na.rm = TRUE))

print(summary_stats_by_location)

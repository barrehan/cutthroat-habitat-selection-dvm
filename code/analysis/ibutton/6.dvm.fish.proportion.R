library(readr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(RColorBrewer)
library(viridis)
library(grid)
library(gridExtra)
library(data.table)
library(ggpubr)

setwd("C:/Users/barrehan/GitHub/projects/cwa.habitat.selection.dvm/data/modif.data/ibutton/do.depth.interpolation")

list_csv_files <- list.files(path = "/Users/barrehan/GitHub/projects/cwa.habitat.selection.dvm/data/modif.data/ibutton/do.depth.interpolation/")
df <- do.call(rbind, lapply(list_csv_files, function(x) read.csv(x, stringsAsFactors = FALSE)))

setwd("C:/Users/barrehan/GitHub/projects/cwa.habitat.selection.dvm")

df$date.time<- ymd_hms(df$date.time)
df$Hour <- hour(df$date.time)

df<-df[df$date.time >="2021-07-30 00:00:00" & df$date.time < "2021-08-05 12:00:00",]
df$date <-as.Date(df$date.time)

depths <-df %>%
  group_by(date, ibutton) %>%
  slice(which.min(fish.depth), which.max(fish.depth))

data_frame <- depths %>%
  group_by(date, ibutton)%>%
  mutate(diff=fish.depth-lag(
  fish.depth,default=first(fish.depth)))



rm(list=ls())

library(readr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)

setwd("C:/Users/barrehan/GitHub/projects/cwa.habitat.selection.dvm")

all.array.dat<-read.csv("data/raw.data/logger.array/all.arrays.osu.epa.csv")

#remove error temp readings (-888)

#remove data with only temperature (no DO)

#compare river loop temps to mainstem temps over time remembering that river loop got warm (remove data where not 
#2c colder than mainstem)
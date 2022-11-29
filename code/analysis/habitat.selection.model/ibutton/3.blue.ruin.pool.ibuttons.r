#'10-19-2022
#'pool ibuttons for br, create stratum.ID for individual and for pooled data
rm(list=ls())

setwd("C:/Users/barrehan/GitHub/projects/cwa.habitat.selection")

library(readr)
library(dplyr)
library(lubridate)

# Bring in all dfs for blue ruin ibutton fish ------------------------------

f01 <- read.csv("data/modif.data/hab.select.mod/br.ibutton.data/button.01.hab.select.mod.csv")
f03 <- read.csv("data/modif.data/hab.select.mod/br.ibutton.data/button.03.hab.select.mod.csv")
f05 <- read.csv("data/modif.data/hab.select.mod/br.ibutton.data/button.05.hab.select.mod.csv")
f07 <- read.csv("data/modif.data/hab.select.mod/br.ibutton.data/button.07.hab.select.mod.csv")
f08 <- read.csv("data/modif.data/hab.select.mod/br.ibutton.data/button.08.hab.select.mod.csv")
f10 <- read.csv("data/modif.data/hab.select.mod/br.ibutton.data/button.10.hab.select.mod.csv")
f11 <- read.csv("data/modif.data/hab.select.mod/br.ibutton.data/button.11.hab.select.mod.csv")
f15 <- read.csv("data/modif.data/hab.select.mod/br.ibutton.data/button.15.hab.select.mod.csv")
f17 <- read.csv("data/modif.data/hab.select.mod/br.ibutton.data/button.17.hab.select.mod.csv")
f21 <- read.csv("data/modif.data/hab.select.mod/br.ibutton.data/button.21.hab.select.mod.csv")
f22 <- read.csv("data/modif.data/hab.select.mod/br.ibutton.data/button.22.hab.select.mod.csv")
f23 <- read.csv("data/modif.data/hab.select.mod/br.ibutton.data/button.23.hab.select.mod.csv")
f30 <- read.csv("data/modif.data/hab.select.mod/br.ibutton.data/button.30.hab.select.mod.csv")
f31 <- read.csv("data/modif.data/hab.select.mod/br.ibutton.data/button.31.hab.select.mod.csv")

# Pool all dfs into one ---------------------------------------------------

br.fish <- do.call("rbind", list(f01, f03, f05, f07, f08, f10, f15, f17, f21, f22, f23, f30, f31))
br.fish$date.time <- ymd_hms(br.fish$date.time) 

# Remove stratID and timeID and create as pooled vectors ------------------
br.fish <-subset(br.fish, select = -c(7, 10))

#'order by fish.id and date.time
br.fish <- br.fish[
  order(br.fish[,6], br.fish[,1] ),
]

br.fish <- br.fish %>% 
  mutate(stratID = group_indices(.,ibutton.id, date.time))

br.fish$hourID <-hour(br.fish$date.time)

br.fish$dayID <- ifelse(br.fish$hourID <18 & br.fish$hourID >=6, 0, 1)

br.fish$quarterID<- ifelse(br.fish$hourID < 6, 1,
                           ifelse(br.fish$hourID < 12, 2,
                                  ifelse(br.fish$hourID <18, 3,
                                         4)))
                                            
# write pooled .csv -------------------------------------------------------
write.csv(br.fish, "data/modif.data/hab.select.mod/br.ibutton.data/br.ibutton.pooled.csv", row.names = F)




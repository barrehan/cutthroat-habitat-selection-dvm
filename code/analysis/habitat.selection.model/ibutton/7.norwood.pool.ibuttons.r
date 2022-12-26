#'2022-11-07
#'pool ibuttons for br, create stratum.ID for individual and for pooled data
rm(list=ls())

setwd("C:/Users/barrehan/GitHub/projects/cwa.habitat.selection")

library(readr)
library(dplyr)
library(lubridate)
library(TeachingDemos)


# Bring in all dfs for norwood ibutton fish -------------------------------
#'02, 04, 06, 13, 14, 16, 18
f02 <- read.csv("data/modif.data/hab.select.mod/norwood.ibutton.data/button.02.hab.select.mod.csv")
f04 <- read.csv("data/modif.data/hab.select.mod/norwood.ibutton.data/button.04.hab.select.mod.csv")
f06 <- read.csv("data/modif.data/hab.select.mod/norwood.ibutton.data/button.06.hab.select.mod.csv")
f13 <- read.csv("data/modif.data/hab.select.mod/norwood.ibutton.data/button.13.hab.select.mod.csv")
f14 <- read.csv("data/modif.data/hab.select.mod/norwood.ibutton.data/button.14.hab.select.mod.csv")
f16 <- read.csv("data/modif.data/hab.select.mod/norwood.ibutton.data/button.16.hab.select.mod.csv")
f18 <- read.csv("data/modif.data/hab.select.mod/norwood.ibutton.data/button.18.hab.select.mod.csv")

# Pool all dfs into one ---------------------------------------------------

nor.fish <- do.call("rbind", list(f02, f04, f06, f13, f14, f16, f18))
nor.fish$date.time <- ymd_hms(nor.fish$date.time) 

# Remove stratID and timeID and create as pooled vectors ------------------
nor.fish <-subset(nor.fish, select = -c(7, 10))

#'order by fish.id and date.time
nor.fish <- nor.fish[
  order(nor.fish[,6], nor.fish[,1] ),
]

nor.fish <- nor.fish %>% 
  mutate(stratID = group_indices(.,ibutton.id, date.time))

nor.fish$hourID <-hour(nor.fish$date.time)

nor.fish$dayID <- ifelse(nor.fish$hourID <18 & nor.fish$hourID >=6, 0, 1)

nor.fish$quarterID<- ifelse(nor.fish$hourID < 6, 1,
                           ifelse(nor.fish$hourID < 12, 2,
                                  ifelse(nor.fish$hourID <18, 3,
                                         4)))

# highest environmental DO between 4pm-8pm, lowest between ~4am-8am

nor.fish$highlowenvDO <-ifelse(15 %<% nor.fish$hourID %<% 20, "high",
                              ifelse(3 %<% nor.fish$hourID %<% 8, "low", "NA"))


# write pooled .csv -------------------------------------------------------
write.csv(nor.fish, "data/modif.data/hab.select.mod/norwood.ibutton.data/norwood.ibutton.pooled.csv", row.names = F)

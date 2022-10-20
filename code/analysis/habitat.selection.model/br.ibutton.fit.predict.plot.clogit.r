
#'10-20-2022
#'Fit conditional logistic regression to pooled blue ruin ibutton data

rm(list=ls())
library(survival)
library(ggplot2)

br.ib<- read.csv("data/modif.data/ibutton/hab.select.mod/br.ibutton.pooled.csv")
# Make time ID columns factors --------------------------------------------

br.ib$dayID <-as.factor(br.ib$dayID)
br.ib$hourID <-as.factor(br.ib$hourID)
br.ib$quarterID <-as.factor(br.ib$quarterID)
br.ib$timeID <-as.factor(br.ib$timeID)

# Logistic regression day versus night ------------------------------------

daynight.clogit<-clogit(formula = case ~
                       standardized.do+
                       standardized.temp+
                       standardized.do:dayID+
                       standardized.temp:dayID+
                       standardized.do:standardized.temp+
                       standardized.do:standardized.temp:dayID+
                       strata(stratID),
                     data=br.ib)
summary(daynight.clogit) 


# Selection for do/temp ---------------------------------------------------

# predictions

# make new dataframe with the exact variables in your clogit model, but with values for whatever you want predictions for
# here i am looking to see how selection for landcover 1 changes as a function of time since disturbance,
# for an elk population at high density (below i do the same for the low density population)
# you'll probably want to make DO or Temp vary instead of time (give them values spanning your observed dataset, and dont forget they may be standardized!!)
# it doesn't matter what value you put for strata but you need to put something
pred_vals_high<-data.frame(time=seq(0,34,1),
                           time2=seq(0,34,1)^2,
                           landcover=as.factor(1),
                           density=1,
                           water=0,
                           roads=0,
                           northness=0,
                           strata=1)

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
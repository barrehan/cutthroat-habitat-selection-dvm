#contour plots with GAM

rm(list=ls())
library(survival)
library(ggplot2)
library(plotly)
library(ggpubr)
library(htmlwidgets)
library(reticulate)
library(viridis)
library(ggforce)
library(dplyr)
library(lubridate)
library(sjPlot)
library(AICcmodavg)
library(mgcv)
library(tidyverse)

setwd("C:/Users/barrehan/GitHub/projects/cwa.habitat.selection.dvm")
nor.ib<- read.csv("data/modif.data/hab.select.mod/norwood.ibutton.data/norwood.ibutton.pooled.csv")
nor.ib$date.time <- ymd_hms(nor.ib$date.time)
nor.ib <- nor.ib %>% force_tz(nor.ib$date.time, tzone = "America/Los_Angeles")
nor.ib<-nor.ib[nor.ib$date.time >="2021-07-30 00:00:00" & nor.ib$date.time < "2021-08-06 00:00:00",]

# Data frames by high/low interval ----------------------------------------

high.int <- nor.ib[nor.ib$highlowDO.2hours == "high",]
high.int <- high.int[!is.na(high.int$highlowDO.2hours),]

low.int <- nor.ib[nor.ib$highlowDO.2hours == "low",]
low.int <- low.int[!is.na(low.int$highlowDO.2hours),]


# GAM Norwood high DO 2hr window ------------------------------------------


high.int$dumt <-rep(1,nrow(high.int))
gam.high<-gam(cbind(dumt,stratID) ~ s(standardized.temp) + s(standardized.do) + s(standardized.do,standardized.temp), #how to add interaction for GAM
             data = high.int,
             family=cox.ph, weights = case)
summary(gam.high)

pred.vals.high<- data.frame(standardized.do = -0.3603449,
                                   standardized.temp = seq(min(high.int$standardized.temp, na.rm = T),
                                                           max(high.int$standardized.temp, na.rm = T), 
                                                           0.1),
                                   stratID = 691)

preds.gam.high <- predict.gam(gam.high, newdata = pred.vals.high, type = 'link', se.fit = T) 
plot(x = pred.vals.high$standardized.temp, y = preds.gam.high$fit, type = 'l') #if want exp put exp in front of y



# GAM Norwood low DO 2hr window ------------------------------------------


low.int$dumt <-rep(1,nrow(low.int))
out.gam.low<-gam(cbind(dumt,stratID) ~ s(standardized.temp) + s(standardized.do) + s(standardized.do,standardized.temp), #how to add interaction for GAM
                  data = low.int,
                  family=cox.ph, weights = case)
summary(out.gam.low)

pred.vals.low<- data.frame(standardized.do = -0.3603449,
                            standardized.temp = seq(min(low.int$standardized.temp, na.rm = T),
                                                    max(low.int$standardized.temp, na.rm = T), 
                                                    0.1),
                            stratID = 631)

preds.gam.low <- predict.gam(out.gam.low, newdata = pred.vals.low, type = 'link', se.fit = T) 
plot(x = pred.vals.low$standardized.temp, y = preds.gam.low$fit, type = 'l') #if want exp put exp in front of y

#######Predictions########

# Span of temp/DO values during quart1 -----------------------------------------

df.pred.high <-expand_grid(
  standardized.do = seq(from = min(high.int$standardized.do, na.rm = T), 
               to = max(high.int$standardized.do, na.rm = T), 
               length.out = 100),
  standardized.temp = seq(from = min(high.int$standardized.temp, na.rm = T), 
                  to = max(high.int$standardized.temp, na.rm = T), 
                  length.out = 100)
)

df.pred.high <- predict(gam.high, newdata = df.pred.high,
                        se.fit = T)%>%
  as_tibble()%>%
  cbind(df.pred.high)

ggplot()+
  geom_tile(data = df.pred.high, aes(x = standardized.temp, y = standardized.do, fill = fit))+
  geom_point(data = high.int, aes(x = standardized.temp, y = standardized.do))+
  scale_fill_distiller(palette = "YlGnBu")+
  geom_contour(data = df.pred.high, aes(x= standardized.temp, y = standardized.do, z= fit), colour = "white")

# +coord_cartesian(xlim = c(1.9, 4.5), ylim = c(4, 8))




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

setwd("C:/Users/barrehan/GitHub/projects/cwa.habitat.selection.dvm")
br.ib<- read.csv("data/modif.data/hab.select.mod/br.ibutton.data/br.ibutton.pooled.csv")
br.ib$date.time <- ymd_hms(br.ib$date.time)
br.ib <- br.ib %>% force_tz(br.ib$date.time, tzone = "America/Los_Angeles")
br.ib<-br.ib[br.ib$date.time >="2021-07-30 00:00:00" & br.ib$date.time < "2021-08-06 00:00:00",]

# Data frames by high/low interval ----------------------------------------

high.int <- br.ib[br.ib$highlowDO.2hr == "high",]
high.int <- high.int[!is.na(high.int$highlowDO.2hr),]
low.int <- br.ib[br.ib$highlowDO.2hr == "low",]
low.int <- low.int[!is.na(low.int$highlowDO.2hr),]


# GAM Blue Ruin high DO 2hr window ----------------------------------------


high.int$dumt <-rep(1,nrow(high.int))
out.gam.high<-gam(cbind(dumt,stratID) ~ s(standardized.temp) + s(standardized.do) + s(standardized.do,standardized.temp), #how to add interaction for GAM
                  data = high.int,
                  family=cox.ph, weights = case)
summary(out.gam.high)

pred.vals.high<- data.frame(standardized.do = 0.1848958,
                            standardized.temp = seq(min(high.int$standardized.temp, na.rm = T),
                                                    max(high.int$standardized.temp, na.rm = T), 
                                                    0.1),
                            stratID = 1734)

preds.gam.high <- predict.gam(out.gam.high, newdata = pred.vals.high, type = 'link', se.fit = T) 
plot(x = pred.vals.high$standardized.temp, y = preds.gam.high$fit, type = 'l') #if want exp put exp in front of y

preds.gam.high<-cbind(pred.vals.high, preds.gam.high)
preds.gam.high$lcl<-preds.gam.high$fit - (1.96*preds.gam.high$se.fit)
preds.gam.high$ucl<-preds.gam.high$fit + (1.96*preds.gam.high$se.fit)

#plot
m <-ggplot(preds.gam.high, aes(x=standardized.temp, y=fit)) +
  #geom_hline(yintercept=1, color='grey',size=2)+ #horizontal line at y = 0 , reference point line of indifference
  geom_line(aes(y = fit), linewidth = 1.25, color = "#274C31")+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), alpha=0.3, fill="#3F7156")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 15, family = "serif"))+
  xlab("Standardized temperature (\u00B0C)") + ylab("Relative probability of selection")+
  theme(legend.title = element_blank()) 
# GAM Blue Ruin low DO 2hr window -----------------------------------------

low.int$dumt <-rep(1,nrow(low.int))
out.gam.low<-gam(cbind(dumt,stratID) ~ s(standardized.temp) + s(standardized.do) + s(standardized.do,standardized.temp), #how to add interaction for GAM
                 data = low.int,
                 family=cox.ph, weights = case)
summary(out.gam.low)

pred.vals.low<- data.frame(standardized.do = 0.1848958,
                           standardized.temp = seq(min(low.int$standardized.temp, na.rm = T),
                                                   max(low.int$standardized.temp, na.rm = T), 
                                                   0.1),
                           stratID = 565)

preds.gam.low <- predict.gam(out.gam.low, newdata = pred.vals.low, type = 'link', se.fit = T) 
plot(x = pred.vals.low$standardized.temp, y = preds.gam.low$fit, type = 'l') #if want exp put exp in front of y

preds.gam.low<-cbind(pred.vals.low, preds.gam.low)
preds.gam.low$lcl<-preds.gam.low$fit - (1.96*preds.gam.low$se.fit)
preds.gam.low$ucl<-preds.gam.low$fit + (1.96*preds.gam.low$se.fit)

#plot
n <-ggplot(preds.gam.low, aes(x=standardized.temp, y=fit)) +
  #geom_hline(yintercept=1, color='grey',size=2)+ #horizontal line at y = 0 , reference point line of indifference
  geom_line(aes(y = fit), linewidth = 1.25, color = "#274C31")+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), alpha=0.3, fill="#3F7156")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 15, family = "serif"))+
  xlab("Standardized temperature (\u00B0C)") + ylab("Relative probability of selection")+
  theme(legend.title = element_blank())

o <- ggarrange(m,
               n + rremove("ylab"),
               ncol = 2)

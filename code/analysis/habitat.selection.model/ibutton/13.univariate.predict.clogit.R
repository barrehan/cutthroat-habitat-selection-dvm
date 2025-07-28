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

setwd("C:/Users/barrehan/Documents/GitHub/cwa.habitat.selection.dvm")
nor.ib<- read.csv("data/modif.data/hab.select.mod/norwood.ibutton.data/norwood.ibutton.pooled.csv")
nor.ib$date.time <- ymd_hms(nor.ib$date.time)
nor.ib <- nor.ib %>% force_tz(nor.ib$date.time, tzone = "America/Los_Angeles")
nor.ib<-nor.ib[nor.ib$date.time >="2021-07-30 00:00:00" & nor.ib$date.time < "2021-08-06 00:00:00",]

# Data frames by high/low interval ----------------------------------------

high.int <- nor.ib[nor.ib$highlowDO.2hours == "high",]
high.int <- high.int[!is.na(high.int$highlowDO.2hours),]

low.int <- nor.ib[nor.ib$highlowDO.2hours == "low",]
low.int <- low.int[!is.na(low.int$highlowDO.2hours),]


# Logistic regression high temp quadratic ---------------------------------

high.clogit.temp<-clogit(formula = case ~
                      standardized.temp+
                      I(standardized.temp^2)+ 
                      strata(stratID),
                      data=high.int)
summary(high.clogit.temp) 
tab_model(high.clogit.temp, show.re.var = T)


# Logistic regression high do quadratic -----------------------------------

high.clogit.do<-clogit(formula = case ~
                           standardized.do+
                           I(standardized.do^2)+
                         strata(stratID),
                         data=high.int)
summary(high.clogit.do) 
tab_model(high.clogit.do, show.re.var = T)

# Logistic regression low temp quadratic ---------------------------------

low.clogit.temp<-clogit(formula = case ~
                     standardized.temp+
                     I(standardized.temp^2)+
                     strata(stratID),
                   data=low.int)
summary(low.clogit.temp) 
tab_model(low.clogit.temp, show.re.var = T)

# Logistic regression low do quadratic ---------------------------------

low.clogit.do<-clogit(formula = case ~
                          standardized.do+
                          I(standardized.do^2)+
                        strata(stratID),
                        data=low.int)
summary(low.clogit.do) 
tab_model(low.clogit.do, show.re.var = T)

#######################################################################

# Predictions peak (daytime) 2-hour window --------------------------------
# Temperature
pred.vals.high.temp <- data.frame(standardized.temp = seq(min(high.int$standardized.temp, na.rm = T),
                                                          max(high.int$standardized.temp, na.rm = T),
                                                          0.1),
                                  stratID = 691)

# Get predictions from model using values created above
predictions.high.temp <-predict(high.clogit.temp, newdata = pred.vals.high.temp, type = 'lp', se.fit = T)

preds.high.temp<-cbind(pred.vals.high.temp, predictions.high.temp)
preds.high.temp$lcl<-preds.high.temp$fit - (1.96*preds.high.temp$se.fit)
preds.high.temp$ucl<-preds.high.temp$fit + (1.96*preds.high.temp$se.fit)

ggplot(preds.high.temp, aes(x=standardized.temp, y=fit)) +
  geom_line(aes(y = fit), linewidth = 1.25, color = "#274C31")+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), alpha=0.3, fill="#3F7156")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 15, family = "serif"))+
  xlab("Standardized temperature (\u00B0C)") + ylab("Relative probability of selection")+
  theme(legend.title = element_blank()) 


# DO
pred.vals.high.do <- data.frame(standardized.do = seq(min(high.int$standardized.do, na.rm = T),
                                                            max(high.int$standardized.do, na.rm = T),
                                                            0.1),
                                    stratID = 691)

# Get predictions from model using values created above
predictions.high.do <-predict(high.clogit.do, newdata = pred.vals.high.do, type = 'lp', se.fit = T)

preds.high.do<-cbind(pred.vals.high.do, predictions.high.do)
preds.high.do$lcl<-preds.high.do$fit - (1.96*preds.high.do$se.fit)
preds.high.do$ucl<-preds.high.do$fit + (1.96*preds.high.do$se.fit)

ggplot(preds.high.do, aes(x=standardized.do, y=fit)) +
  geom_line(aes(y = fit), linewidth = 1.25, color = "#274C31")+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), alpha=0.3, fill="#3F7156")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 15, family = "serif"))+
  xlab("Standardized DO") + ylab("Relative probability of selection")+
  theme(legend.title = element_blank()) 


ggplot()+
  geom_line(data = preds.high.do, aes(x = standardized.do, y = fit), linewidth = 1.25, color = "#274C31")+
  geom_ribbon(data= preds.high.do, aes(x =standardized.do, ymin=lcl, ymax=ucl), alpha=0.3, fill="#3F7156")+
  geom_line(data = preds.high.temp, aes(x = standardized.temp, y =fit), linewidth = 1.25, color = "#20223E")+
  geom_ribbon(data = preds.high.temp, aes(x = standardized.temp, ymin=lcl, ymax=ucl), alpha=0.3, fill="#3F3F7B")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 15, family = "serif"))+
  xlab("Standardized variable") + ylab("Relative probability of selection")+
  theme(legend.title = element_blank()) 

#######################################################################


# Predictions low (evening) 2-hour window ---------------------------------

# Temperature
pred.vals.low.temp <- data.frame(standardized.temp = seq(min(low.int$standardized.temp, na.rm = T),
                                                          max(low.int$standardized.temp, na.rm = T),
                                                          0.1),
                                  stratID = 631)

# Get predictions from model using values created above
predictions.low.temp <-predict(low.clogit.temp, newdata = pred.vals.low.temp, type = 'lp', se.fit = T)

preds.low.temp<-cbind(pred.vals.low.temp, predictions.low.temp)
preds.low.temp$lcl<-preds.low.temp$fit - (1.96*preds.low.temp$se.fit)
preds.low.temp$ucl<-preds.low.temp$fit + (1.96*preds.low.temp$se.fit)

ggplot(preds.low.temp, aes(x=standardized.temp, y=fit)) +
  geom_line(aes(y = fit), linewidth = 1.25, color = "#274C31")+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), alpha=0.3, fill="#3F7156")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 15, family = "serif"))+
  xlab("Standardized temperature (\u00B0C)") + ylab("Relative probability of selection")+
  theme(legend.title = element_blank()) 


# DO
pred.vals.low.do <- data.frame(standardized.do = seq(min(low.int$standardized.do, na.rm = T),
                                                      max(low.int$standardized.do, na.rm = T),
                                                      0.1),
                                stratID = 631)

# Get predictions from model using values created above
predictions.low.do <-predict(low.clogit.do, newdata = pred.vals.low.do, type = 'lp', se.fit = T)

preds.low.do<-cbind(pred.vals.low.do, predictions.low.do)
preds.low.do$lcl<-preds.low.do$fit - (1.96*preds.low.do$se.fit)
preds.low.do$ucl<-preds.low.do$fit + (1.96*preds.low.do$se.fit)

ggplot(preds.low.do, aes(x=standardized.do, y=fit)) +
  geom_line(aes(y = fit), linewidth = 1.25, color = "#274C31")+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), alpha=0.3, fill="#3F7156")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 15, family = "serif"))+
  xlab("Standardized DO") + ylab("Relative probability of selection")+
  theme(legend.title = element_blank()) 


ggplot()+
  geom_line(data = preds.low.do, aes(x = standardized.do, y = fit), linewidth = 1.25, color = "#274C31")+
  geom_ribbon(data= preds.low.do, aes(x =standardized.do, ymin=lcl, ymax=ucl), alpha=0.3, fill="#3F7156")+
  geom_line(data = preds.low.temp, aes(x = standardized.temp, y =fit), linewidth = 1.25, color = "#20223E")+
  geom_ribbon(data = preds.low.temp, aes(x = standardized.temp, ymin=lcl, ymax=ucl), alpha=0.3, fill="#3F3F7B")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 15, family = "serif"))+
  xlab("Standardized variable") + ylab("Relative probability of selection")+
  theme(legend.title = element_blank()) 


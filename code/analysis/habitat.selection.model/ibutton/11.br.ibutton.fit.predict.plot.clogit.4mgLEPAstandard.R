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

# Logistic regression high DO, 17:00 & 18:00, --------------------------------

high.clogit<-clogit(formula = case ~
                      standardized.do+
                      standardized.temp+
                      standardized.do:standardized.temp+
                      strata(stratID),
                    data=high.int)
summary(high.clogit) 
tab_model(high.clogit, show.re.var = T)

# Logistic regression low DO, 5:00 & 6:00  --------------------------------------

low.clogit<-clogit(formula = case ~
                     standardized.do+
                     standardized.temp+
                     standardized.do:standardized.temp+
                     strata(stratID),
                   data=low.int)
summary(low.clogit) 
tab_model(low.clogit, show.re.var = T)


# Prediction data frame for peak DO 2-hour window -------------------------

#######Predictions peak (daytime)) DO period - DO at average EPA standard 4.0mg/L #######
##DO avg, time 17:00 & 18:00

pred.vals.noon.avgDO <- data.frame(standardized.do = 0.1848958,
                                   standardized.temp = seq(min(high.int$standardized.temp, na.rm = T),
                                                           max(high.int$standardized.temp, na.rm = T), 
                                                           0.1),
                                   stratID = 1734)

# get predictions from model using the values just created above
predictions.noon.avgDO<-predict(high.clogit, newdata=pred.vals.noon.avgDO, type='risk', se.fit=T)

preds.noon.avgDO<-cbind(pred.vals.noon.avgDO, predictions.noon.avgDO)
preds.noon.avgDO$lcl<-preds.noon.avgDO$fit - (1.96*preds.noon.avgDO$se.fit)
preds.noon.avgDO$ucl<-preds.noon.avgDO$fit + (1.96*preds.noon.avgDO$se.fit)

#plot
m <-ggplot(preds.noon.avgDO, aes(x=standardized.temp, y=fit)) +
  #geom_hline(yintercept=1, color='grey',size=2)+ #horizontal line at y = 0 , reference point line of indifference
  geom_line(aes(y = fit), linewidth = 1.25, color = "#274C31")+
  geom_ribbon(aes(ymin=lcl, ymax=ucl),alpha=0.3, fill = "#3F7156")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 15, family = "serif"))+
  labs(title = "Blue Ruin temperature selection",
       subtitle = "Early evening; 17:00 - 19:00 (4mg/L)")+
  xlab("Standardized temperature (\u00B0C)") + ylab("Relative probability of selection")+
  theme(legend.title = element_blank()) 

#######Predictions peak (daytime)) DO period - DO at average #######
##DO avg, time 05:00 & 06:00

pred.vals.night.lowDO <- data.frame(standardized.do = 0.1848958,
                                    standardized.temp = seq(min(low.int$standardized.temp, na.rm = T),
                                                            max(low.int$standardized.temp, na.rm = T), 
                                                            0.1),
                                    stratID = 565)

# get predictions from model using the values just created above
predictions.night.lowDO<-predict(low.clogit, newdata=pred.vals.night.lowDO, type='risk', se.fit=T)

preds.night.lowDO<-cbind(pred.vals.night.lowDO, predictions.night.lowDO)
preds.night.lowDO$lcl<-preds.night.lowDO$fit - (1.96*preds.night.lowDO$se.fit)
preds.night.lowDO$ucl<-preds.night.lowDO$fit + (1.96*preds.night.lowDO$se.fit)


#plot
e <-ggplot(preds.night.lowDO, aes(x=standardized.temp, y=fit)) +
  #geom_hline(yintercept=1, color='grey',size=2)+ #horizontal line at y = 0 , reference point line of indifference
  geom_line(aes(y = fit), linewidth = 1.25, color = "#274C31")+
  geom_ribbon(aes(ymin=lcl, ymax=ucl),alpha=0.3, fill="#3F7156")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 15, family = "serif"))+
  labs(title = "Blue Ruin temperature selection",
       subtitle = "Early morning; 05:00 - 07:00 (4mg/L)")+
  xlab("Standardized temperature (\u00B0C)") + ylab("Relative probability of selection")+
  theme(legend.title = element_blank()) 

f<-ggarrange(e,
             m,
             ncol = 2)
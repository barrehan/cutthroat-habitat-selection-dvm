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
setwd("C:/Users/barrehan/GitHub/projects/cwa.habitat.selection")

rt<- read.csv("data/modif.data/hab.select.mod/br.radio.tag.data/br.rt.with.interpolated.do.temp.csv")

# Standardize temp and DO -------------------------------------------------

rt$standardized.do <- as.numeric(scale(rt$dissolved.oxygen))
rt$standardized.temp <-as.numeric(scale(rt$temperature))

# Data frames for high and low DO periods ---------------------------------
# BR high DO 17:00 & 18:00
# BR low DO 05:00 & 06:00
rt$time <- hour(rt$date.time) #create time column

high.int <- rt[rt$time >= 17 & rt$time <19,] #pull times for high do period
low.int <- rt[rt$time >= 5 & rt$time < 7,] #pull times for low do period

# Logistic regression -----------------------------------------------------

high.clogit<-clogit(formula = case ~
                        standardized.do+
                        standardized.temp+
                        standardized.do:standardized.temp+
                        strata(stratID),
                      data=high.int)
summary(high.clogit)

low.clogit<-clogit(formula = case ~
                       standardized.do+
                       standardized.temp+
                       standardized.do:standardized.temp+
                       strata(stratID),
                     data=low.int)
summary(low.clogit) 

#######Predictions peak (daytime)) DO period - range temps over set high/mid/low DO 

# Prediction data frame for peak DO 2-hour window -------------------------

# Here I set DO at high (8mg/L), mid (5mg/L), and low (2mg/L) values to create 
# a prediction data frame where temp is varied across the range recorded
# We can then plot these predictions to see how probability of selection across
# temperatures varies as a function of available DO - i.e. what temps will
# fish select at given DO concentrations

##DO 8mg/L, time 17:00 & 18:00

pred.vals.noon.highDO <- data.frame(standardized.do = 3.360625,
                                    standardized.temp = seq(min(high.int$standardized.temp, na.rm = T),
                                                            max(high.int$standardized.temp, na.rm = T), 
                                                            0.1),
                                    stratID = 17)

# get predictions from model using the values just created above
predictions.noon.highDO<-predict(high.clogit, newdata=pred.vals.noon.highDO, type='risk', se.fit=T)

preds.noon.highDO<-cbind(pred.vals.noon.highDO, predictions.noon.highDO)
preds.noon.highDO$lcl<-preds.noon.highDO$fit - (1.96*preds.noon.highDO$se.fit)
preds.noon.highDO$ucl<-preds.noon.highDO$fit + (1.96*preds.noon.highDO$se.fit)
preds.noon.highDO$set.DO <- "DO 8mg/L"

##DO 5mg/L, time 17:00 & 18:00

pred.vals.noon.midDO <- data.frame(standardized.do = 0.9788282,
                                   standardized.temp = seq(min(high.int$standardized.temp, na.rm = T),
                                                           max(high.int$standardized.temp, na.rm = T), 
                                                           0.1),
                                   stratID = 17)

# get predictions from model using the values just created above
predictions.noon.midDO<-predict(high.clogit, newdata=pred.vals.noon.midDO, type='risk', se.fit=T)

preds.noon.midDO<-cbind(pred.vals.noon.midDO, predictions.noon.midDO)
preds.noon.midDO$lcl<-preds.noon.midDO$fit - (1.96*preds.noon.midDO$se.fit)
preds.noon.midDO$ucl<-preds.noon.midDO$fit + (1.96*preds.noon.midDO$se.fit)
preds.noon.midDO$set.DO <- "DO 5mg/L"

#DO 2mg/L, time 17:00 & 18:00

pred.vals.noon.lowDO <- data.frame(standardized.do = -1.402969,
                                   standardized.temp = seq(min(high.int$standardized.temp, na.rm = T),
                                                           max(high.int$standardized.temp, na.rm = T), 
                                                           0.1),
                                   stratID = 17)

# get predictions from model using the values just created above
predictions.noon.lowDO<-predict(high.clogit, newdata=pred.vals.noon.lowDO, type='risk', se.fit=T)

preds.noon.lowDO<-cbind(pred.vals.noon.lowDO, predictions.noon.lowDO)
preds.noon.lowDO$lcl<-preds.noon.lowDO$fit - (1.96*preds.noon.lowDO$se.fit)
preds.noon.lowDO$ucl<-preds.noon.lowDO$fit + (1.96*preds.noon.lowDO$se.fit)
preds.noon.lowDO$set.DO <- "DO 2mg/L"

# Combine data frames for this peak DO 2-hour time period and plot --------

preds.peak <-do.call("rbind", list(preds.noon.lowDO, preds.noon.midDO, preds.noon.highDO))

#Plot

plot.temp.eve <-ggplot(preds.peak, aes(x=standardized.temp, y=fit, color=set.DO, fill=set.DO, group=set.DO)) +
  #geom_hline(yintercept=1, color='grey',size=2)+ #horizontal line at y = 0 , reference point line of indifference
  geom_line(aes(y = fit), size = 2)+
  #facet_zoom(ylim = c(0, 50))+
  scale_colour_manual(values=c("wheat3","skyblue4", "red"))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl, fill=set.DO),alpha=0.4, color=NA)+
  scale_fill_manual(values=c("lightseagreen","grey", "pink"))+
  theme_classic()+
  labs(title = "Blue Ruin radio tag fish temperature selection across set DO concentrations",
       subtitle = "Early evening; 17:00 -19:00")+
  xlab("Standardized temperature (°C)") + ylab("Relative Probability of Selection")+
  theme(legend.title = element_blank()) 

#######Predictions low (nighttime) DO period - range temps over set high/mid/low DO 

# Prediction data frame for low DO 2-hour window -------------------------

# Here I set DO at high (8mg/L), mid (5mg/L), and low (2mg/L) values to create 
# a predicition data frame where temp is varied across the range recorded
# We can then plot these predictions to see how probability of selection across
# temperatures varies as a function of available DO - i.e. what temps will
# fish select at given DO concentrations

##DO 8mg/L, time 05:00 & 06:00

pred.vals.night.highDO <- data.frame(standardized.do = 3.360625,
                                     standardized.temp = seq(min(low.int$standardized.temp, na.rm = T),
                                                             max(low.int$standardized.temp, na.rm = T), 
                                                             0.1),
                                     stratID = 14)

# get predictions from model using the values just created above
predictions.night.highDO<-predict(low.clogit, newdata=pred.vals.night.highDO, type='risk', se.fit=T)

preds.night.highDO<-cbind(pred.vals.night.highDO, predictions.night.highDO)
preds.night.highDO$lcl<-preds.night.highDO$fit - (1.96*preds.night.highDO$se.fit)
preds.night.highDO$ucl<-preds.night.highDO$fit + (1.96*preds.night.highDO$se.fit)
preds.night.highDO$set.DO <- "DO 8mg/L"

##DO 5mg/L, time 05:00 & 06:00

pred.vals.night.midDO <- data.frame(standardized.do = 0.9788282,
                                    standardized.temp = seq(min(low.int$standardized.temp, na.rm = T),
                                                            max(low.int$standardized.temp, na.rm = T), 
                                                            0.1),
                                    stratID = 14)

# get predictions from model using the values just created above
predictions.night.midDO<-predict(low.clogit, newdata=pred.vals.night.midDO, type='risk', se.fit=T)

preds.night.midDO<-cbind(pred.vals.night.midDO, predictions.night.midDO)
preds.night.midDO$lcl<-preds.night.midDO$fit - (1.96*preds.night.midDO$se.fit)
preds.night.midDO$ucl<-preds.night.midDO$fit + (1.96*preds.night.midDO$se.fit)
preds.night.midDO$set.DO <- "DO 5mg/L"

#DO 2mg/L, time 05:00 & 06:00

pred.vals.night.lowDO <- data.frame(standardized.do = -1.402969,
                                    standardized.temp = seq(min(low.int$standardized.temp, na.rm = T),
                                                            max(low.int$standardized.temp, na.rm = T), 
                                                            0.1),
                                    stratID = 14)

# get predictions from model using the values just created above
predictions.night.lowDO<-predict(low.clogit, newdata=pred.vals.night.lowDO, type='risk', se.fit=T)

preds.night.lowDO<-cbind(pred.vals.night.lowDO, predictions.night.lowDO)
preds.night.lowDO$lcl<-preds.night.lowDO$fit - (1.96*preds.night.lowDO$se.fit)
preds.night.lowDO$ucl<-preds.night.lowDO$fit + (1.96*preds.night.lowDO$se.fit)
preds.night.lowDO$set.DO <- "DO 2mg/L"

# Combine data frames for this peak DO 2-hour time period and plot --------

preds.low <-do.call("rbind", list(preds.night.lowDO, preds.night.midDO, preds.night.highDO))

#Plot

plot.temp.morning <-ggplot(preds.low, aes(x=standardized.temp, y=fit, color=set.DO, fill=set.DO, group=set.DO)) +
  #geom_hline(yintercept=1, color='grey',size=2)+ #horizontal line at y = 0 , reference point line of indifference
  facet_zoom(ylim = c(0, 50))+
  geom_line(aes(y = fit), size = 2)+
  scale_colour_manual(values=c("wheat3","skyblue4", "red"))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl, fill=set.DO),alpha=0.4, color=NA)+
  scale_fill_manual(values=c("lightseagreen","grey", "pink"))+
  theme_classic()+
  labs(title = "Blue Ruin radio tag fish temperature selection across set DO concentrations",
       subtitle = "Early morning; 05:00 - 07:00")+
  xlab("Standardized temperature (°C)") + ylab("Relative Probability of Selection")+
  theme(legend.title = element_blank()) 

ggsave(plot.temp.eve, filename = paste("results/figures/hab.select.mod.figures/line.plots/br.rt.evening.highlowDO.selection.probability.png"), width = 18, height = 10, units = "cm")

ggsave(plot.temp.morning, filename = paste("results/figures/hab.select.mod.figures/line.plots/br.rt.morning.highlowDO.selection.probability.png"), width = 18, height = 10, units = "cm")


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

##POTENTIAL MORTS - 18, 24, 26, 36
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


# Predictions peak (daytime) DO period - DO at system average -------------
median(high.int$standardized.do)

pred.vals.noon.highDO <- data.frame(standardized.do = 0.7,
                                    standardized.temp = seq(min(high.int$standardized.temp, na.rm = T),
                                                            max(high.int$standardized.temp, na.rm = T), 
                                                            0.1),
                                    stratID = 17)

# get predictions from model using the values just created above
predictions.noon.highDO<-predict(high.clogit, newdata=pred.vals.noon.highDO, type='risk', se.fit=T)

preds.noon.highDO<-cbind(pred.vals.noon.highDO, predictions.noon.highDO)
preds.noon.highDO$lcl<-preds.noon.highDO$fit - (1.96*preds.noon.highDO$se.fit)
preds.noon.highDO$ucl<-preds.noon.highDO$fit + (1.96*preds.noon.highDO$se.fit)

plot.temp.noon.avg <-ggplot(preds.noon.highDO, aes(x=standardized.temp, y=fit)) +
  #geom_hline(yintercept=1, color='grey',size=2)+ #horizontal line at y = 0 , reference point line of indifference
  geom_line(aes(y = fit), linewidth = 2)+
  #scale_colour_manual(values=c("wheat3","skyblue4", "red"))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl),alpha=0.4, color=NA)+
  #scale_fill_manual(values=c("lightseagreen","grey", "pink"))+
  theme_classic()+
  labs(title = "Blue Ruin radio tag fish temperature selection at system average DO",
       subtitle = "Early evening; 17:00-19:00")+
  xlab("Standardized temperature (°C)") + ylab("Relative Probability of Selection")+
  theme(legend.title = element_blank()) 


# Predictions low (early am) DO period - DO at system average -------------

median(low.int$standardized.do)
pred.vals.lowDO <- data.frame(standardized.do = 0,
                                    standardized.temp = seq(min(low.int$standardized.temp, na.rm = T),
                                                            max(low.int$standardized.temp, na.rm = T), 
                                                            0.1),
                                    stratID = 14)

# get predictions from model using the values just created above
predictions.lowDO<-predict(low.clogit, newdata=pred.vals.lowDO, type='risk', se.fit=T)

preds.lowDO<-cbind(pred.vals.lowDO, predictions.lowDO)
preds.lowDO$lcl<-preds.lowDO$fit - (1.96*preds.lowDO$se.fit)
preds.lowDO$ucl<-preds.lowDO$fit + (1.96*preds.lowDO$se.fit)

plot.temp.am.avg <-ggplot(preds.lowDO, aes(x=standardized.temp, y=fit)) +
  #geom_hline(yintercept=1, color='grey',size=2)+ #horizontal line at y = 0 , reference point line of indifference
  geom_line(aes(y = fit), linewidth = 2)+
  #scale_colour_manual(values=c("wheat3","skyblue4", "red"))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl),alpha=0.4, color=NA)+
  #scale_fill_manual(values=c("lightseagreen","grey", "pink"))+
  theme_classic()+
  labs(title = "Blue Ruin radio tag fish temperature selection at system average DO",
       subtitle = "Early morning; 05:00 - 07:00")+
  xlab("Standardized temperature (°C)") + ylab("Relative Probability of Selection")+
  theme(legend.title = element_blank()) 


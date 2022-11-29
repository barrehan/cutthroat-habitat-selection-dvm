#'10-20-2022
#'Fit conditional logistic regression to pooled blue ruin ibutton data

rm(list=ls())
library(survival)
library(ggplot2)

setwd("C:/Users/barrehan/GitHub/projects/cwa.habitat.selection")
nor.ib<- read.csv("data/modif.data/hab.select.mod/norwood.ibutton.data/norwood.ibutton.pooled.csv")
# Make time ID columns factors --------------------------------------------

nor.ib$dayID <-as.factor(nor.ib$dayID)
nor.ib$hourID <-as.factor(nor.ib$hourID)
nor.ib$quarterID <-as.factor(nor.ib$quarterID)

# Data frames by day and night --------------------------------------------

day <- nor.ib[nor.ib$dayID == 0,]
night <-nor.ib[nor.ib$dayID == 1,]

# Logistic reression for day ----------------------------------------------

day.clogit<-clogit(formula = case ~
                     standardized.do+
                     standardized.temp+
                     standardized.do:standardized.temp+
                     strata(stratID),
                   data=day)
summary(day.clogit) 

# Logistic reression for night --------------------------------------------

night.clogit<-clogit(formula = case ~
                       standardized.do+
                       standardized.temp+
                       standardized.do:standardized.temp+
                       strata(stratID),
                     data=night)
summary(night.clogit) 

#######Predictions########

# Prediction data frame vary do hold temp constant ------------------------

# make new data frame with the exact variables in your clogit model, but with values for whatever you want predictions for
# make DO or Temp vary (give them values spanning your observed data set, and don't forget they may be standardized!!)
# it doesn't matter what value you put for strata but you need to put something

# Span of do values during day --------------------------------------------

#creating prediction data frame varying do, keeping temp constant at mean (0)

pred.vals.day.vary.do <- data.frame(standardized.temp = 0,
                                    standardized.do = seq(min(day$standardized.do, na.rm = T),
                                                          max(day$standardized.do, na.rm = T), 
                                                          0.1),
                                    stratID = 31)

# get predictions from model using the values just created above
predictions.day.vary.do<-predict(day.clogit, newdata=pred.vals.day.vary.do, type='risk', se.fit=T)

preds.day.do<-cbind(pred.vals.day.vary.do, predictions.day.vary.do)
preds.day.do$lcl<-preds.day.do$fit - (1.96*preds.day.do$se.fit)
preds.day.do$ucl<-preds.day.do$fit + (1.96*preds.day.do$se.fit)
preds.day.do$time<-'Day'

# Span of do values during night ------------------------------------------

#creating prediction data frame varying do, keeping temp constant at mean (0)

pred.vals.night.vary.do <- data.frame(standardized.temp = 0,
                                      standardized.do = seq(min(night$standardized.do, na.rm = T),
                                                            max(night$standardized.do, na.rm = T), 
                                                            0.1),
                                      stratID = 30)

# get predictions from model using the values just created above
predictions.night.vary.do<-predict(night.clogit, newdata=pred.vals.night.vary.do, type='risk', se.fit=T)

preds.night.do<-cbind(pred.vals.night.vary.do, predictions.night.vary.do)
preds.night.do$lcl<-preds.night.do$fit - (1.96*preds.night.do$se.fit)
preds.night.do$ucl<-preds.night.do$fit + (1.96*preds.night.do$se.fit)
preds.night.do$time<-'Night'

preds.do <-rbind(preds.day.do, preds.night.do)

#Plot

plot.do<-ggplot(preds.do, aes(x=standardized.do, y=fit, color=time, fill=time, group=time)) +
  #geom_hline(yintercept=1, color='grey',size=2)+ #horizontal line at y = 0 , reference point line of indifference
  geom_line(aes(y = fit), size = 2)+
  scale_colour_manual(values=c("wheat3","skyblue4"))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl, fill=time),alpha=0.4, color=NA)+
  scale_fill_manual(values=c("lightseagreen","skyblue4"))+
  theme_classic()+
  ggtitle("Norwood fish dissolved oxygen selection")+
  xlab("Standardized dissolved oxygen (mg/L)") + ylab("Relative Probability of Selection")+
  theme(legend.title = element_blank()) 



# Prediction data frame vary temp hold do constant ------------------------

# Span of temp values during day --------------------------------------------

#creating prediction data frame varying do, keeping temp constant at mean (0)

pred.vals.day.vary.temp <- data.frame(standardized.do = 0,
                                      standardized.temp = seq(min(day$standardized.temp, na.rm = T),
                                                              max(day$standardized.temp, na.rm = T), 
                                                              0.1),
                                      stratID = 31)

# get predictions from model using the values just created above
predictions.day.vary.temp<-predict(day.clogit, newdata=pred.vals.day.vary.temp, type='risk', se.fit=T)

preds.day.temp<-cbind(pred.vals.day.vary.temp, predictions.day.vary.temp)
preds.day.temp$lcl<-preds.day.temp$fit - (1.96*preds.day.temp$se.fit)
preds.day.temp$ucl<-preds.day.temp$fit + (1.96*preds.day.temp$se.fit)
preds.day.temp$time<-'Day'

# Span of do values during night ------------------------------------------

#creating prediction data frame varying do, keeping temp constant at mean (0)

pred.vals.night.vary.temp <- data.frame(standardized.do = 0,
                                        standardized.temp = seq(min(night$standardized.temp, na.rm = T),
                                                                max(night$standardized.temp, na.rm = T), 
                                                                0.1),
                                        stratID = 30)

# get predictions from model using the values just created above
predictions.night.vary.temp<-predict(night.clogit, newdata=pred.vals.night.vary.temp, type='risk', se.fit=T)

preds.night.temp<-cbind(pred.vals.night.vary.temp, predictions.night.vary.temp)
preds.night.temp$lcl<-preds.night.temp$fit - (1.96*preds.night.temp$se.fit)
preds.night.temp$ucl<-preds.night.temp$fit + (1.96*preds.night.temp$se.fit)
preds.night.temp$time<-'Night'

preds.temp <-rbind(preds.day.temp, preds.night.temp)

#Plot

plot.temp<-ggplot(preds.temp, aes(x=standardized.temp, y=fit, color=time, fill=time, group=time)) +
  #geom_hline(yintercept=1, color='grey',size=2)+ #horizontal line at y = 0 , reference point line of indifference
  geom_line(aes(y = fit), size = 2)+
  scale_colour_manual(values=c("wheat3","skyblue4"))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl, fill=time),alpha=0.4, color=NA)+
  scale_fill_manual(values=c("lightseagreen","skyblue4"))+
  theme_classic()+
  ggtitle("Norwood fish temperature selection")+
  xlab("Standardized temperature (°C)") + ylab("Relative Probability of Selection")+
  theme(legend.title = element_blank()) 


ggsave(plot.do, filename = paste("results/figures/hab.select.mod.figures/norwood.day.vs.night.do.selection.png"), width = 12, height = 8, units = "cm")
ggsave(plot.temp, filename = paste("results/figures/hab.select.mod.figures/norwood.day.vs.night.temp.selection.png"), width = 12, height = 8, units = "cm")

#'cCHECK FOR CORRELATION
#cor(day$standardized.do, day$standardized.temp, na.remove = T)
#'R value >.8 don't include both variables in the model


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

# Data frames by day and night --------------------------------------------

day <- br.ib[br.ib$dayID == 0,]
night <-br.ib[br.ib$dayID == 1,]

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


min(day$standardized.do, na.rm = T)
max(day$standardized.do, na.rm = T)

#creating prediction data frame varying do, keeping temp constant at mean (0)

pred.vals.day.vary.do <- data.frame(standardized.temp = 0,
                             standardized.do = seq(-1.4, 3.8, 0.2),
                            stratID = 1)

# get predictions from model using the values just created above
predictions.day<-predict(day.clogit, newdata=pred.vals.day.vary.do, type='risk', se.fit=T)

preds.day<-cbind(pred.vals.day.vary.do, predictions.day)
preds.day$lcl<-preds.day$fit - (1.96*preds.day$se.fit)
preds.day$ucl<-preds.day$fit + (1.96*preds.day$se.fit)
preds.day$time<-'Day'

# Span of do values during night ------------------------------------------

min(night$standardized.do, na.rm = T)
max(night$standardized.do, na.rm = T)

#creating prediction data frame varying do, keeping temp constant at mean (0)

pred.vals.night.vary.do <- data.frame(standardized.temp = 0,
                                      standardized.do = seq(-1.4, 3.8, 0.2),
                                      stratID = 30)

# get predictions from model using the values just created above
predictions.night<-predict(night.clogit, newdata=pred.vals.night.vary.do, type='risk', se.fit=T)

preds.night<-cbind(pred.vals.night.vary.do, predictions.night)
preds.night$lcl<-preds.night$fit - (1.96*preds.night$se.fit)
preds.night$ucl<-preds.night$fit + (1.96*preds.night$se.fit)
preds.night$time<-'Night'

preds <-rbind(preds.day, preds.night)


#interaction instead of setting to mean of standardized 
#subtract mean divide by sd, if we are tryiing to get temp value of
#11 get sd and mean of temp spread temp value of 1 means 1 sd above mean
#' -1 is 


# plot

# density is the grouping variable (to get lines on same plot). for you this might be day/night
# OR Temp so you can see the effect of DO at a high temp vs low temp

plot.do<-ggplot(preds, aes(x=standardized.do, y=fit, color=time, fill=time, group=time)) +
  #geom_hline(yintercept=1, color='grey',size=2)+ #horizontal line at y = 0 , reference point line of indifference
  geom_line(aes(y = fit), size = 2)+
  scale_colour_manual(values=c("wheat3","skyblue4"))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl, fill=time),alpha=0.4, color=NA)+
  scale_fill_manual(values=c("lightseagreen","skyblue4"))+
  # ylim(.5,2.8)+
  # ylim(.5,1.6)+
  #xlim(0,1001)+
  theme_classic()+
  ggtitle("Resp to do day vs night")+
  xlab("Years Since Treatment") + ylab("Relative Probability of Selection")+
  theme(legend.position="none")+
  theme(axis.title.x=element_blank())+
  theme(axis.title.y=element_blank()) 



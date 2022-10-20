rm(list=ls())

library(survival)
library(ggplot2)

night_clogit<-clogit(formula = case_ ~
                       water+
                       northness+
                       roads+
                       landcover+
                       landcover:density:time+
                       landcover:density:time2+
                       strata(strata),
                     data=data_night_clogit)
summary(night_clogit) 


### Selection for landcover-------------------------------------------------------------------------------------

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

# now get predictions from model using the values you just created. 
# note that "night_clogit" is what i named the model at the top
predictions_high<-predict(night_clogit, newdata=pred_vals_high, type='risk', se.fit=T)

# bind values and preds together
preds_high<-cbind(pred_vals_high, predictions_high)

# get CIs
preds_high$lcl<-preds_high$fit - (1.96*preds_high$se.fit)
preds_high$ucl<-preds_high$fit + (1.96*preds_high$se.fit)
preds_high$Density<-'High'

# now I do the same thing again but for a low density elk population
# same as above but now i have the density variable coded 0 and not 1 as above
# you may want to do something like this to plot the day and night difference
# where you change day = 0 to day = 1
# OR to look at the effect of DO when Temp is at a high value vs a low value...
pred_vals_low<-data.frame(time=seq(0,34,1),
                          time2=seq(0,34,1)^2,
                          landcover=as.factor(1),
                          density=0,
                          water=0,
                          roads=0,
                          northness=0,
                          strata=1
)

predictions_low<-predict(night_clogit, newdata=pred_vals_low, type='risk', se.fit=T)
preds_low<-cbind(pred_vals_low, predictions_low)
preds_low$lcl<-preds_low$fit - (1.96*preds_low$se.fit)
preds_low$ucl<-preds_low$fit + (1.96*preds_low$se.fit)
preds_low$Density<-'Low'

# combine predictions into 1 dataframe
preds_density <- rbind(preds_low, preds_high)

# plot

# density is the grouping variable (to get lines on same plot). for you this might be day/night
# OR Temp so you can see the effect of DO at a high temp vs low temp

plot_night_tx<-ggplot(preds_density, aes(x=time, y=fit, color=Density, fill=Density, group=Density)) +
  geom_hline(yintercept=1, color='grey',size=2)+
  geom_line(aes(y = fit), size = 2)+
  scale_colour_manual(values=c("wheat3","skyblue4"))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl, fill=Density),alpha=0.4, color=NA)+
  scale_fill_manual(values=c("lightseagreen","skyblue4"))+
  # ylim(.5,2.8)+
  # ylim(.5,1.6)+
  #xlim(0,1001)+
  theme_classic()+
  ggtitle("Treated Forest, Night")+
  xlab("Years Since Treatment") + ylab("Relative Probability of Selection")+
  theme(legend.position="none")+
  theme(axis.title.x=element_blank())+
  theme(axis.title.y=element_blank())  
#scale_y_continuous(breaks=c(1,1.5,2,2.5))+
#scale_x_continuous(breaks=seq(8,30,4))+
#geom_pointrange(aes(x=0, y=1.105, ymin=1.05, ymax=1.16))
# theme(legend.position = c(0.8, 0.8))

# save(plot_night_tx, file='C:/Users/ruprechj/Box/Documents/postdoc/elk_forest_mgmt/figures/temp/plot_night_tx.RData')

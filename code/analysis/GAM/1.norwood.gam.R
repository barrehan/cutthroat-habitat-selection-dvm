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

ggplot(high.int, aes(x= date.time, y = temperature))+
  geom_point()
ggplot(high.int, aes(x= date.time, y = dissolved.oxygen))+
  geom_point()

low.int <- nor.ib[nor.ib$highlowDO.2hours == "low",]
low.int <- low.int[!is.na(low.int$highlowDO.2hours),]

ggplot(low.int, aes(x= date.time, y = temperature))+
  geom_point()
ggplot(low.int, aes(x= date.time, y = dissolved.oxygen))+
  geom_point()

######HIGH DO ANALYSIS######
# GAM Norwood high DO 2hr window ------------------------------------------


high.int$dumt <-rep(1,nrow(high.int))
gam.high<-gam(cbind(dumt,stratID) ~ s(standardized.do,standardized.temp), #how to add interaction for GAM
             data = high.int,
             method="REML",
             family=cox.ph, weights = case)
summary(gam.high)

pred.vals.high<- data.frame(standardized.do = -0.3603449, #setting DO to 4mg/l epa standard
                                   standardized.temp = seq(min(high.int$standardized.temp, na.rm = T),
                                                           max(high.int$standardized.temp, na.rm = T),
                                                           0.1),
                                   stratID = 691)


preds.gam.high <- predict.gam(gam.high, newdata = pred.vals.high, type = 'link', se.fit = T) 
plot(x = pred.vals.high$standardized.temp, y = preds.gam.high$fit, type = 'l') #if want exp put exp in front of y

#######Predictions########

# Predictions Norwood high DO 2hr window ----------------------------------

######Line Plot#########

#Prep

preds.gam.high<-cbind(pred.vals.high, preds.gam.high)
preds.gam.high$lcl<-preds.gam.high$fit - (1.96*preds.gam.high$se.fit)
preds.gam.high$ucl<-preds.gam.high$fit + (1.96*preds.gam.high$se.fit)

#back transformation from standardized scale
mean.t<-mean(nor.ib$temperature)
sd.t <-sd(nor.ib$temperature)
mean.do<-mean(nor.ib$dissolved.oxygen)
sd.do<-sd(nor.ib$dissolved.oxygen)

preds.gam.high$temperature <- preds.gam.high$standardized.temp*sd.t+mean.t
preds.gam.high$dissolved.oxygen<-preds.gam.high$standardized.do*sd.do+mean.do

#Plot

p <-ggplot(preds.gam.high, aes(x=temperature, y=fit)) +
  geom_line(aes(y = fit), linewidth = 1.25, color = "#01353D")+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), alpha=0.2, fill="#01353D")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 15, family = "serif"))+
  xlab("Temperature (\u00B0C)") + ylab("Relative probability of selection")+
  theme(legend.title = element_blank())

######Contour Plot#######

#Prep

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

# Back transform temp and do for plot

df.pred.high$temperature <- df.pred.high$standardized.temp*sd.t+mean.t
df.pred.high$dissolved.oxygen <-df.pred.high$standardized.do*sd.do+mean.do

#Plot

a<-ggplot()+
  geom_tile(data = df.pred.high, aes(x = temperature, y = dissolved.oxygen, fill = fit))+
  geom_point(data = high.int[low.int$case == 0,], aes(x = temperature, y = dissolved.oxygen), colour = "black", alpha = 0.5)+
  geom_point(data = high.int[low.int$case == 1,], aes(x = temperature, y = dissolved.oxygen), colour = "white", alpha = 0.75)+
  scale_fill_gradientn(colours = c("#D4D9DD", "#AEB2B7", "#878195", "#7C5467", "#532A34","#291919"))+
  geom_contour(data = df.pred.high, aes(x = temperature, y = dissolved.oxygen, z= fit), colour = "white")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.key=element_rect(fill="white"), text = element_text(size = 15, family = "serif"))+
  xlab(label = "Temperature (\u00B0C)") +
  ylab(label = "Dissolved oxygen (mg/l)")+
  labs(fill='') 
# +coord_cartesian(xlim = c(1.9, 4.5), ylim = c(4, 8))


######LOW DO ANALYSIS#####
# GAM Norwood low DO 2hr window ------------------------------------------

#StratID 9990 has weird predicted temp for the DO, remove for analysis
low.int <-low.int[!(low.int$stratID == 9990),]

low.int$dumt <-rep(1,nrow(low.int))
gam.low<-gam(cbind(dumt,stratID) ~ ti(standardized.do,standardized.temp), 
             data = low.int,
             method = "REML",
             family=cox.ph, weights = case)
summary(gam.low)

pred.vals.low<- data.frame(standardized.do = -0.3603449, #setting DO to 4mg/l epa standard
                           standardized.temp = seq(min(low.int$standardized.temp, na.rm = T),
                                                   max(low.int$standardized.temp, na.rm = T), 
                                                   0.1),
                           stratID = 631)

preds.gam.low <- predict.gam(gam.low, newdata = pred.vals.low, type = 'link', se.fit = T) 
plot(x = pred.vals.low$standardized.temp, y = preds.gam.low$fit, type = 'l') #if want exp put exp in front of y

# Predictions Norwood low DO 2hr window -----------------------------------

######Line Plot#########

#Prep

preds.gam.low<-cbind(pred.vals.low, preds.gam.low)
preds.gam.low$lcl<-preds.gam.low$fit - (1.96*preds.gam.low$se.fit)
preds.gam.low$ucl<-preds.gam.low$fit + (1.96*preds.gam.low$se.fit)

#back transformation from standardized scale

preds.gam.low$temperature <- preds.gam.low$standardized.temp*sd.t+mean.t
preds.gam.low$dissolved.oxygen<-preds.gam.low$standardized.do*sd.do+mean.do

#Plot

q <-ggplot(preds.gam.low, aes(x=temperature, y=fit)) +
  geom_line(aes(y = fit), linewidth = 1.25, color = "#01353D")+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), alpha=0.2, fill="#01353D")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 15, family = "serif"))+
  xlab("Temperature (\u00B0C)") + ylab("Relative probability of selection")+
  theme(legend.title = element_blank())

######Contour Plot#######

#Prep

df.pred.low <-expand_grid(
  standardized.do = seq(from = min(low.int$standardized.do, na.rm = T), 
                        to = max(low.int$standardized.do, na.rm = T), 
                        length.out = 100),
  standardized.temp = seq(from = min(low.int$standardized.temp, na.rm = T), 
                          to = max(low.int$standardized.temp, na.rm = T), 
                          length.out = 100)
)

df.pred.low <- predict(gam.low, newdata = df.pred.low,
                        se.fit = T)%>%
  as_tibble()%>%
  cbind(df.pred.low)



df.pred.low$temperature <- df.pred.low$standardized.temp*sd.t+mean.t
df.pred.low$dissolved.oxygen <-df.pred.low$standardized.do*sd.do+mean.do

b<-ggplot()+
  geom_rug(data = low.int[low.int$case == 0,], aes(x = temperature, y = dissolved.oxygen))+
  geom_tile(data = df.pred.low, aes(x = temperature, y = dissolved.oxygen, fill = fit))+
  geom_jitter(data = low.int[low.int$case == 0,], aes(x = temperature, y = dissolved.oxygen), colour = "black", alpha = 0.5)+
  geom_point(data = low.int[low.int$case == 1,], aes(x = temperature, y = dissolved.oxygen), colour = "white", alpha = 0.75)+
  scale_fill_gradientn(colours = c("#D4D9DD", "#AEB2B7", "#878195", "#7C5467", "#532A34","#291919"))+
  geom_contour(data = df.pred.low, aes(x= temperature, y = dissolved.oxygen, z= fit), colour = "white")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.key=element_rect(fill="white"), text = element_text(size = 15, family = "serif"))+
  xlab(label = "Temperature (\u00B0C)") +
  ylab(label = "Dissolved oxygen (mg/l)")+
  labs(fill='') 

ggplot()+
  geom_histogram(data = low.int[low.int$case == 0,], aes(x = dissolved.oxygen))


nor.cont <- ggarrange(a, 
                      b + rremove("ylab"),
                      ncol = 2, nrow = 1)

nor.line <- ggarrange(p,
                      q + rremove("ylab"),
                      ncol = 2, nrow = 1)





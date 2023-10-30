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
br.ib<- read.csv("data/modif.data/hab.select.mod/br.ibutton.data/br.ibutton.pooled.csv")
br.ib$date.time <- ymd_hms(br.ib$date.time)
br.ib <- br.ib %>% force_tz(br.ib$date.time, tzone = "America/Los_Angeles")
br.ib<-br.ib[br.ib$date.time >="2021-07-30 00:00:00" & br.ib$date.time < "2021-08-06 00:00:00",]

br.ib<-br.ib[,-7:-8]

br.ib$standardized.do <- as.numeric(scale(br.ib$dissolved.oxygen))
br.ib$standardized.temp <-as.numeric(scale(br.ib$temperature))

#variables for back transformation from standardized scale

mean.t<-mean(br.ib$temperature)
sd.t <-sd(br.ib$temperature)
mean.do<-mean(br.ib$dissolved.oxygen)
sd.do<-sd(br.ib$dissolved.oxygen)

# Data frames by high/low interval ----------------------------------------

high.int <- br.ib[br.ib$highlowDO.2hr == "high",]
high.int <- high.int[!is.na(high.int$highlowDO.2hr),]
low.int <- br.ib[br.ib$highlowDO.2hr == "low",]
low.int <- low.int[!is.na(low.int$highlowDO.2hr),]

# GAM Blue Ruin high DO 2hr window ----------------------------------------


high.int$dumt <-rep(1,nrow(high.int))
gam.high<-gam(cbind(dumt,stratID) ~  s(standardized.do,standardized.temp), #how to add interaction for GAM
                  data = high.int,
                  family=cox.ph, weights = case)
summary(gam.high)

pred.vals.high.2<- data.frame(standardized.do = (2-mean.do)/sd.do, #setting DO to 4mg/l epa standard
                              standardized.temp = seq(min(high.int$standardized.temp, na.rm = T),
                                                      max(high.int$standardized.temp, na.rm = T), 
                                                      0.1),
                              stratID = 1734)

preds.gam.high.2 <- predict.gam(gam.high, newdata = pred.vals.high.2, type = 'link', se.fit = T) 

pred.vals.high.4<- data.frame(standardized.do = (4-mean.do)/sd.do, #setting DO to 4mg/l epa standard
                              standardized.temp = seq(min(high.int$standardized.temp, na.rm = T),
                                                      max(high.int$standardized.temp, na.rm = T), 
                                                      0.1),
                              stratID = 1734)

preds.gam.high.4 <- predict.gam(gam.high, newdata = pred.vals.high.4, type = 'link', se.fit = T) 


# Predictions Blue Ruin high DO 2hr window -----------------------------------

######Line Plot#########

#Prep

preds.gam.high.2<-cbind(pred.vals.high.2, preds.gam.high.2)
preds.gam.high.2$lcl<-preds.gam.high.2$fit - (1.96*preds.gam.high.2$se.fit)
preds.gam.high.2$ucl<-preds.gam.high.2$fit + (1.96*preds.gam.high.2$se.fit)

preds.gam.high.4<-cbind(pred.vals.high.4, preds.gam.high.4)
preds.gam.high.4$lcl<-preds.gam.high.4$fit - (1.96*preds.gam.high.4$se.fit)
preds.gam.high.4$ucl<-preds.gam.high.4$fit + (1.96*preds.gam.high.4$se.fit)

#back transformation from standardized scale

preds.gam.high.2$temperature <- preds.gam.high.2$standardized.temp*sd.t+mean.t
preds.gam.high.2$dissolved.oxygen<-preds.gam.high.2$standardized.do*sd.do+mean.do

preds.gam.high.4$temperature <- preds.gam.high.4$standardized.temp*sd.t+mean.t
preds.gam.high.4$dissolved.oxygen<-preds.gam.high.4$standardized.do*sd.do+mean.do

#Plot

p <- ggplot() +
  geom_line(data = preds.gam.high.4, aes(x = temperature, y = fit), linewidth = 1.25, color = "#01353D")+
  geom_ribbon(data = preds.gam.high.4, aes(x = temperature, ymin=lcl, ymax=ucl), alpha=0.2, fill="#01353D")+
  geom_line(data = preds.gam.high.2, aes(x = temperature, y = fit), linewidth = 1.25, color = "red")+
  geom_ribbon(data = preds.gam.high.2, aes(x = temperature, ymin=lcl, ymax=ucl), alpha=0.2, fill="pink")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 15, family = "serif"))+
  xlab("Temperature (\u00B0C)") + ylab("Relative probability of selection")+
  theme(legend.title = element_blank())+
  xlim(10,25)



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

#Back transform

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
  labs(fill='') +
  xlim(10,25)
# +coord_cartesian(xlim = c(1.9, 4.5), ylim = c(4, 8))

#######################################
###GAM LOW DO WINDOW###################

# GAM Blue Ruin low DO 2hr window -----------------------------------------

low.int$dumt <-rep(1,nrow(low.int))
gam.low<-gam(cbind(dumt,stratID) ~ s(standardized.do,standardized.temp), #how to add interaction for GAM
                 data = low.int,
                 family=cox.ph, weights = case)
summary(gam.low)

pred.vals.low.2<- data.frame(standardized.do = (2-mean.do)/sd.do, #setting DO to 4mg/l epa standard
                             standardized.temp = seq(min(low.int$standardized.temp, na.rm = T),
                                                     max(low.int$standardized.temp, na.rm = T), 
                                                     0.1),
                             stratID = 565)

preds.gam.low.2 <- predict.gam(gam.low, newdata = pred.vals.low.2, type = 'link', se.fit = T) 

pred.vals.low.4<- data.frame(standardized.do = (4-mean.do)/sd.do, #setting DO to 4mg/l epa standard
                             standardized.temp = seq(min(low.int$standardized.temp, na.rm = T),
                                                     max(low.int$standardized.temp, na.rm = T), 
                                                     0.1),
                             stratID = 565)

preds.gam.low.4 <- predict.gam(gam.low, newdata = pred.vals.low.4, type = 'link', se.fit = T) 


# Predictions Norwood low DO 2hr window -----------------------------------

######Line Plot#########

#Prep

preds.gam.low.2<-cbind(pred.vals.low.2, preds.gam.low.2)
preds.gam.low.2$lcl<-preds.gam.low.2$fit - (1.96*preds.gam.low.2$se.fit)
preds.gam.low.2$ucl<-preds.gam.low.2$fit + (1.96*preds.gam.low.2$se.fit)

preds.gam.low.4<-cbind(pred.vals.low.4, preds.gam.low.4)
preds.gam.low.4$lcl<-preds.gam.low.4$fit - (1.96*preds.gam.low.4$se.fit)
preds.gam.low.4$ucl<-preds.gam.low.4$fit + (1.96*preds.gam.low.4$se.fit)

#back transformation from standardized scale

preds.gam.low.2$temperature <- preds.gam.low.2$standardized.temp*sd.t+mean.t
preds.gam.low.2$dissolved.oxygen<-preds.gam.low.2$standardized.do*sd.do+mean.do

preds.gam.low.4$temperature <- preds.gam.low.4$standardized.temp*sd.t+mean.t
preds.gam.low.4$dissolved.oxygen<-preds.gam.low.4$standardized.do*sd.do+mean.do

#Plot

q <- ggplot() +
  geom_line(data = preds.gam.low.4, aes(x = temperature, y = fit), linewidth = 1.25, color = "#01353D")+
  geom_ribbon(data = preds.gam.low.4, aes(x = temperature, ymin=lcl, ymax=ucl), alpha=0.2, fill="#01353D")+
  geom_line(data = preds.gam.low.2, aes(x = temperature, y = fit), linewidth = 1.25, color = "red")+
  geom_ribbon(data = preds.gam.low.2, aes(x = temperature, ymin=lcl, ymax=ucl), alpha=0.2, fill="pink")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 15, family = "serif"))+
  xlab("Temperature (\u00B0C)") + ylab("Relative probability of selection")+
  theme(legend.title = element_blank())+
  xlim(10,25)

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

#Back transform

#Back transform

df.pred.low$temperature <- df.pred.low$standardized.temp*sd.t+mean.t
df.pred.low$dissolved.oxygen <-df.pred.low$standardized.do*sd.do+mean.do

#Plot

b<-ggplot()+
  geom_tile(data = df.pred.low, aes(x = temperature, y = dissolved.oxygen, fill = fit))+
  geom_point(data = low.int[low.int$case == 0,], aes(x = temperature, y = dissolved.oxygen), colour = "black", alpha = 0.5)+
  geom_point(data = low.int[low.int$case == 1,], aes(x = temperature, y = dissolved.oxygen), colour = "white", alpha = 0.75)+
  scale_fill_gradientn(colours = c("#D4D9DD", "#AEB2B7", "#878195", "#7C5467", "#532A34","#291919"))+
  geom_contour(data = df.pred.low, aes(x = temperature, y = dissolved.oxygen, z= fit), colour = "white")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.key=element_rect(fill="white"), text = element_text(size = 15, family = "serif"))+
  xlab(label = "Temperature (\u00B0C)") +
  ylab(label = "Dissolved oxygen (mg/l)")+
  labs(fill='') +
  xlim(10, 25)

br.cont <- ggarrange(a, 
                      b + rremove("ylab"),
                      ncol = 2, nrow = 1)

br.line <- ggarrange(p,
                      q + rremove("ylab"),
                      ncol = 2, nrow = 1)






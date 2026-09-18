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

br.ib<- read.csv("data/modif.data/hab.select.mod/br.ibutton.data/br.ibutton.pooled.csv")
br.ib$date.time <- ymd_hms(br.ib$date.time)
br.ib <- br.ib %>% force_tz(br.ib$date.time, tzone = "America/Los_Angeles")
br.ib<-br.ib[br.ib$date.time >="2021-07-30 00:00:00" & br.ib$date.time < "2021-08-06 00:00:00",]

#Need to standardize across both dataframes, and make new stratIDs, and add Location column
#Just keep first 6 columns of these og dfs

br.ib<-br.ib[,c(1:6)]
nor.ib<-nor.ib[,c(1:6)]

#'stratum column
br.ib <- br.ib %>% mutate(stratID = group_indices_(br.ib, .dots = c("date.time", "ibutton.id")))
br.ib$location <- "Blue Ruin"

#'stratum column
nor.ib <- nor.ib %>% mutate(stratID = group_indices_(nor.ib, .dots = c("date.time", "ibutton.id")))
nor.ib$stratID <- nor.ib$stratID + 9287
nor.ib$location <- "Norwood"

all.dat <- rbind(br.ib, nor.ib)

all.dat$standardized.temp <- as.numeric(scale(all.dat$temperature))
all.dat$standardized.do <- as.numeric(scale(all.dat$dissolved.oxygen))

all.dat$hour <-hour(all.dat$date.time)

do.mean <- mean(all.dat$dissolved.oxygen)
do.sd <-sd(all.dat$dissolved.oxygen)

epa.std.4mgl <-(4-do.mean)/do.sd

# Data frames by high/low interval ----------------------------------------
# going to do 6-8 

high.int <- all.dat[all.dat$hour == 6 | all.dat$hour == 7,]
low.int <- all.dat[all.dat$hour == 18 | all.dat$hour == 19,]

# GAM pooled high DO 2hr window ------------------------------------------

high.int$dumt <-rep(1,nrow(high.int))
gam.high<-gam(cbind(dumt,stratID) ~ s(standardized.temp) + s(standardized.do) + s(standardized.do,standardized.temp), #how to add interaction for GAM
              data = high.int,
              family=cox.ph, weights = case)
summary(gam.high)



pred.vals.high<- data.frame(standardized.do = epa.std.4mgl,
                            standardized.temp = seq(min(high.int$standardized.temp, na.rm = T),
                                                    max(high.int$standardized.temp, na.rm = T), 
                                                    0.1),
                            stratID = 361)

preds.gam.high <- predict.gam(gam.high, newdata = pred.vals.high, type = 'link', se.fit = T) 
plot(x = pred.vals.high$standardized.temp, y = preds.gam.high$fit, type = 'l') #if want exp put exp in front of y

# GAM pooled low DO 2hr window ------------------------------------------

#StratID 9990 has weird predicted temp for the DO, remove for analysis
# low.int <-low.int[!(low.int$stratID == 9990),]

low.int$dumt <-rep(1,nrow(low.int))
gam.low<-gam(cbind(dumt,stratID) ~ s(standardized.temp) + s(standardized.do) + s(standardized.do,standardized.temp), 
             data = low.int,
             family=cox.ph, weights = case)
summary(gam.low)

pred.vals.low<- data.frame(standardized.do = epa.std.4mgl,
                           standardized.temp = seq(min(low.int$standardized.temp, na.rm = T),
                                                   max(low.int$standardized.temp, na.rm = T), 
                                                   0.1),
                           stratID = 1038)

preds.gam.low <- predict.gam(gam.low, newdata = pred.vals.low, type = 'link', se.fit = T) 
plot(x = pred.vals.low$standardized.temp, y = preds.gam.low$fit, type = 'l') #if want exp put exp in front of y

#######Predictions########

# Predictions Norwood high DO 2hr window ----------------------------------

######Line Plot#########

#Prep

preds.gam.high<-cbind(pred.vals.high, preds.gam.high)
preds.gam.high$lcl<-preds.gam.high$fit - (1.96*preds.gam.high$se.fit)
preds.gam.high$ucl<-preds.gam.high$fit + (1.96*preds.gam.high$se.fit)

#Plot

p <-ggplot(preds.gam.high, aes(x=standardized.temp, y=fit)) +
  geom_line(aes(y = fit), linewidth = 1.25, color = "#01353D")+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), alpha=0.2, fill="#01353D")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 15, family = "serif"))+
  xlab("Standardized temperature (\u00B0C)") + ylab("Relative probability of selection")+
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

#Plot

a<-ggplot()+
  geom_tile(data = df.pred.high, aes(x = standardized.temp, y = standardized.do, fill = fit))+
  #geom_point(data = high.int[low.int$case == 0,], aes(x = standardized.temp, y = standardized.do), colour = "black", alpha = 0.5)+
  #geom_point(data = high.int[low.int$case == 1,], aes(x = standardized.temp, y = standardized.do), colour = "white", alpha = 0.75)+
  scale_fill_gradientn(colours = c("#D4D9DD", "#AEB2B7", "#878195", "#7C5467", "#532A34","#291919"))+
  geom_contour(data = df.pred.high, aes(x= standardized.temp, y = standardized.do, z= fit), colour = "white")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.key=element_rect(fill="white"), text = element_text(size = 15, family = "serif"))+
  xlab(label = "Standardized temperature (\u00B0C)") +
  ylab(label = "Dissolved oxygen (mg/l)")+
  labs(fill='') 
# +coord_cartesian(xlim = c(1.9, 4.5), ylim = c(4, 8))


# Predictions Norwood low DO 2hr window -----------------------------------

######Line Plot#########

#Prep

preds.gam.low<-cbind(pred.vals.low, preds.gam.low)
preds.gam.low$lcl<-preds.gam.low$fit - (1.96*preds.gam.low$se.fit)
preds.gam.low$ucl<-preds.gam.low$fit + (1.96*preds.gam.low$se.fit)

#Plot

q <-ggplot(preds.gam.low, aes(x=standardized.temp, y=fit)) +
  geom_line(aes(y = fit), linewidth = 1.25, color = "#01353D")+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), alpha=0.2, fill="#01353D")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 15, family = "serif"))+
  xlab("Standardized temperature (\u00B0C)") + ylab("Relative probability of selection")+
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

b<-ggplot()+
  geom_tile(data = df.pred.low, aes(x = standardized.temp, y = standardized.do, fill = fit))+
  #geom_point(data = low.int[low.int$case == 0,], aes(x = standardized.temp, y = standardized.do), colour = "black", alpha = 0.5)+
  #geom_point(data = low.int[low.int$case == 1,], aes(x = standardized.temp, y = standardized.do), colour = "white", alpha = 0.75)+
  scale_fill_gradientn(colours = c("#D4D9DD", "#AEB2B7", "#878195", "#7C5467", "#532A34","#291919"))+
  geom_contour(data = df.pred.low, aes(x= standardized.temp, y = standardized.do, z= fit), colour = "white")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.key=element_rect(fill="white"), text = element_text(size = 15, family = "serif"))+
  xlab(label = "Standardized temperature (\u00B0C)") +
  ylab(label = "Dissolved oxygen (mg/l)")+
  labs(fill='') 

cwa.cont <- ggarrange(a, 
                      b + rremove("ylab"),
                      ncol = 2, nrow = 1)

cwa.line <- ggarrange(p,
                      q + rremove("ylab"),
                      ncol = 2, nrow = 1)



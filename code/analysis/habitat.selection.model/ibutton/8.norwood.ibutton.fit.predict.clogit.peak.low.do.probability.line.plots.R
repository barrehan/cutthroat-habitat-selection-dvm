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
nor.ib<- read.csv("data/modif.data/hab.select.mod/norwood.ibutton.data/norwood.ibutton.pooled.csv")
nor.ib$date.time <- ymd_hms(nor.ib$date.time)
nor.ib <- nor.ib %>% force_tz(nor.ib$date.time, tzone = "America/Los_Angeles")
nor.ib<-nor.ib[nor.ib$date.time >="2021-07-30 00:00:00" & nor.ib$date.time < "2021-08-06 00:00:00",]

# Data frames by high/low interval ----------------------------------------

high.int <- nor.ib[nor.ib$highlowDO.2hours == "high",]
high.int <- high.int[!is.na(high.int$highlowDO.2hours),]
low.int <- nor.ib[nor.ib$highlowDO.2hours == "low",]
low.int <- low.int[!is.na(low.int$highlowDO.2hours),]

# Logistic regression high DO, 18:00 & 19:00, --------------------------------

high.clogit<-clogit(formula = case ~
                      standardized.do+
                      standardized.temp+
                      standardized.do:standardized.temp+
                      strata(stratID),
                    data=high.int)
summary(high.clogit) 
tab_model(high.clogit, show.re.var = T)
# Logistic regression low DO, 6:00 & 7:00  --------------------------------------

low.clogit<-clogit(formula = case ~
                     standardized.do+
                     standardized.temp+
                     standardized.do:standardized.temp+
                     strata(stratID),
                   data=low.int)
summary(low.clogit) 
tab_model(low.clogit, show.re.var = T)

#######Predictions peak (daytime)) DO period - range temps over set high/mid/low DO 

# Prediction data frame for peak DO 2-hour window -------------------------

#######Predictions peak (daytime)) DO period - DO at average (0) #######
##DO avg, time 18:00 & 19:00

pred.vals.noon.avgDO <- data.frame(standardized.do = 1.5,
                                   standardized.temp = seq(min(high.int$standardized.temp, na.rm = T),
                                                           max(high.int$standardized.temp, na.rm = T), 
                                                           0.1),
                                   stratID = 691)

# get predictions from model using the values just created above
predictions.noon.avgDO<-predict(high.clogit, newdata=pred.vals.noon.avgDO, type='risk', se.fit=T)

preds.noon.avgDO<-cbind(pred.vals.noon.avgDO, predictions.noon.avgDO)
preds.noon.avgDO$lcl<-preds.noon.avgDO$fit - (1.96*preds.noon.avgDO$se.fit)
preds.noon.avgDO$ucl<-preds.noon.avgDO$fit + (1.96*preds.noon.avgDO$se.fit)

#plot
m <-ggplot(preds.noon.avgDO, aes(x=standardized.temp, y=fit)) +
  #geom_hline(yintercept=1, color='grey',size=2)+ #horizontal line at y = 0 , reference point line of indifference
  geom_line(aes(y = fit), linewidth = 1.25, color = "#274C31")+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), alpha=0.3, fill="#3F7156")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 15, family = "serif"))+
  labs(title = "Norwood temperature selection",
       subtitle = "Early evening; 18:00 - 20:00 (8.4mg/L)")+
  xlab("Standardized temperature (\u00B0C)") + ylab("Relative probability of selection")+
  theme(legend.title = element_blank()) 

#######Predictions peak (daytime)) DO period - DO at average (0) #######
##DO avg, time 05:00 & 06:00

pred.vals.night.lowDO <- data.frame(standardized.do = -1.5,
                                    standardized.temp = seq(min(low.int$standardized.temp, na.rm = T),
                                                            max(low.int$standardized.temp, na.rm = T), 
                                                            0.1),
                                    stratID = 631)

# get predictions from model using the values just created above
predictions.night.lowDO<-predict(low.clogit, newdata=pred.vals.night.lowDO, type='risk', se.fit=T)

preds.night.lowDO<-cbind(pred.vals.night.lowDO, predictions.night.lowDO)
preds.night.lowDO$lcl<-preds.night.lowDO$fit - (1.96*preds.night.lowDO$se.fit)
preds.night.lowDO$ucl<-preds.night.lowDO$fit + (1.96*preds.night.lowDO$se.fit)


#plot
e <-ggplot(preds.night.lowDO, aes(x=standardized.temp, y=fit)) +
  #geom_hline(yintercept=1, color='grey',size=2)+ #horizontal line at y = 0 , reference point line of indifference
  geom_line(aes(y = fit), linewidth = 1.25, color = "#274C31")+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), alpha=0.3, fill="#3F7156")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 15, family = "serif"))+
  labs(title = "Norwood  temperature selection",
       subtitle = "Early morning; 06:00 - 08:00 (1.3mg/L)")+
  xlab("Standardized temperature (\u00B0C)") + ylab("Relative probability of selection")+
  theme(legend.title = element_blank()) 

f<-ggarrange(e,
          m,
          ncol = 2)


# ggsave(f, filename = paste("results/figures/hab.select.mod.figures/line.plots/norwood.2hr.hypoDOavg.png"), width = 23, height = 10, units = "cm")

# Threshold vals --------------------------------------------------------
# at what temperature threshold value does DO become more important 
# than temperature?

# High DO threshold -------------------------------------------------------

summary(high.clogit)
# y = a + bT + cDO + dTDO
# y = a + bT + DO(c + dT)
# pull out c + dT
# c + dT = 0 
# T* = -c/d
# c = coef st.do.
# d = coef st.do:st.temp

c <- summary(high.clogit)$coefficients[1,1]
d <- summary(high.clogit)$coefficients[3,1]
high.temp.thresh.std <- -c/d

# Calculate temp from standardized temp
# st.T* = (T* - mean)/sd
# (st.T*sd) + mean = T*

nor.sd <- sd(nor.ib$temperature)
nor.mean <- mean(nor.ib$temperature)

high.temp.thresh <-  (high.temp.thresh.std * nor.sd) + nor.mean

# Low DO threshold --------------------------------------------------------

summary(low.clogit)

c.low <- summary(low.clogit)$coefficients[1,1]
d.low <- summary(low.clogit)$coefficients[3,1]
low.temp.thresh.std <- -c.low/d.low


low.temp.thresh <-  (low.temp.thresh.std * nor.sd) + nor.mean



rm(list=ls())
library(survival)
library(ggplot2)

setwd("C:/Users/barrehan/GitHub/projects/cwa.habitat.selection")
br.ib<- read.csv("data/modif.data/hab.select.mod/br.ibutton.data/br.ibutton.pooled.csv")
# Make time ID columns factors --------------------------------------------

br.ib$dayID <-as.factor(br.ib$dayID)
br.ib$hourID <-as.factor(br.ib$hourID)
br.ib$quarterID <-as.factor(br.ib$quarterID)

# Data frames by day and night --------------------------------------------

quart1 <- br.ib[br.ib$quarterID == 1,]
quart2 <- br.ib[br.ib$quarterID == 2,]
quart3 <- br.ib[br.ib$quarterID == 3,]
quart4 <- br.ib[br.ib$quarterID == 4,]


# Logistic regression quarter 1, midnight - 6am ---------------------------


quart1.clogit<-clogit(formula = case ~
                        standardized.do+
                        standardized.temp+
                        standardized.do:standardized.temp+
                        strata(stratID),
                      data=quart1)
summary(quart1.clogit) 

# Logistic regression quarter 2, 6am - noon -------------------------------


quart2.clogit<-clogit(formula = case ~
                        standardized.do+
                        standardized.temp+
                        standardized.do:standardized.temp+
                        strata(stratID),
                      data=quart2)
summary(quart2.clogit) 


# Logistic regression quarter 3, noon - 6pm -------------------------------

quart3.clogit<-clogit(formula = case ~
                        standardized.do+
                        standardized.temp+
                        standardized.do:standardized.temp+
                        strata(stratID),
                      data=quart3)
summary(quart3.clogit)

# Logistic regression quarter 4, 6pm - midnight ---------------------------

quart4.clogit<-clogit(formula = case ~
                        standardized.do+
                        standardized.temp+
                        standardized.do:standardized.temp+
                        strata(stratID),
                      data=quart4)
summary(quart4.clogit) 

#######Predictions DO 5mg/L########

# Prediction data frame vary temp hold do constant ------------------------

# Span of temp values during quart1 -----------------------------------------

#creating prediction data frame varying do, keeping temp constant at mean (0)

pred.vals.quart1.vary.temp <- data.frame(standardized.do = 0.9788282,
                                         standardized.temp = seq(min(quart1$standardized.temp, na.rm = T),
                                                                 max(quart1$standardized.temp, na.rm = T), 
                                                                 0.1),
                                         stratID = 60)

# get predictions from model using the values just created above
predictions.quart1.vary.temp<-predict(quart1.clogit, newdata=pred.vals.quart1.vary.temp, type='risk', se.fit=T)

preds.quart1.temp<-cbind(pred.vals.quart1.vary.temp, predictions.quart1.vary.temp)
preds.quart1.temp$lcl<-preds.quart1.temp$fit - (1.96*preds.quart1.temp$se.fit)
preds.quart1.temp$ucl<-preds.quart1.temp$fit + (1.96*preds.quart1.temp$se.fit)
preds.quart1.temp$time<-'midnight - 6am'

# Span of do values during quart2 -----------------------------------------

#creating prediction data frame varying do, keeping temp constant at mean (0)

pred.vals.quart2.vary.temp <- data.frame(standardized.do = 0.9788282,
                                         standardized.temp = seq(min(quart2$standardized.temp, na.rm = T),
                                                                 max(quart2$standardized.temp, na.rm = T), 
                                                                 0.1),
                                         stratID = 90)

# get predictions from model using the values just created above
predictions.quart2.vary.temp<-predict(quart2.clogit, newdata=pred.vals.quart2.vary.temp, type='risk', se.fit=T)

preds.quart2.temp<-cbind(pred.vals.quart2.vary.temp, predictions.quart2.vary.temp)
preds.quart2.temp$lcl<-preds.quart2.temp$fit - (1.96*preds.quart2.temp$se.fit)
preds.quart2.temp$ucl<-preds.quart2.temp$fit + (1.96*preds.quart2.temp$se.fit)
preds.quart2.temp$time<-'6am - noon'

# Span of do values during quart3 -----------------------------------------

#creating prediction data frame varying do, keeping temp constant at mean (0)

pred.vals.quart3.vary.temp <- data.frame(standardized.do = 0.9788282,
                                         standardized.temp = seq(min(quart3$standardized.temp, na.rm = T),
                                                                 max(quart3$standardized.temp, na.rm = T), 
                                                                 0.1),
                                         stratID = 1)

# get predictions from model using the values just created above
predictions.quart3.vary.temp<-predict(quart3.clogit, newdata=pred.vals.quart3.vary.temp, type='risk', se.fit=T)

preds.quart3.temp <- cbind(pred.vals.quart3.vary.temp, predictions.quart3.vary.temp)
preds.quart3.temp$lcl <-preds.quart3.temp$fit - (1.96*preds.quart3.temp$se.fit)
preds.quart3.temp$ucl <-preds.quart3.temp$fit + (1.96*preds.quart3.temp$se.fit)
preds.quart3.temp$time <-'noon - 6pm'

# Span of do values during quart4 -----------------------------------------

#creating prediction data frame varying do, keeping temp constant at mean (0)

pred.vals.quart4.vary.temp <- data.frame(standardized.do = 0.9788282,
                                         standardized.temp = seq(min(quart4$standardized.temp, na.rm = T),
                                                                 max(quart4$standardized.temp, na.rm = T), 
                                                                 0.1),
                                         stratID = 30)

# get predictions from model using the values just created above
predictions.quart4.vary.temp<-predict(quart4.clogit, newdata=pred.vals.quart4.vary.temp, type='risk', se.fit=T)

preds.quart4.temp <- cbind(pred.vals.quart4.vary.temp, predictions.quart4.vary.temp)
preds.quart4.temp$lcl <- preds.quart4.temp$fit - (1.96*preds.quart4.temp$se.fit)
preds.quart4.temp$ucl <- preds.quart4.temp$fit + (1.96*preds.quart4.temp$se.fit)
preds.quart4.temp$time <- '6pm - midnight'

preds.temp <-do.call("rbind", list(preds.quart1.temp, preds.quart2.temp, preds.quart3.temp, preds.quart4.temp))

#Plot

plot.temp <-ggplot(preds.temp, aes(x=standardized.temp, y=fit, color=time, fill=time, group=time)) +
  #geom_hline(yintercept=1, color='grey',size=2)+ #horizontal line at y = 0 , reference point line of indifference
  geom_line(aes(y = fit), size = 2)+
  scale_colour_manual(values=c("wheat3","skyblue4", "red", "black"))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl, fill=time),alpha=0.4, color=NA)+
  scale_fill_manual(values=c("lightseagreen","skyblue4", "pink", "grey"))+
  theme_classic()+
  labs(title = "Blue Ruin fish temperature selection",
       subtitle = "Dissolved oxygen held at 5mg/L")+
  xlab("Standardized temperature (°C)") + ylab("Relative Probability of Selection")+
  theme(legend.title = element_blank()) 

ggsave(plot.temp, filename = paste("results/figures/hab.select.mod.figures/br.quarterly.temp.selection.do.5mgl.png"), width = 12, height = 8, units = "cm")

#######Predictions DO 2mg/L########

# Prediction data frame vary temp hold do constant ------------------------

# Span of temp values during quart1 -----------------------------------------

#creating prediction data frame varying do, keeping temp constant at mean (0)

pred.vals.quart1.vary.temp <- data.frame(standardized.do = -1.402969,
                                         standardized.temp = seq(min(quart1$standardized.temp, na.rm = T),
                                                                 max(quart1$standardized.temp, na.rm = T), 
                                                                 0.1),
                                         stratID = 60)

# get predictions from model using the values just created above
predictions.quart1.vary.temp<-predict(quart1.clogit, newdata=pred.vals.quart1.vary.temp, type='risk', se.fit=T)

preds.quart1.temp<-cbind(pred.vals.quart1.vary.temp, predictions.quart1.vary.temp)
preds.quart1.temp$lcl<-preds.quart1.temp$fit - (1.96*preds.quart1.temp$se.fit)
preds.quart1.temp$ucl<-preds.quart1.temp$fit + (1.96*preds.quart1.temp$se.fit)
preds.quart1.temp$time<-'midnight - 6am'

# Span of do values during quart2 -----------------------------------------

#creating prediction data frame varying do, keeping temp constant at mean (0)

pred.vals.quart2.vary.temp <- data.frame(standardized.do = -1.402969,
                                         standardized.temp = seq(min(quart2$standardized.temp, na.rm = T),
                                                                 max(quart2$standardized.temp, na.rm = T), 
                                                                 0.1),
                                         stratID = 90)

# get predictions from model using the values just created above
predictions.quart2.vary.temp<-predict(quart2.clogit, newdata=pred.vals.quart2.vary.temp, type='risk', se.fit=T)

preds.quart2.temp<-cbind(pred.vals.quart2.vary.temp, predictions.quart2.vary.temp)
preds.quart2.temp$lcl<-preds.quart2.temp$fit - (1.96*preds.quart2.temp$se.fit)
preds.quart2.temp$ucl<-preds.quart2.temp$fit + (1.96*preds.quart2.temp$se.fit)
preds.quart2.temp$time<-'6am - noon'

# Span of do values during quart3 -----------------------------------------

#creating prediction data frame varying do, keeping temp constant at mean (0)

pred.vals.quart3.vary.temp <- data.frame(standardized.do = -1.402969,
                                         standardized.temp = seq(min(quart3$standardized.temp, na.rm = T),
                                                                 max(quart3$standardized.temp, na.rm = T), 
                                                                 0.1),
                                         stratID = 1)

# get predictions from model using the values just created above
predictions.quart3.vary.temp<-predict(quart3.clogit, newdata=pred.vals.quart3.vary.temp, type='risk', se.fit=T)

preds.quart3.temp <- cbind(pred.vals.quart3.vary.temp, predictions.quart3.vary.temp)
preds.quart3.temp$lcl <-preds.quart3.temp$fit - (1.96*preds.quart3.temp$se.fit)
preds.quart3.temp$ucl <-preds.quart3.temp$fit + (1.96*preds.quart3.temp$se.fit)
preds.quart3.temp$time <-'noon - 6pm'

# Span of do values during quart4 -----------------------------------------

#creating prediction data frame varying do, keeping temp constant at mean (0)

pred.vals.quart4.vary.temp <- data.frame(standardized.do = -1.402969,
                                         standardized.temp = seq(min(quart4$standardized.temp, na.rm = T),
                                                                 max(quart4$standardized.temp, na.rm = T), 
                                                                 0.1),
                                         stratID = 30)

# get predictions from model using the values just created above
predictions.quart4.vary.temp<-predict(quart4.clogit, newdata=pred.vals.quart4.vary.temp, type='risk', se.fit=T)

preds.quart4.temp <- cbind(pred.vals.quart4.vary.temp, predictions.quart4.vary.temp)
preds.quart4.temp$lcl <- preds.quart4.temp$fit - (1.96*preds.quart4.temp$se.fit)
preds.quart4.temp$ucl <- preds.quart4.temp$fit + (1.96*preds.quart4.temp$se.fit)
preds.quart4.temp$time <- '6pm - midnight'

preds.temp <-do.call("rbind", list(preds.quart1.temp, preds.quart2.temp, preds.quart3.temp, preds.quart4.temp))

#Plot

plot.temp <-ggplot(preds.temp, aes(x=standardized.temp, y=fit, color=time, fill=time, group=time)) +
  #geom_hline(yintercept=1, color='grey',size=2)+ #horizontal line at y = 0 , reference point line of indifference
  geom_line(aes(y = fit), size = 2)+
  scale_colour_manual(values=c("wheat3","skyblue4", "red", "black"))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl, fill=time),alpha=0.4, color=NA)+
  scale_fill_manual(values=c("lightseagreen","skyblue4", "pink", "grey"))+
  theme_classic()+
  labs(title = "Blue Ruin fish temperature selection",
       subtitle = "Dissolved oxygen held at 2mg/L")+
  xlab("Standardized temperature (°C)") + ylab("Relative Probability of Selection")+
  theme(legend.title = element_blank()) 

ggsave(plot.temp, filename = paste("results/figures/hab.select.mod.figures/br.quarterly.temp.selection.do.2mgl.png"), width = 12, height = 8, units = "cm")

#######Predictions DO 8mg/L########

# Prediction data frame vary temp hold do constant ------------------------

# Span of temp values during quart1 -----------------------------------------

#creating prediction data frame varying do, keeping temp constant at mean (0)

pred.vals.quart1.vary.temp <- data.frame(standardized.do = 3.360625,
                                         standardized.temp = seq(min(quart1$standardized.temp, na.rm = T),
                                                                 max(quart1$standardized.temp, na.rm = T), 
                                                                 0.1),
                                         stratID = 60)

# get predictions from model using the values just created above
predictions.quart1.vary.temp<-predict(quart1.clogit, newdata=pred.vals.quart1.vary.temp, type='risk', se.fit=T)

preds.quart1.temp<-cbind(pred.vals.quart1.vary.temp, predictions.quart1.vary.temp)
preds.quart1.temp$lcl<-preds.quart1.temp$fit - (1.96*preds.quart1.temp$se.fit)
preds.quart1.temp$ucl<-preds.quart1.temp$fit + (1.96*preds.quart1.temp$se.fit)
preds.quart1.temp$time<-'midnight - 6am'

# Span of do values during quart2 -----------------------------------------

#creating prediction data frame varying do, keeping temp constant at mean (0)

pred.vals.quart2.vary.temp <- data.frame(standardized.do = 3.360625,
                                         standardized.temp = seq(min(quart2$standardized.temp, na.rm = T),
                                                                 max(quart2$standardized.temp, na.rm = T), 
                                                                 0.1),
                                         stratID = 90)

# get predictions from model using the values just created above
predictions.quart2.vary.temp<-predict(quart2.clogit, newdata=pred.vals.quart2.vary.temp, type='risk', se.fit=T)

preds.quart2.temp<-cbind(pred.vals.quart2.vary.temp, predictions.quart2.vary.temp)
preds.quart2.temp$lcl<-preds.quart2.temp$fit - (1.96*preds.quart2.temp$se.fit)
preds.quart2.temp$ucl<-preds.quart2.temp$fit + (1.96*preds.quart2.temp$se.fit)
preds.quart2.temp$time<-'6am - noon'

# Span of do values during quart3 -----------------------------------------

#creating prediction data frame varying do, keeping temp constant at mean (0)

pred.vals.quart3.vary.temp <- data.frame(standardized.do = 3.360625,
                                         standardized.temp = seq(min(quart3$standardized.temp, na.rm = T),
                                                                 max(quart3$standardized.temp, na.rm = T), 
                                                                 0.1),
                                         stratID = 1)

# get predictions from model using the values just created above
predictions.quart3.vary.temp<-predict(quart3.clogit, newdata=pred.vals.quart3.vary.temp, type='risk', se.fit=T)

preds.quart3.temp <- cbind(pred.vals.quart3.vary.temp, predictions.quart3.vary.temp)
preds.quart3.temp$lcl <-preds.quart3.temp$fit - (1.96*preds.quart3.temp$se.fit)
preds.quart3.temp$ucl <-preds.quart3.temp$fit + (1.96*preds.quart3.temp$se.fit)
preds.quart3.temp$time <-'noon - 6pm'

# Span of do values during quart4 -----------------------------------------

#creating prediction data frame varying do, keeping temp constant at mean (0)

pred.vals.quart4.vary.temp <- data.frame(standardized.do = 3.360625,
                                         standardized.temp = seq(min(quart4$standardized.temp, na.rm = T),
                                                                 max(quart4$standardized.temp, na.rm = T), 
                                                                 0.1),
                                         stratID = 30)

# get predictions from model using the values just created above
predictions.quart4.vary.temp<-predict(quart4.clogit, newdata=pred.vals.quart4.vary.temp, type='risk', se.fit=T)

preds.quart4.temp <- cbind(pred.vals.quart4.vary.temp, predictions.quart4.vary.temp)
preds.quart4.temp$lcl <- preds.quart4.temp$fit - (1.96*preds.quart4.temp$se.fit)
preds.quart4.temp$ucl <- preds.quart4.temp$fit + (1.96*preds.quart4.temp$se.fit)
preds.quart4.temp$time <- '6pm - midnight'

preds.temp <-do.call("rbind", list(preds.quart1.temp, preds.quart2.temp, preds.quart3.temp, preds.quart4.temp))

#Plot

plot.temp <-ggplot(preds.temp, aes(x=standardized.temp, y=fit, color=time, fill=time, group=time)) +
  #geom_hline(yintercept=1, color='grey',size=2)+ #horizontal line at y = 0 , reference point line of indifference
  geom_line(aes(y = fit), size = 2)+
  scale_colour_manual(values=c("wheat3","skyblue4", "red", "black"))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl, fill=time),alpha=0.4, color=NA)+
  scale_fill_manual(values=c("lightseagreen","skyblue4", "pink", "grey"))+
  theme_classic()+
  labs(title = "Blue Ruin fish temperature selection",
       subtitle = "Dissolved oxygen held at 8mg/L")+
  xlab("Standardized temperature (°C)") + ylab("Relative Probability of Selection")+
  theme(legend.title = element_blank()) 

ggsave(plot.temp, filename = paste("results/figures/hab.select.mod.figures/br.quarterly.temp.selection.do.8mgl.png"), width = 12, height = 8, units = "cm")

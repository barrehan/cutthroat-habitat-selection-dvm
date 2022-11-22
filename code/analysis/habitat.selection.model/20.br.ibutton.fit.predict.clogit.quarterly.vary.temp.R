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

#######Predictions########

# Prediction data frame vary do hold temp constant ------------------------

# Span of do values during quart1 -----------------------------------------

#creating prediction data frame varying do, keeping temp constant at mean (0)

pred.vals.quart1.vary.do <- data.frame(standardized.temp = 0.7091638,
                                       standardized.do = seq(min(quart1$standardized.do, na.rm = T),
                                                             max(quart1$standardized.do, na.rm = T), 
                                                             0.1),
                                       stratID = 60)

# get predictions from model using the values just created above
predictions.quart1.vary.do<-predict(quart1.clogit, newdata=pred.vals.quart1.vary.do, type='risk', se.fit=T)

preds.quart1.do<-cbind(pred.vals.quart1.vary.do, predictions.quart1.vary.do)
preds.quart1.do$lcl<-preds.quart1.do$fit - (1.96*preds.quart1.do$se.fit)
preds.quart1.do$ucl<-preds.quart1.do$fit + (1.96*preds.quart1.do$se.fit)
preds.quart1.do$time<-'midnight - 6am'

# Span of do values during quart2 -----------------------------------------

#creating prediction data frame varying do, keeping temp constant at mean (0)

pred.vals.quart2.vary.do <- data.frame(standardized.temp = 0.7091638,
                                       standardized.do = seq(min(quart2$standardized.do, na.rm = T),
                                                             max(quart2$standardized.do, na.rm = T), 
                                                             0.1),
                                       stratID = 90)

# get predictions from model using the values just created above
predictions.quart2.vary.do<-predict(quart2.clogit, newdata=pred.vals.quart2.vary.do, type='risk', se.fit=T)

preds.quart2.do<-cbind(pred.vals.quart2.vary.do, predictions.quart2.vary.do)
preds.quart2.do$lcl<-preds.quart2.do$fit - (1.96*preds.quart2.do$se.fit)
preds.quart2.do$ucl<-preds.quart2.do$fit + (1.96*preds.quart2.do$se.fit)
preds.quart2.do$time<-'6am - noon'

# Span of do values during quart3 -----------------------------------------

#creating prediction data frame varying do, keeping temp constant at mean (0)

pred.vals.quart3.vary.do <- data.frame(standardized.temp = 0.7091638,
                                       standardized.do = seq(min(quart3$standardized.do, na.rm = T),
                                                             max(quart3$standardized.do, na.rm = T), 
                                                             0.1),
                                       stratID = 1)

# get predictions from model using the values just created above
predictions.quart3.vary.do<-predict(quart3.clogit, newdata=pred.vals.quart3.vary.do, type='risk', se.fit=T)

preds.quart3.do <- cbind(pred.vals.quart3.vary.do, predictions.quart3.vary.do)
preds.quart3.do$lcl <-preds.quart3.do$fit - (1.96*preds.quart3.do$se.fit)
preds.quart3.do$ucl <-preds.quart3.do$fit + (1.96*preds.quart3.do$se.fit)
preds.quart3.do$time <-'noon - 6pm'

# Span of do values during quart4 -----------------------------------------

#creating prediction data frame varying do, keeping temp constant at mean (0)

pred.vals.quart4.vary.do <- data.frame(standardized.temp = 0.7091638,
                                       standardized.do = seq(min(quart4$standardized.do, na.rm = T),
                                                             max(quart4$standardized.do, na.rm = T), 
                                                             0.1),
                                       stratID = 30)

# get predictions from model using the values just created above
predictions.quart4.vary.do<-predict(quart4.clogit, newdata=pred.vals.quart4.vary.do, type='risk', se.fit=T)

preds.quart4.do <- cbind(pred.vals.quart4.vary.do, predictions.quart4.vary.do)
preds.quart4.do$lcl <- preds.quart4.do$fit - (1.96*preds.quart4.do$se.fit)
preds.quart4.do$ucl <- preds.quart4.do$fit + (1.96*preds.quart4.do$se.fit)
preds.quart4.do$time <- '6pm - midnight'

preds.do <-do.call("rbind", list(preds.quart1.do, preds.quart2.do, preds.quart3.do, preds.quart4.do))

#Plot

plot.do <- ggplot(preds.do, aes(x=standardized.do, y=fit, color=time, fill=time, group=time)) +
  #geom_hline(yintercept=1, color='grey',size=2)+ #horizontal line at y = 0 , reference point line of indifference
  geom_line(aes(y = fit), size = 2)+
  facet_zoom(ylim = c(0, 50))+
  scale_colour_manual(values=c("wheat3","skyblue4", "red", "black"))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl, fill=time),alpha=0.4, color=NA)+
  scale_fill_manual(values=c("lightseagreen","skyblue4", "pink", "grey"))+
  theme_classic()+
  labs(title = "Blue Ruin fish dissolved oxygen selection",
       subtitle = "Temperature held at 15°C")+
  xlab("Standardized dissolved oxygen (mg/L)") + ylab("Relative Probability of Selection")+
  theme(legend.title = element_blank()) 

ggsave(plot.do, filename = paste("results/figures/hab.select.mod.figures/br.quarterly.do.selection.temp.15C.png"), width = 16, height = 8, units = "cm")

#######Predictions 13C########

# Prediction data frame vary do hold temp constant ------------------------

# Span of do values during quart1 -----------------------------------------

#creating prediction data frame varying do, keeping temp constant at mean (0)

pred.vals.quart1.vary.do <- data.frame(standardized.temp = -0.5156625,
                                       standardized.do = seq(min(quart1$standardized.do, na.rm = T),
                                                             max(quart1$standardized.do, na.rm = T), 
                                                             0.1),
                                       stratID = 60)

# get predictions from model using the values just created above
predictions.quart1.vary.do<-predict(quart1.clogit, newdata=pred.vals.quart1.vary.do, type='risk', se.fit=T)

preds.quart1.do<-cbind(pred.vals.quart1.vary.do, predictions.quart1.vary.do)
preds.quart1.do$lcl<-preds.quart1.do$fit - (1.96*preds.quart1.do$se.fit)
preds.quart1.do$ucl<-preds.quart1.do$fit + (1.96*preds.quart1.do$se.fit)
preds.quart1.do$time<-'midnight - 6am'

# Span of do values during quart2 -----------------------------------------

#creating prediction data frame varying do, keeping temp constant at mean (0)

pred.vals.quart2.vary.do <- data.frame(standardized.temp = -0.5156625,
                                       standardized.do = seq(min(quart2$standardized.do, na.rm = T),
                                                             max(quart2$standardized.do, na.rm = T), 
                                                             0.1),
                                       stratID = 90)

# get predictions from model using the values just created above
predictions.quart2.vary.do<-predict(quart2.clogit, newdata=pred.vals.quart2.vary.do, type='risk', se.fit=T)

preds.quart2.do<-cbind(pred.vals.quart2.vary.do, predictions.quart2.vary.do)
preds.quart2.do$lcl<-preds.quart2.do$fit - (1.96*preds.quart2.do$se.fit)
preds.quart2.do$ucl<-preds.quart2.do$fit + (1.96*preds.quart2.do$se.fit)
preds.quart2.do$time<-'6am - noon'

# Span of do values during quart3 -----------------------------------------

#creating prediction data frame varying do, keeping temp constant at mean (0)

pred.vals.quart3.vary.do <- data.frame(standardized.temp = -0.5156625,
                                       standardized.do = seq(min(quart3$standardized.do, na.rm = T),
                                                             max(quart3$standardized.do, na.rm = T), 
                                                             0.1),
                                       stratID = 1)

# get predictions from model using the values just created above
predictions.quart3.vary.do<-predict(quart3.clogit, newdata=pred.vals.quart3.vary.do, type='risk', se.fit=T)

preds.quart3.do <- cbind(pred.vals.quart3.vary.do, predictions.quart3.vary.do)
preds.quart3.do$lcl <-preds.quart3.do$fit - (1.96*preds.quart3.do$se.fit)
preds.quart3.do$ucl <-preds.quart3.do$fit + (1.96*preds.quart3.do$se.fit)
preds.quart3.do$time <-'noon - 6pm'

# Span of do values during quart4 -----------------------------------------

#creating prediction data frame varying do, keeping temp constant at mean (0)

pred.vals.quart4.vary.do <- data.frame(standardized.temp = -0.5156625,
                                       standardized.do = seq(min(quart4$standardized.do, na.rm = T),
                                                             max(quart4$standardized.do, na.rm = T), 
                                                             0.1),
                                       stratID = 30)

# get predictions from model using the values just created above
predictions.quart4.vary.do<-predict(quart4.clogit, newdata=pred.vals.quart4.vary.do, type='risk', se.fit=T)

preds.quart4.do <- cbind(pred.vals.quart4.vary.do, predictions.quart4.vary.do)
preds.quart4.do$lcl <- preds.quart4.do$fit - (1.96*preds.quart4.do$se.fit)
preds.quart4.do$ucl <- preds.quart4.do$fit + (1.96*preds.quart4.do$se.fit)
preds.quart4.do$time <- '6pm - midnight'

preds.do <-do.call("rbind", list(preds.quart1.do, preds.quart2.do, preds.quart3.do, preds.quart4.do))

#Plot

plot.do <- ggplot(preds.do, aes(x=standardized.do, y=fit, color=time, fill=time, group=time)) +
  #geom_hline(yintercept=1, color='grey',size=2)+ #horizontal line at y = 0 , reference point line of indifference
  geom_line(aes(y = fit), size = 2)+
  facet_zoom(ylim = c(0, 50))+
  scale_colour_manual(values=c("wheat3","skyblue4", "red", "black"))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl, fill=time),alpha=0.4, color=NA)+
  scale_fill_manual(values=c("lightseagreen","skyblue4", "pink", "grey"))+
  theme_classic()+
  labs(title = "Blue Ruin fish dissolved oxygen selection",
       subtitle = "Temperature held at 13°C")+
  xlab("Standardized dissolved oxygen (mg/L)") + ylab("Relative Probability of Selection")+
  theme(legend.title = element_blank()) 

ggsave(plot.do, filename = paste("results/figures/hab.select.mod.figures/br.quarterly.do.selection.temp.13C.png"), width = 16, height = 8, units = "cm")

#######Predictions 17C########

# Prediction data frame vary do hold temp constant ------------------------

# Span of do values during quart1 -----------------------------------------

#creating prediction data frame varying do, keeping temp constant at mean (0)

pred.vals.quart1.vary.do <- data.frame(standardized.temp = 1.93399,
                                       standardized.do = seq(min(quart1$standardized.do, na.rm = T),
                                                             max(quart1$standardized.do, na.rm = T), 
                                                             0.1),
                                       stratID = 60)

# get predictions from model using the values just created above
predictions.quart1.vary.do<-predict(quart1.clogit, newdata=pred.vals.quart1.vary.do, type='risk', se.fit=T)

preds.quart1.do<-cbind(pred.vals.quart1.vary.do, predictions.quart1.vary.do)
preds.quart1.do$lcl<-preds.quart1.do$fit - (1.96*preds.quart1.do$se.fit)
preds.quart1.do$ucl<-preds.quart1.do$fit + (1.96*preds.quart1.do$se.fit)
preds.quart1.do$time<-'midnight - 6am'

# Span of do values during quart2 -----------------------------------------

#creating prediction data frame varying do, keeping temp constant at mean (0)

pred.vals.quart2.vary.do <- data.frame(standardized.temp = 1.93399,
                                       standardized.do = seq(min(quart2$standardized.do, na.rm = T),
                                                             max(quart2$standardized.do, na.rm = T), 
                                                             0.1),
                                       stratID = 90)

# get predictions from model using the values just created above
predictions.quart2.vary.do<-predict(quart2.clogit, newdata=pred.vals.quart2.vary.do, type='risk', se.fit=T)

preds.quart2.do<-cbind(pred.vals.quart2.vary.do, predictions.quart2.vary.do)
preds.quart2.do$lcl<-preds.quart2.do$fit - (1.96*preds.quart2.do$se.fit)
preds.quart2.do$ucl<-preds.quart2.do$fit + (1.96*preds.quart2.do$se.fit)
preds.quart2.do$time<-'6am - noon'

# Span of do values during quart3 -----------------------------------------

#creating prediction data frame varying do, keeping temp constant at mean (0)

pred.vals.quart3.vary.do <- data.frame(standardized.temp = 1.93399,
                                       standardized.do = seq(min(quart3$standardized.do, na.rm = T),
                                                             max(quart3$standardized.do, na.rm = T), 
                                                             0.1),
                                       stratID = 1)

# get predictions from model using the values just created above
predictions.quart3.vary.do<-predict(quart3.clogit, newdata=pred.vals.quart3.vary.do, type='risk', se.fit=T)

preds.quart3.do <- cbind(pred.vals.quart3.vary.do, predictions.quart3.vary.do)
preds.quart3.do$lcl <-preds.quart3.do$fit - (1.96*preds.quart3.do$se.fit)
preds.quart3.do$ucl <-preds.quart3.do$fit + (1.96*preds.quart3.do$se.fit)
preds.quart3.do$time <-'noon - 6pm'

# Span of do values during quart4 -----------------------------------------

#creating prediction data frame varying do, keeping temp constant at mean (0)

pred.vals.quart4.vary.do <- data.frame(standardized.temp = 1.93399,
                                       standardized.do = seq(min(quart4$standardized.do, na.rm = T),
                                                             max(quart4$standardized.do, na.rm = T), 
                                                             0.1),
                                       stratID = 30)

# get predictions from model using the values just created above
predictions.quart4.vary.do<-predict(quart4.clogit, newdata=pred.vals.quart4.vary.do, type='risk', se.fit=T)

preds.quart4.do <- cbind(pred.vals.quart4.vary.do, predictions.quart4.vary.do)
preds.quart4.do$lcl <- preds.quart4.do$fit - (1.96*preds.quart4.do$se.fit)
preds.quart4.do$ucl <- preds.quart4.do$fit + (1.96*preds.quart4.do$se.fit)
preds.quart4.do$time <- '6pm - midnight'

preds.do <-do.call("rbind", list(preds.quart1.do, preds.quart2.do, preds.quart3.do, preds.quart4.do))

#Plot

plot.do <- ggplot(preds.do, aes(x=standardized.do, y=fit, color=time, fill=time, group=time)) +
  #geom_hline(yintercept=1, color='grey',size=2)+ #horizontal line at y = 0 , reference point line of indifference
  geom_line(aes(y = fit), size = 2)+
  facet_zoom(ylim = c(0, 50))+
  scale_colour_manual(values=c("wheat3","skyblue4", "red", "black"))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl, fill=time),alpha=0.4, color=NA)+
  scale_fill_manual(values=c("lightseagreen","skyblue4", "pink", "grey"))+
  theme_classic()+
  labs(title = "Blue Ruin fish dissolved oxygen selection",
       subtitle = "Temperature held at 17°C")+
  xlab("Standardized dissolved oxygen (mg/L)") + ylab("Relative Probability of Selection")+
  theme(legend.title = element_blank()) 

ggsave(plot.do, filename = paste("results/figures/hab.select.mod.figures/br.quarterly.do.selection.temp.17C.png"), width = 16, height = 8, units = "cm")

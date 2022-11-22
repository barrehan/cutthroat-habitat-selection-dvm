#'11-14-2022
#'Fit conditional logistic regression to pooled blue ruin ibutton data
#'1-HOUR TIME INTERVAL

rm(list=ls())
library(survival)
library(ggplot2)
library(dplyr)

setwd("C:/Users/barrehan/GitHub/projects/cwa.habitat.selection")
br.ib<- read.csv("data/modif.data/hab.select.mod/br.ibutton.data/br.ibutton.pooled.csv")
# Make time ID columns factors --------------------------------------------

br.ib$hourID <-as.factor(br.ib$hourID)

do.clogit<-clogit(formula = case ~
                        standardized.do +
                        standardized.do:hourID+
                        strata(stratID),
                      data=br.ib)
summary(do.clogit) 


# Selection for DO --------------------------------------------------------

#predictions
time <- data.frame(hourID = seq(0,23,1))
do <- data.frame(standardized.do = seq(min(br.ib$standardized.do, na.rm = T),
                                                             max(br.ib$standardized.do, na.rm = T), 
                                                             0.1),
                                       stratID = 1)

pred.vals.vary.do <-merge(do, time)

pred.vals.vary.do$hourID <- as.factor(pred.vals.vary.do$hourID)

# get predictions from model using the values just created above
predictions.vary.do<-predict(do.clogit, newdata=pred.vals.vary.do, type='risk', se.fit=T)

preds.do<-cbind(pred.vals.vary.do, predictions.vary.do)
preds.do$lcl<-preds.do$fit - (1.96*preds.do$se.fit)
preds.do$ucl<-preds.do$fit + (1.96*preds.do$se.fit)

c24 <- c("dodgerblue2", "#E31A1C", "green4","#6A3D9A", "#FF7F00", "black", "gold1","skyblue2", "#FB9A99", "palegreen2", "#CAB2D6","#FDBF6F", "gray70", "khaki2", "maroon","orchid1", "deeppink1", "blue1", "steelblue4", "darkturquoise", "green1", "yellow4","yellow3", "darkorange4","brown")

plot.do <-ggplot(preds.do, aes(x=standardized.do, y=fit, color=hourID, fill=hourID, group=hourID)) +
  geom_line(aes(y = fit), size = 1.5)+
  scale_colour_manual(values=c24)+
  geom_ribbon(aes(ymin=lcl, ymax=ucl, fill=hourID),alpha=0.4, color=NA)+
  scale_fill_manual(values=c24)+
  theme_classic()+
  labs(title = "Blue Ruin fish DO selection")+
  xlab("Standardized DO mg/L") + ylab("Relative Probability of Selection")+
  theme(legend.title = element_blank())

ggsave(plot.do, filename = paste("results/figures/hab.select.mod.figures/br.hourly.do.selection.png"), width = 15, height = 8, units = "cm")

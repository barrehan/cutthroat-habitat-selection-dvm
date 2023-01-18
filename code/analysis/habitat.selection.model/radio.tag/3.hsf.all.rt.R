rm(list=ls())
library(survival)
library(ggplot2)
library(plotly)
library(ggpubr)
library(htmlwidgets)
library(reticulate)
library(viridis)

setwd("C:/Users/barrehan/GitHub/projects/cwa.habitat.selection")
rt<- read.csv("data/modif.data/hab.select.mod/br.radio.tag.data/br.rt.with.interpolated.do.temp.csv")

# Standardize temp and DO -------------------------------------------------

rt$standardized.do <- scale(rt$dissolved.oxygen)
rt$standardized.temp <-scale(rt$temperature)

# Logistic regression -----------------------------------------------------


mvmt.clogit<-clogit(formula = case ~
                      standardized.do+
                      standardized.temp+
                      standardized.do:standardized.temp+
                      strata(stratID),
                    data=rt)
summary(mvmt.clogit) 


# DF of case = 1 (used habitat) -------------------------------------------

used <- rt[rt$case == 1, ]

avail <- rt[rt$case == 0 ,]

# Span of temp/DO values during study ---------------------------------------

do.val <- seq(min(rt$standardized.do, na.rm = T), max(rt$standardized.do, na.rm = T), length.out = 100)
temp.val <- seq(min(rt$standardized.temp, na.rm = T), max(rt$standardized.temp, na.rm = T), length.out = 100)

vals<- matrix(NA, 100, 100)

summary(mvmt.clogit) 

for(i in 1:length(do.val)){
  for(j in 1:length(temp.val)){
    vals[i,j]<-(0.80409 *do.val[i] -2.10089*temp.val[j] + 0.86593*(do.val[i]*temp.val[j]))
  }
}

# find min/max prediction values for the z aspect of contour plot 

h1 <- max(vals)
l1 <- min(vals)

# Contour plots -----------------------------------------------------------

h1
l1

fig1.1 <- plot_ly(
  x = do.val,
  y = temp.val,
  z = t(vals),
  type = "contour",
  colorscale = 'YlOrRd',
  reversescale = T,
  autocontour = F, 
  contours = list(
    start = 16.75,
    end = -18.5,
    size = 1,
    showlabels = T))%>%
  layout(title = 'Blue Ruin radio tag habitat selection', xaxis = list(title = 'Standardized dissolved oxygen'), 
         yaxis = list(title = 'Standardized temperature'))%>%
  colorbar(title = "Selection probability") %>%
  add_trace(x = rt$standardized.do,
            y = rt$standardized.temp,
            type = 'scatter',
            mode = 'markers',
            color = I("gray6"),
            opacity = 0.75,
            marker = list(size = 3),
            name = 'Available habitat',
            showlegend = TRUE)

fig1.2 <- plot_ly(
  x = do.val,
  y = temp.val,
  z = t(vals),
  type = "contour",
  colorscale = 'YlOrRd',
  reversescale = T,
  autocontour = F, 
  contours = list(
    start = 16.75,
    end = -18.5,
    size = .5,
    showlabels = T))%>%
  colorbar(title = "Selection probability")%>%
  add_trace(x = used$standardized.do,
            y = used$standardized.temp,
            type = 'scatter',
            mode = "markers",
            color = I("chartreuse4"),
            opacity = .85,
            marker = list(size = 3),
            symbol = I('o'),
            name = "Selected habitat")%>%
  layout(title = 'Blue Ruin radio tag habitat selection', 
         xaxis = list(title = 'Standardized dissolved oxygen'), 
         yaxis = list(title = 'Standardized temperature'), 
         showlegend = T) 

fig1<-subplot(fig1.1, 
              fig1.2,
              nrows = 1,
              shareY = T,
              shareX = T)

#creating prediction data frame varying do, keeping temp constant at mean (0)

pred.vals <- data.frame(standardized.temp = 0, 
                                       standardized.do = seq(min(rt$standardized.do, na.rm = T),
                                                             max(rt$standardized.do, na.rm = T), 
                                                             0.1),
                                       stratID = 1)

# get predictions from model using the values just created above
predictions<-predict(mvmt.clogit, newdata=pred.vals, type='risk', se.fit=T)

preds.do<-cbind(pred.vals, predictions)
preds.do$lcl<-preds.do$fit - (1.96*preds.do$se.fit)
preds.do$ucl<-preds.do$fit + (1.96*preds.do$se.fit)

#Plot

plot.do <- ggplot(preds.do, aes(x=standardized.do, y=fit)) +
  geom_hline(yintercept=1, color='grey',size=2)+ #horizontal line at y = 0 , reference point line of indifference
  geom_line(aes(y = fit), size = 2)
  #facet_zoom(ylim = c(0, 3000))+
  scale_colour_manual(values= "wheat3")+
  geom_ribbon(aes(ymin=lcl, ymax=ucl, fill=time),alpha=0.4, color=NA)+
  scale_fill_manual(values=c("lightseagreen"))+
  theme_classic()+
  ggtitle("Blue ruin radio tag fish DO selection")+
  xlab("Standardized dissolved oxygen (mg/L)") + ylab("Relative Probability of Selection")+
  theme(legend.title = element_blank()) 


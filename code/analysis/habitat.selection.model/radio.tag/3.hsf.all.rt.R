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

rt$standardized.do <- as.numeric(scale(rt$dissolved.oxygen))
rt$standardized.temp <-as.numeric(scale(rt$temperature))

# Data frames for high and low DO periods ---------------------------------
# BR high DO 17:00 & 18:00
# BR low DO 05:00 & 06:00
rt$time <- hour(rt$date.time) #create time column

highDO <- rt[rt$time >= 17 & rt$time <19,] #pull times for high do period
lowDO <- rt[rt$time >= 5 & rt$time < 7,] #pull times for low do period

# Logistic regression -----------------------------------------------------

highDO.clogit<-clogit(formula = case ~
                       standardized.do+
                       standardized.temp+
                       standardized.do:standardized.temp+
                       strata(stratID),
                     data=highDO)
summary(highDO.clogit)

lowDO.clogit<-clogit(formula = case ~
                      standardized.do+
                      standardized.temp+
                      standardized.do:standardized.temp+
                      strata(stratID),
                    data=lowDO)
summary(lowDO.clogit) 

# Used versus available habitat for contour plot scatter points -----------

highDO.used <- highDO[highDO$case == 1, ]
highDO.avail <- highDO[highDO$case == 0 ,]

lowDO.used <-lowDO[lowDO$case ==1,]
lowDO.avail <- lowDO[lowDO$case == 0,]

###### HIGH DO TIME PERIOD ######
# Span of temp/DO values during study ---------------------------------------

highDO.do.val <- seq(min(highDO$standardized.do, na.rm = T), max(highDO$standardized.do, na.rm = T), length.out = 100)
highDO.temp.val <- seq(min(highDO$standardized.temp, na.rm = T), max(highDO$standardized.temp, na.rm = T), length.out = 100)

high.vals<- matrix(NA, 100, 100)

summary(highDO.clogit) 

for(i in 1:length(highDO.do.val)){
  for(j in 1:length(highDO.temp.val)){
    high.vals[i,j]<-(0.49815 *highDO.do.val[i] -1.80089*highDO.temp.val[j] +  0.33639*(highDO.do.val[i]*highDO.temp.val[j]))
  }
}

# find min/max prediction values for the z aspect of contour plot 

h1 <- max(high.vals)
l1 <- min(high.vals)

# Contour plots -----------------------------------------------------------

h1
l1

fig1.1 <- plot_ly(
  x = highDO.do.val,
  y = highDO.temp.val,
  z = t(high.vals),
  type = "contour",
  colorscale = 'YlOrRd',
  reversescale = T,
  autocontour = F, 
  contours = list(
    start = 3.5,
    end = -10.75,
    size = .5,
    showlabels = T))%>%
  layout(title = 'Blue Ruin radio tag habitat selection, 17:00 & 18:00', xaxis = list(title = 'Standardized dissolved oxygen'), 
         yaxis = list(title = 'Standardized temperature'))%>%
  colorbar(title = "Selection probability") %>%
  add_trace(x = highDO.avail$standardized.do,
            y = highDO.avail$standardized.temp,
            type = 'scatter',
            mode = 'markers',
            color = I("gray6"),
            opacity = 0.75,
            marker = list(size = 3),
            name = 'Available habitat',
            showlegend = TRUE)

fig1.2 <- plot_ly(
  x = highDO.do.val,
  y = highDO.temp.val,
  z = t(high.vals),
  type = "contour",
  colorscale = 'YlOrRd',
  reversescale = T,
  autocontour = F, 
  contours = list(
    start = 3.5,
    end = -10.75,
    size = .5,
    showlabels = T))%>%
  colorbar(title = "Selection probability")%>%
  add_trace(x = highDO.used$standardized.do,
            y = highDO.used$standardized.temp,
            type = 'scatter',
            mode = "markers",
            jitter = 0.7,
            color = I("chartreuse4"),
            opacity = .85,
            marker = list(size = 3),
            symbol = I('o'),
            name = "Selected habitat")%>%
  layout(title = 'Blue Ruin radio tag habitat selection, 17:00 & 18:00', 
         xaxis = list(title = 'Standardized dissolved oxygen'), 
         yaxis = list(title = 'Standardized temperature'), 
         showlegend = T) 

fig1<-subplot(fig1.1, 
              fig1.2,
              nrows = 1,
              shareY = T,
              shareX = T)

fig1.3 <- plot_ly(
  x = highDO.do.val,
  y = highDO.temp.val,
  z = t(high.vals),
  type = "contour",
  colorscale = 'YlOrRd',
  reversescale = T,
  autocontour = F, 
  contours = list(
    start = 3,
    end = -11,
    size = .5,
    showlabels = T))%>%
  colorbar(title = "Selection probability")%>%
  layout(title = 'Blue Ruin radio tag habitat selection, 17:00 - 19:00', 
         xaxis = list(title = 'Standardized dissolved oxygen'), 
         yaxis = list(title = 'Standardized temperature'), 
         showlegend = T) 

###### LOW DO TIME PERIOD ######
# Span of temp/DO values during study ---------------------------------------

lowDO.do.val <- seq(min(lowDO$standardized.do, na.rm = T), max(lowDO$standardized.do, na.rm = T), length.out = 100)
lowDO.temp.val <- seq(min(lowDO$standardized.temp, na.rm = T), max(lowDO$standardized.temp, na.rm = T), length.out = 100)

low.vals<- matrix(NA, 100, 100)

summary(lowDO.clogit) 

for(i in 1:length(lowDO.do.val)){
  for(j in 1:length(lowDO.temp.val)){
    low.vals[i,j]<-(2.54592 *lowDO.do.val[i] -1.85772*lowDO.temp.val[j] +2.41270*(lowDO.do.val[i]*lowDO.temp.val[j]))
  }
}

# find min/max prediction values for the z aspect of contour plot 

h2 <- max(low.vals)
l2 <- min(low.vals)

# Contour plots -----------------------------------------------------------

h2
l2

fig2.1 <- plot_ly(
  x = lowDO.do.val,
  y =lowDO.temp.val,
  z = t(low.vals),
  type = "contour",
  colorscale = 'YlOrRd',
  reversescale = T,
  autocontour = F, 
  contours = list(
    start = 7.25,
    end = -30.5,
    size = 1,
    showlabels = T))%>%
  layout(title = 'Blue Ruin radio tag habitat selection, 05:00 & 06:00', xaxis = list(title = 'Standardized dissolved oxygen'), 
         yaxis = list(title = 'Standardized temperature'))%>%
  colorbar(title = "Selection probability") %>%
  add_trace(x = lowDO.avail$standardized.do,
            y = lowDO.avail$standardized.temp,
            type = 'scatter',
            mode = 'markers',
            color = I("gray6"),
            opacity = 0.75,
            marker = list(size = 3),
            name = 'Available habitat',
            showlegend = TRUE)

fig2.2 <- plot_ly(
  x = lowDO.do.val,
  y = lowDO.temp.val,
  z = t(low.vals),
  type = "contour",
  colorscale = 'YlOrRd',
  reversescale = T,
  autocontour = F, 
  contours = list(
    start = 8,
    end = -30,
    size = 2,
    showlabels = T))%>%
  colorbar(title = "Selection probability")%>%
  add_trace(x = lowDO.used$standardized.do,
            y = lowDO.used$standardized.temp,
            type = 'scatter',
            mode = "markers",
            color = I("chartreuse4"),
            opacity = .85,
            marker = list(size = 3),
            symbol = I('o'),
            name = "Selected habitat")%>%
  layout(title = 'Blue Ruin radio tag habitat selection, 05:00 & 06:00', 
         xaxis = list(title = 'Standardized dissolved oxygen'), 
         yaxis = list(title = 'Standardized temperature'), 
         showlegend = T) 

fig2<-subplot(fig2.1, 
              fig2.2,
              nrows = 1,
              shareY = T,
              shareX = T)

fig2.3 <- plot_ly(
  x = lowDO.do.val,
  y = lowDO.temp.val,
  z = t(low.vals),
  type = "contour",
  colorscale = 'YlOrRd',
  reversescale = T,
  autocontour = F, 
  contours = list(
    start = 8,
    end = -30,
    size = 2,
    showlabels = T))%>%
  
  layout(title = 'Blue Ruin radio tag habitat selection, 05:00 - 07:00', 
         xaxis = list(title = 'Standardized dissolved oxygen'), 
         yaxis = list(title = 'Standardized temperature'), 
         showlegend = T)

saveWidget(fig1, "results/figures/hab.select.mod.figures/contour.plots/peak.low.do/rt.br.peak.do.html", selfcontained = T)

saveWidget(fig2, "results/figures/hab.select.mod.figures/contour.plots/peak.low.do/rt.br.low.do.html", selfcontained = T)

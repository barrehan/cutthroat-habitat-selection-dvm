# 12/14/2022 
# HSF3 hr interval to focus on highest/lowest DO periods
# BR midnight to ~3 looks like lowest DO period for fish, and highest
# DO period is roughly noon to 3 (looking at ibutton 
# figures), redoing analysis at 3 hour time segments

# questions:
# midnight - 3am temp not significant p val
# 6am-9am temp:do interaction not significant p val
# 9pm-midnight do not significant p val

rm(list=ls())
library(survival)
library(ggplot2)
library(plotly)
library(ggpubr)
library(htmlwidgets)
library(reticulate)
library(viridis)

setwd("C:/Users/barrehan/GitHub/projects/cwa.habitat.selection")
br.ib<- read.csv("data/modif.data/hab.select.mod/br.ibutton.data/br.ibutton.pooled.csv")

# Make three hr ID column factor ------------------------------------------

br.ib$threehrID <-as.factor(br.ib$threehrID)

# DFs for fish do/temp use at each interval -------------------------------

s1.ib <- br.ib[br.ib$case == 1 & br.ib$threehrID == 1,]
s2.ib <- br.ib[br.ib$case == 1 & br.ib$threehrID == 2,]
s3.ib <- br.ib[br.ib$case == 1 & br.ib$threehrID == 3,]
s4.ib <- br.ib[br.ib$case == 1 & br.ib$threehrID == 4,]
s5.ib <- br.ib[br.ib$case == 1 & br.ib$threehrID == 5,]
s6.ib <- br.ib[br.ib$case == 1 & br.ib$threehrID == 6,]
s7.ib <- br.ib[br.ib$case == 1 & br.ib$threehrID == 7,]
s8.ib <- br.ib[br.ib$case == 1 & br.ib$threehrID == 8,]

# Data frames by 3hr interval ---------------------------------------------

int1 <- br.ib[br.ib$threehrID == 1,]
int2 <- br.ib[br.ib$threehrID == 2,]
int3 <- br.ib[br.ib$threehrID == 3,]
int4 <- br.ib[br.ib$threehrID == 4,]
int5 <- br.ib[br.ib$threehrID == 5,]
int6 <- br.ib[br.ib$threehrID == 6,]
int7 <- br.ib[br.ib$threehrID == 7,]
int8 <- br.ib[br.ib$threehrID == 8,]

# Logistic regression sec 1, midnight - 3am ---------------------------

sec1.clogit<-clogit(formula = case ~
                        standardized.do+
                        standardized.temp+
                        standardized.do:standardized.temp+
                        strata(stratID),
                      data=int1)
summary(sec1.clogit) 

# Logistic regression sec 2, 3am - 6am  -------------------------------

sec2.clogit<-clogit(formula = case ~
                      standardized.do+
                      standardized.temp+
                      standardized.do:standardized.temp+
                      strata(stratID),
                    data=int2)
summary(sec2.clogit) 

# Logistic regression sec 3, 6am- 9am ---------------------------------

sec3.clogit<-clogit(formula = case ~
                      standardized.do+
                      standardized.temp+
                      standardized.do:standardized.temp+
                      strata(stratID),
                    data=int3)
summary(sec3.clogit)

# Logistic regression sec 4, 9am-noon ---------------------------------

sec4.clogit<-clogit(formula = case ~
                      standardized.do+
                      standardized.temp+
                      standardized.do:standardized.temp+
                      strata(stratID),
                    data=int4)
summary(sec4.clogit)

# Logistic regression sec 5, noon - 3pm --------------------------------

sec5.clogit<-clogit(formula = case ~
                      standardized.do+
                      standardized.temp+
                      standardized.do:standardized.temp+
                      strata(stratID),
                    data=int5)
summary(sec5.clogit)

# Logistic regression sec 6, 3pm-6pm --------------------------------

sec6.clogit<-clogit(formula = case ~
                      standardized.do+
                      standardized.temp+
                      standardized.do:standardized.temp+
                      strata(stratID),
                    data=int6)
summary(sec6.clogit)

# Logistic regression sec 7, 6pm-9pm --------------------------------

sec7.clogit<-clogit(formula = case ~
                      standardized.do+
                      standardized.temp+
                      standardized.do:standardized.temp+
                      strata(stratID),
                    data=int7)
summary(sec7.clogit)

# Logistic regression sec 8, 9pm-midnight ---------------------------

sec8.clogit<-clogit(formula = case ~
                      standardized.do+
                      standardized.temp+
                      standardized.do:standardized.temp+
                      strata(stratID),
                    data=int8)
summary(sec8.clogit)

#######Predictions########

# Span of temp/DO values during sec1 -----------------------------------------

s1.do<-seq(min(int1$standardized.do, na.rm = T), max(int1$standardized.do, na.rm = T), length.out = 100)
s1.temp <- seq(min(int1$standardized.temp, na.rm = T), max(int1$standardized.temp, na.rm = T), length.out = 100)

i1<- matrix(NA, 100, 100)

summary(sec1.clogit) 

for(i in 1:length(s1.do)){
  for(j in 1:length(s1.temp)){
    i1[i,j]<-(-2.20148*s1.do[i] - 0.04433*s1.temp[j] - 0.47892*(s1.do[i]*s1.temp[j]))
  }
}

# find min/max prediction values for the z aspect of contour plot 

h1 <- max(i1)
l1 <- min(i1)

# Span of temp/DO values during sec2 -----------------------------------------

s2.do<-seq(min(int2$standardized.do, na.rm = T), max(int2$standardized.do, na.rm = T), length.out = 100)
s2.temp <- seq(min(int2$standardized.temp, na.rm = T), max(int2$standardized.temp, na.rm = T), length.out = 100)

i2<- matrix(NA, 100, 100)

summary(sec2.clogit) 

for(i in 1:length(s2.do)){
  for(j in 1:length(s2.temp)){
    i2[i,j]<-(-3.69214*s2.do[i] +1.42355*s2.temp[j] +1.26480*(s2.do[i]*s2.temp[j]))
  }
}

# find min/max prediction values for the z aspect of contour plot 

h2 <- max(i2)
l2 <- min(i2)

# Span of temp/DO values during sec3 -----------------------------------------

s3.do<-seq(min(int3$standardized.do, na.rm = T), max(int3$standardized.do, na.rm = T), length.out = 100)
s3.temp <- seq(min(int3$standardized.temp, na.rm = T), max(int3$standardized.temp, na.rm = T), length.out = 100)

i3<- matrix(NA, 100, 100)

summary(sec3.clogit) 

for(i in 1:length(s3.do)){
  for(j in 1:length(s3.temp)){
    i3[i,j]<-(-1.20599*s3.do[i] - 0.25471*s3.temp[j] + 0.17633*(s3.do[i]*s3.temp[j]))
  }
}

# find min/max prediction values for the z aspect of contour plot 

h3 <- max(i3)
l3 <- min(i3)

# Span of temp/DO values during sec4 -----------------------------------------

s4.do<-seq(min(int4$standardized.do, na.rm = T), max(int4$standardized.do, na.rm = T), length.out = 100)
s4.temp <- seq(min(int4$standardized.temp, na.rm = T), max(int4$standardized.temp, na.rm = T), length.out = 100)

i4<- matrix(NA, 100, 100)

summary(sec4.clogit) 

for(i in 1:length(s4.do)){
  for(j in 1:length(s4.temp)){
    i4[i,j]<-(1.57543*s4.do[i] -1.46698*s4.temp[j] -0.94635*(s4.do[i]*s4.temp[j]))
  }
}

# find min/max prediction values for the z aspect of contour plot 

h4 <- max(i4)
l4 <- min(i4)

# Span of temp/DO values during sec5 -----------------------------------------

s5.do<-seq(min(int5$standardized.do, na.rm = T), max(int5$standardized.do, na.rm = T), length.out = 100)
s5.temp <- seq(min(int5$standardized.temp, na.rm = T), max(int5$standardized.temp, na.rm = T), length.out = 100)

i5<- matrix(NA, 100, 100)

summary(sec5.clogit) 

for(i in 1:length(s5.do)){
  for(j in 1:length(s5.temp)){
    i5[i,j]<-(2.34959*s5.do[i] -1.46819*s5.temp[j] -0.96166*(s5.do[i]*s5.temp[j]))
  }
}

# find min/max prediction values for the z aspect of contour plot 

h5 <- max(i5)
l5 <- min(i5)

# Span of temp/DO values during sec6 -----------------------------------------

s6.do<-seq(min(int6$standardized.do, na.rm = T), max(int6$standardized.do, na.rm = T), length.out = 100)
s6.temp <- seq(min(int6$standardized.temp, na.rm = T), max(int6$standardized.temp, na.rm = T), length.out = 100)

i6<- matrix(NA, 100, 100)

summary(sec6.clogit) 

for(i in 1:length(s6.do)){
  for(j in 1:length(s6.temp)){
    i6[i,j]<-(2.39453*s6.do[i] -1.93440*s6.temp[j] -0.81516*(s6.do[i]*s6.temp[j]))
  }
}

# find min/max prediction values for the z aspect of contour plot 

h6 <- max(i6)
l6 <- min(i6)

# Span of temp/DO values during sec7 -----------------------------------------

s7.do<-seq(min(int7$standardized.do, na.rm = T), max(int7$standardized.do, na.rm = T), length.out = 100)
s7.temp <- seq(min(int7$standardized.temp, na.rm = T), max(int7$standardized.temp, na.rm = T), length.out = 100)

i7<- matrix(NA, 100, 100)

summary(sec7.clogit) 

for(i in 1:length(s7.do)){
  for(j in 1:length(s7.temp)){
    i7[i,j]<-(1.14590*s7.do[i] -1.214083*s7.temp[j] -1.01464*(s7.do[i]*s7.temp[j]))
  }
}

# find min/max prediction values for the z aspect of contour plot 

h7 <- max(i7)
l7 <- min(i7)

# Span of temp/DO values during sec8 -----------------------------------------

s8.do<-seq(min(int8$standardized.do, na.rm = T), max(int8$standardized.do, na.rm = T), length.out = 100)
s8.temp <- seq(min(int8$standardized.temp, na.rm = T), max(int8$standardized.temp, na.rm = T), length.out = 100)

i8<- matrix(NA, 100, 100)

summary(sec8.clogit) 

for(i in 1:length(s8.do)){
  for(j in 1:length(s8.temp)){
    i8[i,j]<-(-0.09071*s8.do[i] -1.02313*s8.temp[j] -1.26341*(s8.do[i]*s8.temp[j]))
  }
}

# find min/max prediction values for the z aspect of contour plot 

h8 <- max(i8)
l8 <- min(i8)

# Contour plots -----------------------------------------------------------


# Midnight to 3am ---------------------------------------------------------


h1
l1

fig1.1 <- plot_ly(
  x = s1.do,
  y = s1.temp,
  z = t(i1),
  type = "contour",
  colorscale = 'YlOrRd',
  reversescale = T,
  autocontour = F, 
  contours = list(
    start = 4,
    end = -4.5,
    size = .5,
    showlabels = T))%>%
  layout(title = 'Blue Ruin midnight - 3am', xaxis = list(title = 'Standardized dissolved oxygen'), 
         yaxis = list(title = 'Standardized temperature'))%>%
  colorbar(title = "Selection probability") %>%
  add_trace(x = int1$standardized.do,
            y = int1$standardized.temp,
            type = 'scatter',
            mode = 'markers',
            color = I("gray6"),
            opacity = 0.75,
            marker = list(size = 3),
            name = 'Available habitat',
            showlegend = TRUE)

fig1.2 <- plot_ly(
  x = s1.do,
  y = s1.temp,
  z = t(i1),
  type = "contour",
  colorscale = 'YlOrRd',
  reversescale = T,
  autocontour = F, 
  contours = list(
    start = 4,
    end = -4.5,
    size = .5,
    showlabels = T))%>%
  colorbar(title = "Selection probability")%>%
  add_trace(x = s1.ib$standardized.do,
            y = s1.ib$standardized.temp,
            type = 'scatter',
            mode = "markers",
            color = I("chartreuse4"),
            opacity = .85,
            marker = list(size = 3),
            symbol = I('o'),
            name = "Selected habitat")%>%
  layout(title = 'Blue Ruin midnight - 3am', 
         xaxis = list(title = 'Standardized dissolved oxygen'), 
         yaxis = list(title = 'Standardized temperature'), 
         showlegend = T) 

f1<-subplot(fig1.1, fig1.2,
                 nrows = 1,
                 shareY = T,
                 shareX = T)


# 3am to 6am --------------------------------------------------------------

h2
l2

fig2.1 <- plot_ly(
  x = s2.do,
  y = s2.temp,
  z = t(i2),
  type = "contour",
  colorscale = 'YlOrRd',
  reversescale = T,
  autocontour = F, 
  contours = list(
    start = 5.5,
    end = -9,
    size = .5,
    showlabels = T))%>%
  layout(title = 'Blue Ruin 3am-6am', xaxis = list(title = 'Standardized dissolved oxygen'), 
         yaxis = list(title = 'Standardized temperature'))%>%
  colorbar(title = "Selection probability") %>%
  add_trace(x = int2$standardized.do,
            y = int2$standardized.temp,
            type = 'scatter',
            mode = 'markers',
            color = I("gray6"),
            opacity = 0.75,
            marker = list(size = 3),
            name = 'Available habitat',
            showlegend = TRUE)

fig2.2 <- plot_ly(
  x = s2.do,
  y = s2.temp,
  z = t(i2),
  type = "contour",
  colorscale = 'YlOrRd',
  reversescale = T,
  autocontour = F, 
  contours = list(
    start = 5.5,
    end = -9,
    size = .5,
    showlabels = T))%>%
  colorbar(title = "Selection probability")%>%
  add_trace(x = s2.ib$standardized.do,
            y = s2.ib$standardized.temp,
            type = 'scatter',
            mode = "markers",
            color = I("chartreuse4"),
            opacity = .85,
            marker = list(size = 3),
            symbol = I('o'),
            name = "Selected habitat")%>%
  layout(title = 'Blue Ruin 3am-6am', 
         xaxis = list(title = 'Standardized dissolved oxygen'), 
         yaxis = list(title = 'Standardized temperature'), 
         showlegend = T) 

f2<-subplot(fig2.1, fig2.2,
            nrows = 1,
            shareY = T,
            shareX = T)


# 6am to 9am --------------------------------------------------------------

h3
l3

fig3.1 <- plot_ly(
  x = s3.do,
  y = s3.temp,
  z = t(i3),
  type = "contour",
  colorscale = 'YlOrRd',
  reversescale = T,
  autocontour = F, 
  contours = list(
    start = 2.5,
    end = -1.5,
    size = .25,
    showlabels = T))%>%
  layout(title = 'Blue Ruin 6am-9am', xaxis = list(title = 'Standardized dissolved oxygen'), 
         yaxis = list(title = 'Standardized temperature'))%>%
  colorbar(title = "Selection probability") %>%
  add_trace(x = int3$standardized.do,
            y = int3$standardized.temp,
            type = 'scatter',
            mode = 'markers',
            color = I("gray6"),
            opacity = 0.75,
            marker = list(size = 3),
            name = 'Available habitat',
            showlegend = TRUE)

fig3.2 <- plot_ly(
  x = s3.do,
  y = s3.temp,
  z = t(i3),
  type = "contour",
  colorscale = 'YlOrRd',
  reversescale = T,
  autocontour = F, 
  contours = list(
    start = 2.5,
    end = -1.5,
    size = .25,
    showlabels = T))%>%
  colorbar(title = "Selection probability")%>%
  add_trace(x = s3.ib$standardized.do,
            y = s3.ib$standardized.temp,
            type = 'scatter',
            mode = "markers",
            color = I("chartreuse4"),
            opacity = .85,
            marker = list(size = 3),
            symbol = I('o'),
            name = "Selected habitat")%>%
  layout(title = 'Blue Ruin 6am-9am', 
         xaxis = list(title = 'Standardized dissolved oxygen'), 
         yaxis = list(title = 'Standardized temperature'), 
         showlegend = T) 

f3<-subplot(fig3.1, fig3.2,
            nrows = 1,
            shareY = T,
            shareX = T)

# 9am to noon --------------------------------------------------------------

h4
l4

fig4.1 <- plot_ly(
  x = s4.do,
  y = s4.temp,
  z = t(i4),
  type = "contour",
  colorscale = 'YlOrRd',
  reversescale = T,
  autocontour = F, 
  contours = list(
    start = 11,
    end = -5,
    size = .5,
    showlabels = T))%>%
  layout(title = 'Blue Ruin 9am-noon', xaxis = list(title = 'Standardized dissolved oxygen'), 
         yaxis = list(title = 'Standardized temperature'))%>%
  colorbar(title = "Selection probability") %>%
  add_trace(x = int4$standardized.do,
            y = int4$standardized.temp,
            type = 'scatter',
            mode = 'markers',
            color = I("gray6"),
            opacity = 0.75,
            marker = list(size = 3),
            name = 'Available habitat',
            showlegend = TRUE)

fig4.2 <- plot_ly(
  x = s4.do,
  y = s4.temp,
  z = t(i4),
  type = "contour",
  colorscale = 'YlOrRd',
  reversescale = T,
  autocontour = F, 
  contours = list(
    start = 11,
    end = -5,
    size = .5,
    showlabels = T))%>%
  colorbar(title = "Selection probability")%>%
  add_trace(x = s4.ib$standardized.do,
            y = s4.ib$standardized.temp,
            type = 'scatter',
            mode = "markers",
            color = I("chartreuse4"),
            opacity = .85,
            marker = list(size = 3),
            symbol = I('o'),
            name = "Selected habitat")%>%
  layout(title = 'Blue Ruin 9am-noon', 
         xaxis = list(title = 'Standardized dissolved oxygen'), 
         yaxis = list(title = 'Standardized temperature'), 
         showlegend = T) 

f4<-subplot(fig4.1, fig4.2,
            nrows = 1,
            shareY = T,
            shareX = T)

# noon to 3pm  --------------------------------------------------------------

h5
l5

fig5.1 <- plot_ly(
  x = s5.do,
  y = s5.temp,
  z = t(i5),
  type = "contour",
  colorscale = 'YlOrRd',
  reversescale = T,
  autocontour = F, 
  contours = list(
    start = 15,
    end = -7.5,
    size = .75,
    showlabels = T))%>%
  layout(title = 'Blue Ruin noon-3pm', xaxis = list(title = 'Standardized dissolved oxygen'), 
         yaxis = list(title = 'Standardized temperature'))%>%
  colorbar(title = "Selection probability") %>%
  add_trace(x = int5$standardized.do,
            y = int5$standardized.temp,
            type = 'scatter',
            mode = 'markers',
            color = I("gray6"),
            opacity = 0.75,
            marker = list(size = 3),
            name = 'Available habitat',
            showlegend = TRUE)

fig5.2 <- plot_ly(
  x = s5.do,
  y = s5.temp,
  z = t(i5),
  type = "contour",
  colorscale = 'YlOrRd',
  reversescale = T,
  autocontour = F, 
  contours = list(
    start = 15,
    end = -7.5,
    size = .75,
    showlabels = T))%>%
  colorbar(title = "Selection probability")%>%
  add_trace(x = s5.ib$standardized.do,
            y = s5.ib$standardized.temp,
            type = 'scatter',
            mode = "markers",
            color = I("chartreuse4"),
            opacity = .85,
            marker = list(size = 3),
            symbol = I('o'),
            name = "Selected habitat")%>%
  layout(title = 'Blue Ruin noon-3pm', 
         xaxis = list(title = 'Standardized dissolved oxygen'), 
         yaxis = list(title = 'Standardized temperature'), 
         showlegend = T) 

f5<-subplot(fig5.1, fig5.2,
            nrows = 1,
            shareY = T,
            shareX = T)

# 3pm to 6pm  --------------------------------------------------------------

h6
l6

fig6.1 <- plot_ly(
  x = s6.do,
  y = s6.temp,
  z = t(i6),
  type = "contour",
  colorscale = 'YlOrRd',
  reversescale = T,
  autocontour = F, 
  contours = list(
    start = 15.75,
    end = -7.75,
    size = .75,
    showlabels = T))%>%
  layout(title = 'Blue Ruin 3pm-6pm', xaxis = list(title = 'Standardized dissolved oxygen'), 
         yaxis = list(title = 'Standardized temperature'))%>%
  colorbar(title = "Selection probability") %>%
  add_trace(x = int6$standardized.do,
            y = int6$standardized.temp,
            type = 'scatter',
            mode = 'markers',
            color = I("gray6"),
            opacity = 0.75,
            marker = list(size = 3),
            name = 'Available habitat',
            showlegend = TRUE)

fig6.2 <- plot_ly(
  x = s6.do,
  y = s6.temp,
  z = t(i6),
  type = "contour",
  colorscale = 'YlOrRd',
  reversescale = T,
  autocontour = F, 
  contours = list(
    start = 15.75,
    end = -7.75,
    size = .75,
    showlabels = T))%>%
  colorbar(title = "Selection probability")%>%
  add_trace(x = s6.ib$standardized.do,
            y = s6.ib$standardized.temp,
            type = 'scatter',
            mode = "markers",
            color = I("chartreuse4"),
            opacity = .85,
            marker = list(size = 3),
            symbol = I('o'),
            name = "Selected habitat")%>%
  layout(title = 'Blue Ruin 3pm-6pm', 
         xaxis = list(title = 'Standardized dissolved oxygen'), 
         yaxis = list(title = 'Standardized temperature'), 
         showlegend = T) 

f6<-subplot(fig6.1, fig6.2,
            nrows = 1,
            shareY = T,
            shareX = T)

# 6pm to 9pm  --------------------------------------------------------------

h7
l7

fig7.1 <- plot_ly(
  x = s7.do,
  y = s7.temp,
  z = t(i7),
  type = "contour",
  colorscale = 'YlOrRd',
  reversescale = T,
  autocontour = F, 
  contours = list(
    start = 10.5,
    end = -11.25,
    size = .75,
    showlabels = T))%>%
  layout(title = 'Blue Ruin 6pm-9pm', xaxis = list(title = 'Standardized dissolved oxygen'), 
         yaxis = list(title = 'Standardized temperature'))%>%
  colorbar(title = "Selection probability") %>%
  add_trace(x = int7$standardized.do,
            y = int7$standardized.temp,
            type = 'scatter',
            mode = 'markers',
            color = I("gray6"),
            opacity = 0.75,
            marker = list(size = 3),
            name = 'Available habitat',
            showlegend = TRUE)

fig7.2 <- plot_ly(
  x = s7.do,
  y = s7.temp,
  z = t(i7),
  type = "contour",
  colorscale = 'YlOrRd',
  reversescale = T,
  autocontour = F, 
  contours = list(
    start = 10.5,
    end = -11.25,
    size = .75,
    showlabels = T))%>%
  colorbar(title = "Selection probability")%>%
  add_trace(x = s7.ib$standardized.do,
            y = s7.ib$standardized.temp,
            type = 'scatter',
            mode = "markers",
            color = I("chartreuse4"),
            opacity = .85,
            marker = list(size = 3),
            symbol = I('o'),
            name = "Selected habitat")%>%
  layout(title = 'Blue Ruin 6pm-9pm', 
         xaxis = list(title = 'Standardized dissolved oxygen'), 
         yaxis = list(title = 'Standardized temperature'), 
         showlegend = T) 

f7<-subplot(fig7.1, fig7.2,
            nrows = 1,
            shareY = T,
            shareX = T)

# 9pm to midnight  --------------------------------------------------------------

h8
l8

fig8.1 <- plot_ly(
  x = s8.do,
  y = s8.temp,
  z = t(i7),
  type = "contour",
  colorscale = 'YlOrRd',
  reversescale = T,
  autocontour = F, 
  contours = list(
    start = 6,
    end = -11.25,
    size = .75,
    showlabels = T))%>%
  layout(title = 'Blue Ruin 9pm-midnight', xaxis = list(title = 'Standardized dissolved oxygen'), 
         yaxis = list(title = 'Standardized temperature'))%>%
  colorbar(title = "Selection probability") %>%
  add_trace(x = int8$standardized.do,
            y = int8$standardized.temp,
            type = 'scatter',
            mode = 'markers',
            color = I("gray6"),
            opacity = 0.75,
            marker = list(size = 3),
            name = 'Available habitat',
            showlegend = TRUE)

fig8.2 <- plot_ly(
  x = s8.do,
  y = s8.temp,
  z = t(i8),
  type = "contour",
  colorscale = 'YlOrRd',
  reversescale = T,
  autocontour = F, 
  contours = list(
    start = 6,
    end = -11.25,
    size = .75,
    showlabels = T))%>%
  colorbar(title = "Selection probability")%>%
  add_trace(x = s8.ib$standardized.do,
            y = s8.ib$standardized.temp,
            type = 'scatter',
            mode = "markers",
            color = I("chartreuse4"),
            opacity = .85,
            marker = list(size = 3),
            symbol = I('o'),
            name = "Selected habitat")%>%
  layout(title = 'Blue Ruin 9pm-midnight', 
         xaxis = list(title = 'Standardized dissolved oxygen'), 
         yaxis = list(title = 'Standardized temperature'), 
         showlegend = T) 

f8<-subplot(fig8.1, fig8.2,
            nrows = 1,
            shareY = T,
            shareX = T)


# Calculate threshold values ----------------------------------------------
# at what temperature threshold value does DO become more important 
# than temperature?

# y = a + bT + cDO + dTDO
# y = a  bT + DO(c + dT)
# pull out c + dT
# c + dT = 0 
# T* = -c/d
# c = coef st.do. d = coef st.do:st.temp

# Calculate temp from standardized temp
# st.T* = (T* - mean)/sd
# (st.T*sd) + mean = T*


# Calculate mean and sd ---------------------------------------------------

br.sd <- sd(br.ib$temperature)
br.mean <- mean(br.ib$temperature)

# Temp threshold midnight-3am ---------------------------------------------

t1<-summary(sec1.clogit)$coefficients["standardized.temp","coef"]
int1<-summary(sec1.clogit)$coefficients["standardized.do:standardized.temp", "coef"]

s1.temp.thresh.std <- -t1/int1
s1.temp.thresh <-  (s1.temp.thresh.std * br.sd) + br.mean

# Temp threshold 3am-6am --------------------------------------------------

t2<-summary(sec2.clogit)$coefficients["standardized.temp","coef"]
int2<-summary(sec2.clogit)$coefficients["standardized.do:standardized.temp", "coef"]

s2.temp.thresh.std <- -t2/int2
s2.temp.thresh <-  (s2.temp.thresh.std * br.sd) + br.mean


# Temp threshold 6am-9am --------------------------------------------------

t3<-summary(sec3.clogit)$coefficients["standardized.temp","coef"]
int3<-summary(sec3.clogit)$coefficients["standardized.do:standardized.temp", "coef"]

s3.temp.thresh.std <- -t3/int3
s3.temp.thresh <-  (s3.temp.thresh.std * br.sd) + br.mean

# Temp threshold 9am-noon --------------------------------------------------

t4<-summary(sec4.clogit)$coefficients["standardized.temp","coef"]
int4<-summary(sec4.clogit)$coefficients["standardized.do:standardized.temp", "coef"]

s4.temp.thresh.std <- -t4/int4
s4.temp.thresh <-  (s4.temp.thresh.std * br.sd) + br.mean

# Temp threshold noon-3pm -------------------------------------------------

t5<-summary(sec5.clogit)$coefficients["standardized.temp","coef"]
int5<-summary(sec5.clogit)$coefficients["standardized.do:standardized.temp", "coef"]

s5.temp.thresh.std <- -t5/int5
s5.temp.thresh <-  (s5.temp.thresh.std * br.sd) + br.mean


# Temp threshold 3pm-6pm --------------------------------------------------

t6<-summary(sec6.clogit)$coefficients["standardized.temp","coef"]
int6<-summary(sec6.clogit)$coefficients["standardized.do:standardized.temp", "coef"]

s6.temp.thresh.std <- -t6/int6
s6.temp.thresh <-  (s6.temp.thresh.std * br.sd) + br.mean


# Temp threshold 6pm-9pm --------------------------------------------------

t7<-summary(sec7.clogit)$coefficients["standardized.temp","coef"]
int7<-summary(sec7.clogit)$coefficients["standardized.do:standardized.temp", "coef"]

s7.temp.thresh.std <- -t7/int7
s7.temp.thresh <- (s7.temp.thresh.std * br.sd) + br.mean

# Temp threshold 9pm-midnight ---------------------------------------------

t8<-summary(sec8.clogit)$coefficients["standardized.temp","coef"]
int8<-summary(sec8.clogit)$coefficients["standardized.do:standardized.temp", "coef"]

s8.temp.thresh.std <- -t8/int8
s8.temp.thresh <-  (s8.temp.thresh.std * br.sd) + br.mean

# saveWidget(f1, "results/figures/hab.select.mod.figures/contour.plots/br.midnight.3am.3hr.int.html", selfcontained = T)
# saveWidget(f2, "results/figures/hab.select.mod.figures/contour.plots/br.3am.6am.3hr.int.html", selfcontained = T)
# saveWidget(f3, "results/figures/hab.select.mod.figures/contour.plots/br.6am.9am.3hr.int.html", selfcontained = T)
# saveWidget(f4, "results/figures/hab.select.mod.figures/contour.plots/br.9am.noon.3hr.int.html", selfcontained = T)
# saveWidget(f5, "results/figures/hab.select.mod.figures/contour.plots/br.noon.3pm.3hr.int.html", selfcontained = T)
# saveWidget(f6, "results/figures/hab.select.mod.figures/contour.plots/br.3pm.6pm.3hr.int.html", selfcontained = T)
# saveWidget(f7, "results/figures/hab.select.mod.figures/contour.plots/br.6pm.9pm.3hr.int.html", selfcontained = T)
# saveWidget(f8, "results/figures/hab.select.mod.figures/contour.plots/br.9pm.midnight.3hr.int.html", selfcontained = T)
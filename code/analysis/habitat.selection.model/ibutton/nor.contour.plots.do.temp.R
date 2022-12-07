rm(list=ls())
library(survival)
library(ggplot2)
library(ggforce)
library(plotly)

setwd("C:/Users/barrehan/GitHub/projects/cwa.habitat.selection")
nor.ib<- read.csv("data/modif.data/hab.select.mod/norwood.ibutton.data/norwood.ibutton.pooled.csv")
# Make time ID columns factors --------------------------------------------

nor.ib$quarterID <-as.factor(nor.ib$quarterID)

# DFs for fish do/temp use at each quarter interval -----------------------

q1.ib <- nor.ib[nor.ib$case == 1 & nor.ib$quarterID == 1,]
q2.ib <- nor.ib[nor.ib$case == 1 & nor.ib$quarterID == 2,]
q3.ib <- nor.ib[nor.ib$case == 1 & nor.ib$quarterID == 3,]
q4.ib <- nor.ib[nor.ib$case == 1 & nor.ib$quarterID == 4,]

# Data frames by quarter interval ------------------------------------------

quart1 <- nor.ib[nor.ib$quarterID == 1,]
quart2 <- nor.ib[nor.ib$quarterID == 2,]
quart3 <- nor.ib[nor.ib$quarterID == 3,]
quart4 <- nor.ib[nor.ib$quarterID == 4,]

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

# Span of temp/DO values during quart1 -----------------------------------------

q1.do<-seq(min(quart1$standardized.do, na.rm = T), max(quart1$standardized.do, na.rm = T), length.out = 100)
q1.temp <- seq(min(quart1$standardized.temp, na.rm = T), max(quart1$standardized.temp, na.rm = T), length.out = 100)

q1<- matrix(NA, 100, 100)

summary(quart1.clogit) 

for(i in 1:length(q1.do)){
  for(j in 1:length(q1.temp)){
    q1[i,j]<-(-0.999792*q1.do[i]-5.080579*q1.temp[j]-4.553744*(q1.do[i]*q1.temp[j]))
  }
}

fig1<- plot_ly(
  x = q1.do,
  y = q1.temp,
  z = t(q1),
  type = "contour")%>%
  layout(title = 'Midnight - 6am',xaxis = list(title = 'Standardized dissolved oxygen'), 
         yaxis = list(title = 'Standardized temperature'))

fig1.1<-add_trace(
  fig1,
  type = 'scatter',
  mode = "markers",
  x = q1.ib$standardized.do,
  y = q1.ib$standardized.temp,
  color = "sienna2",
  name = "fish"
)

f1<-subplot(fig1, fig1.1,
            nrows = 1,
            shareY = T,
            shareX = T)

# Span of temp/DO values during quart2 -----------------------------------------

q2.do<-seq(min(quart2$standardized.do, na.rm = T), max(quart2$standardized.do, na.rm = T), length.out = 100)
q2.temp <- seq(min(quart2$standardized.temp, na.rm = T), max(quart2$standardized.temp, na.rm = T), length.out = 100)

q2<- matrix(NA, 100, 100)

summary(quart2.clogit) 

for(i in 1:length(q2.do)){
  for(j in 1:length(q2.temp)){
    q2[i,j] <- (-4.613802*q2.do[i]-0.957018*q2.temp[j]-3.896848*(q2.do[i]*q2.temp[j]))
  }
}

fig2<- plot_ly(
  x = q2.do,
  y = q2.temp,
  z = t(q2),
  type = "contour")%>%
  layout(title = '6am - noon',xaxis = list(title = 'Standardized dissolved oxygen'), 
         yaxis = list(title = 'Standardized temperature'))

fig2.1<-add_trace(
  fig2,
  type = 'scatter',
  mode = "markers",
  x = q2.ib$standardized.do,
  y = q2.ib$standardized.temp,
  color = "sienna2",
  name = "fish"
)

f2<-subplot(fig2, fig2.1,
            nrows = 1,
            shareY = T,
            shareX = T)

 # Span of temp/DO values during quart3 -----------------------------------------
 # Noon - 6pm
 q3.do<-seq(min(quart3$standardized.do, na.rm = T), max(quart3$standardized.do, na.rm = T), length.out = 100)
 q3.temp <- seq(min(quart3$standardized.temp, na.rm = T), max(quart3$standardized.temp, na.rm = T), length.out = 100)
 
 q3<- matrix(NA, 100, 100)
 
 summary(quart3.clogit) 
 
 for(i in 1:length(q3.do)){
   for(j in 1:length(q3.temp)){
     q3[i,j]<- (5.055630*q3.do[i]-5.406024*q3.temp[j]+1.224542*(q3.do[i]*q3.temp[j]))
   }
 }
 
 fig3<- plot_ly(
   x = q3.do,
   y = q3.temp,
   z = t(q3),
   type = "contour")%>%
   layout(title = 'Noon - 6pm',xaxis = list(title = 'Standardized dissolved oxygen'), 
          yaxis = list(title = 'Standardized temperature'))
 
 fig3.1<-add_trace(
   fig3,
   type = 'scatter',
   mode = "markers",
   x = q3.ib$standardized.do,
   y = q3.ib$standardized.temp,
   color = "sienna2",
   name = "fish"
 )
 
 f3<-subplot(fig3, fig3.1,
             nrows = 1,
             shareY = T,
             shareX = T)
 
 # Span of temp/DO values during quart4 -----------------------------------------
 # 6pm - midnight
 
 q4.do<-seq(min(quart4$standardized.do, na.rm = T), max(quart4$standardized.do, na.rm = T), length.out = 100)
 q4.temp <- seq(min(quart4$standardized.temp, na.rm = T), max(quart4$standardized.temp, na.rm = T), length.out = 100)
 
 q4<- matrix(NA, 100, 100)
 
 summary(quart4.clogit) 
 
 for(i in 1:length(q4.do)){
   for(j in 1:length(q4.temp)){
     q4[i,j]<-(-0.54802*q4.do[i]-1.54043*q4.temp[j]-0.67624*(q4.do[i]*q4.temp[j]))
   }
 }
 
 fig4<- plot_ly(
   x = q4.do,
   y = q4.temp,
   z = t(q4),
   type = "contour")%>%
   layout(title = '6pm - midnight',xaxis = list(title = 'Standardized dissolved oxygen'), 
          yaxis = list(title = 'Standardized temperature'))
 
 fig4.1<-add_trace(
   fig4,
   type = 'scatter',
   mode = "markers",
   x = q4.ib$standardized.do,
   y = q4.ib$standardized.temp,
   color = "sienna2",
   name = "fish"
 )
 
 f4<-subplot(fig4, fig4.1,
            nrows = 1,
            shareY = T,
            shareX = T)
 
rm(list=ls())
library(survival)
library(ggplot2)
library(plotly)
library(ggpubr)
library(htmlwidgets)
library(reticulate)
library(viridis)

setwd("C:/Users/barrehan/GitHub/projects/cwa.habitat.selection")
nor.ib<- read.csv("data/modif.data/hab.select.mod/norwood.ibutton.data/norwood.ibutton.pooled.csv")

# DFs for fish do/temp use at each interval -------------------------------

high.ib <- nor.ib[nor.ib$case == 1 & nor.ib$highlowDO.2hours == "high",]
low.ib <- nor.ib[nor.ib$case == 1 & nor.ib$highlowDO.2hours == "low",]

# Data frames by high/low interval ----------------------------------------

high.int <- nor.ib[nor.ib$highlowDO.2hours == "high",]
low.int <- nor.ib[nor.ib$highlowDO.2hours == "low",]

# Logistic regression high DO, 16:00-20:00 --------------------------------

high.clogit<-clogit(formula = case ~
                      standardized.do+
                      standardized.temp+
                      standardized.do:standardized.temp+
                      strata(stratID),
                    data=high.int)
summary(high.clogit) 

# Logistic regression low Do, 2:00-6:00  --------------------------------------

low.clogit<-clogit(formula = case ~
                     standardized.do+
                     standardized.temp+
                     standardized.do:standardized.temp+
                     strata(stratID),
                   data=low.int)
summary(low.clogit) 

# Span of temp/DO values during high DO ---------------------------------------

high.do<-seq(min(high.int$standardized.do, na.rm = T), max(high.int$standardized.do, na.rm = T), length.out = 100)
high.temp <- seq(min(high.int$standardized.temp, na.rm = T), max(high.int$standardized.temp, na.rm = T), length.out = 100)

high<- matrix(NA, 100, 100)

summary(high.clogit) 

for(i in 1:length(high.do)){
  for(j in 1:length(high.temp)){
    high[i,j]<-(3.43612 *high.do[i] -2.67138*high.temp[j] -0.63660*(high.do[i]*high.temp[j]))
  }
}

# find min/max prediction values for the z aspect of contour plot 

h1 <- max(high)
l1 <- min(high)

# Span of temp/DO values during low DO -----------------------------------------

low.do<-seq(min(low.int$standardized.do, na.rm = T), max(low.int$standardized.do, na.rm = T), length.out = 100)
low.temp <- seq(min(low.int$standardized.temp, na.rm = T), max(low.int$standardized.temp, na.rm = T), length.out = 100)

low<- matrix(NA, 100, 100)

summary(low.clogit) 

for(i in 1:length(low.do)){
  for(j in 1:length(low.temp)){
    low[i,j]<-(-6.257252*low.do[i] +0.783814*low.temp[j] -3.681803*(low.do[i]*low.temp[j]))
  }
}

# find min/max prediction values for the z aspect of contour plot 

h2 <- max(low)
l2 <- min(low)

# Contour plots -----------------------------------------------------------
# High DO ----------------------------------------------------------------

h1
l1

fig1.1 <- plot_ly(
  x = high.do,
  y = high.temp,
     showlabels = T)%>%
  add_trace(x = high.int$standardized.do,
            y = high.int$standardized.temp,
            type = 'scatter',
            mode = 'markers',
            color = I("gray6"),
            opacity = 0.75,
            marker = list(size = 3),
            name = 'Available habitat',
            showlegend = TRUE)%>%
  layout(title = 'Norwood peak DO 18:00-20:00', 
         xaxis = list(title = 'Standardized dissolved oxygen', range = c(.15,2.25)), 
         yaxis = list(title = 'Standardized temperature', range = c(-1.35,2.75)),
         layout.title = FALSE,
         showlegend = T)

fig1.2 <- plot_ly(
  x = high.do,
  y = high.temp,
    showlabels = T)%>%
  add_trace(x = high.ib$standardized.do,
            y = high.ib$standardized.temp,
            type = 'scatter',
            mode = "markers",
            color = I("chartreuse4"),
            opacity = .85,
            marker = list(size = 3),
            symbol = I('o'),
            name = "Selected habitat")%>%
  layout(title = 'Norwood peak DO 18:00-20:00', 
         xaxis = list(title = 'Standardized dissolved oxygen', range = c(.15,2.25)), 
         yaxis = list(title = 'Standardized temperature', range = c(-1.35,2.75)),
         axis = FALSE,
         showlegend = T)

fig1.3 <- plot_ly(
  x = high.do,
  y = high.temp,
  z = t(high),
  type = "contour",
  colorscale = 'YlOrRd',
  reversescale = T,
  autocontour = F, 
  contours = list(
    start = 13.5,
    end = -7,
    size = .5,
    showlabels = T))%>%
  colorbar(title = "Selection probability")%>%
  layout(title = 'Norwood peak DO 18:00-20:00', 
         xaxis = list(title = 'Standardized dissolved oxygen', range = c(.15,2.25)), 
         yaxis = list(title = 'Standardized temperature', range = c(-1.35,2.75)),
         showlegend = T,
         xaxis=list(showgrid=FALSE),
         yaxis = list(showgrid = FALSE))

fig1<-subplot(fig1.1,
              fig1.2, 
              fig1.3,
              nrows = 1,
              shareY = T,
              shareX = T)

# fig1.1 <- plot_ly(
#   x = high.do,
#   y = high.temp,
#   z = t(high),
#   type = "contour",
#   colorscale = 'YlOrRd',
#   reversescale = T,
#   autocontour = F, 
#   contours = list(
#     start = 13.5,
#     end = -7,
#     size = .5,
#     showlabels = T))%>%
#   colorbar(title = "Selection probability") %>%
#   add_trace(x = high.int$standardized.do,
#             y = high.int$standardized.temp,
#             type = 'scatter',
#             mode = 'markers',
#             color = I("gray6"),
#             opacity = 0.75,
#             marker = list(size = 3),
#             name = 'Available habitat',
#             showlegend = TRUE)%>%
#   layout(title = 'Norwood peak DO 18:00-20:00', 
#          xaxis = list(title = 'Standardized dissolved oxygen', range = c(.15,2.25)), 
#          yaxis = list(title = 'Standardized temperature', range = c(-1.35,2.75)),
#          showlegend = T)
# 
# fig1.2 <- plot_ly(
#   x = high.do,
#   y = high.temp,
#   z = t(high),
#   type = "contour",
#   colorscale = 'YlOrRd',
#   reversescale = T,
#   autocontour = F, 
#   contours = list(
#     start = 13.5,
#     end = -7,
#     size = .5,
#     showlabels = T))%>%
#   colorbar(title = "Selection probability")%>%
#   add_trace(x = high.ib$standardized.do,
#             y = high.ib$standardized.temp,
#             type = 'scatter',
#             mode = "markers",
#             color = I("chartreuse4"),
#             opacity = .85,
#             marker = list(size = 3),
#             symbol = I('o'),
#             name = "Selected habitat")%>%
#   layout(title = 'Norwood peak DO 18:00-20:00', 
#          xaxis = list(title = 'Standardized dissolved oxygen', range = c(.15,2.25)), 
#          yaxis = list(title = 'Standardized temperature', range = c(-1.35,2.75)),
#          showlegend = T)
# 
# fig1.3 <- plot_ly(
#   x = high.do,
#   y = high.temp,
#   z = t(high),
#   type = "contour",
#   colorscale = 'YlOrRd',
#   reversescale = T,
#   autocontour = F, 
#   contours = list(
#     start = 13.5,
#     end = -7,
#     size = .5,
#     showlabels = T))%>%
#   colorbar(title = "Selection probability")%>%
#   layout(title = 'Norwood peak DO 18:00-20:00', 
#          xaxis = list(title = 'Standardized dissolved oxygen', range = c(.15,2.25)), 
#          yaxis = list(title = 'Standardized temperature', range = c(-1.35,2.75)),
#          showlegend = T)
# 
# fig1<-subplot(fig1.3,
#               fig1.1, 
#               fig1.2,
#               nrows = 1,
#               shareY = T,
#               shareX = T)
# 
# fig <- fig1%>%
#   layout(
#     annotations = list(
#         x = 0.16,
#         y = 1,
#         font = list(size = 10),
#         xref = "paper",
#         yref = "paper",
#         xanchor = "center",
#         yanchor = "bottom",
#         showarrow = FALSE
#       ))

# Low DO ----------------------------------------------------------------

h2
l2



fig2.1 <- plot_ly(
  x = low.do,
  y = low.temp,
  showlabels = T)%>%
  add_trace(x = low.int$standardized.do,
            y = low.int$standardized.temp,
            type = 'scatter',
            mode = 'markers',
            color = I("gray6"),
            opacity = 0.75,
            marker = list(size = 3),
            name = 'Available habitat',
            showlegend = TRUE)%>%
  layout(title = 'Norwood low DO 6:00-8:00', 
         xaxis = list(title = 'Standardized dissolved oxygen', range = c(-1.95, .6)), 
         yaxis = list(title = 'Standardized temperature', range = c(-1.4, 1.25)),
         showlegend = T)

fig2.2 <- plot_ly(
  x = low.do,
  y = low.temp,
  showlabels = T)%>%
  add_trace(x = low.ib$standardized.do,
            y = low.ib$standardized.temp,
            type = 'scatter',
            mode = "markers",
            color = I("chartreuse4"),
            opacity = .85,
            marker = list(size = 3),
            symbol = I('o'),
            name = "Selected habitat")%>%
  layout(title = 'Norwood low DO 6:00-8:00', 
         xaxis = list(title = 'Standardized dissolved oxygen', range = c(-1.95, .6)), 
         yaxis = list(title = 'Standardized temperature', range = c(-1.4, 1.25)),
         showlegend = T)

fig2.3 <- plot_ly(
  x = low.do,
  y = low.temp,
  z = t(low),
  type = "contour",
  colorscale = 'YlOrRd',
  reversescale = T,
  autocontour = F, 
  contours = list(
    start = 22,
    end = -6.25,
    size = .75,
    showlabels = T))%>%
  colorbar(title = "Selection probability")%>%
  layout(title = 'Norwood low DO 6:00-8:00', 
         xaxis = list(title = 'Standardized dissolved oxygen', range = c(-1.95, .6)), 
         yaxis = list(title = 'Standardized temperature', range = c(-1.4, 1.25)),
         showlegend = T)

fig2<-subplot(fig2.1, fig2.2, fig2.3, 
              nrows = 1,
              shareY = T,
              shareX = T)

# fig2.1 <- plot_ly(
#   x = low.do,
#   y = low.temp,
#   z = t(low),
#   type = "contour",
#   colorscale = 'YlOrRd',
#   reversescale = T,
#   autocontour = F, 
#   contours = list(
#     start = 22,
#     end = -6.25,
#     size = .75,
#     showlabels = T))%>%
#   colorbar(title = "Selection probability") %>%
#   add_trace(x = low.int$standardized.do,
#             y = low.int$standardized.temp,
#             type = 'scatter',
#             mode = 'markers',
#             color = I("gray6"),
#             opacity = 0.75,
#             marker = list(size = 3),
#             name = 'Available habitat',
#             showlegend = TRUE)%>%
#   layout(title = 'Norwood low DO 6:00-8:00', 
#          xaxis = list(title = 'Standardized dissolved oxygen', range = c(-1.95, .6)), 
#          yaxis = list(title = 'Standardized temperature', range = c(-1.4, 1.25)),
#          showlegend = T)
# 
# fig2.2 <- plot_ly(
#   x = low.do,
#   y = low.temp,
#   z = t(low),
#   type = "contour",
#   colorscale = 'YlOrRd',
#   reversescale = T,
#   autocontour = F, 
#   contours = list(
#     start = 22,
#     end = -6.25,
#     size = .75,
#     showlabels = T))%>%
#   colorbar(title = "Selection probability")%>%
#   add_trace(x = low.ib$standardized.do,
#             y = low.ib$standardized.temp,
#             type = 'scatter',
#             mode = "markers",
#             color = I("chartreuse4"),
#             opacity = .85,
#             marker = list(size = 3),
#             symbol = I('o'),
#             name = "Selected habitat")%>%
#   layout(title = 'Norwood low DO 6:00-8:00', 
#          xaxis = list(title = 'Standardized dissolved oxygen', range = c(-1.95, .6)), 
#          yaxis = list(title = 'Standardized temperature', range = c(-1.4, 1.25)),
#          showlegend = T)
# 
# fig2.3 <- plot_ly(
#   x = low.do,
#   y = low.temp,
#   z = t(low),
#   type = "contour",
#   colorscale = 'YlOrRd',
#   reversescale = T,
#   autocontour = F, 
#   contours = list(
#     start = 22,
#     end = -6.25,
#     size = .75,
#     showlabels = T))%>%
#   colorbar(title = "Selection probability")%>%
#   layout(title = 'Norwood low DO 6:00-8:00', 
#          xaxis = list(title = 'Standardized dissolved oxygen', range = c(-1.95, .6)), 
#          yaxis = list(title = 'Standardized temperature', range = c(-1.4, 1.25)),
#          showlegend = T)
# 
# 
# fig2<-subplot(fig2.3, fig2.1, fig2.2, 
#             nrows = 1,
#             shareY = T,
#             shareX = T)
#   

# Threshold high DO --------------------------------------------------------

nor.sd <- sd(nor.ib$temperature)
nor.mean <- mean(nor.ib$temperature)

# at what temperature threshold value does DO become more important 
# than temperature?

summary(high.clogit) 
# y = a + bT + cDO + dTDO
# y = a  bT + DO(c + dT)
# pull out c + dT
# c + dT = 0 
# T* = -c/d
# c = coef st.do. d = coef st.do:st.temp

high.coef.do<-summary(high.clogit)$coefficients[1, 1]
high.coef.int<-summary(high.clogit)$coefficients[3,1]

high.temp.thresh.std <- (-(high.coef.do/high.coef.int))

# Calculate temp from standardized temp
# st.T* = (T* - mean)/sd
# (st.T*sd) + mean = T*

high.temp.thresh <-  (high.temp.thresh.std * nor.sd) + nor.mean

# Threshold low DO --------------------------------------------------------

summary(low.clogit) 
# y = a + bT + cDO + dTDO
# y = a  bT + DO(c + dT)
# pull out c + dT
# c + dT = 0 
# T* = -c/d
# c = coef st.do. d = coef st.do:st.temp

low.coef.do<-summary(low.clogit)$coefficients[1, 1]
low.coef.int<-summary(low.clogit)$coefficients[3,1]

low.temp.thresh.std <- (-(low.coef.do/low.coef.int))

# Calculate temp from standardized temp
# st.T* = (T* - mean)/sd
# (st.T*sd) + mean = T*

low.temp.thresh <- (low.temp.thresh.std * nor.sd) + nor.mean

# Save contour widgets ----------------------------------------------------

saveWidget(fig1, "results/figures/hab.select.mod.figures/contour.plots/peak.low.do/nor.peak.do.html", selfcontained = T)

saveWidget(fig2, "results/figures/hab.select.mod.figures/contour.plots/peak.low.do/nor.low.do.html", selfcontained = T)


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
    q1[i,j]<-exp(-0.999792*q1.do[i]-5.080579*q1.temp[j]-4.553744*(q1.do[i]*q1.temp[j]))
  }
}

# find min/max prediction values for the z aspect of contour plot 

max(q1)
min(q1)

fig1<- plot_ly(
  x = q1.do,
  y = q1.temp,
  z = t(q1),
  type = "contour",
  colorscale = 'YlOrRd',
  reversescale = T, 
  autocontour = F, 
  contours = list(
    start = 2903283,
    end = 0,
    size = 100000,
    showlabels = T))%>%
  layout(title = 'Norwood midnight - 6am', xaxis = list(title = 'Standardized dissolved oxygen'), 
         yaxis = list(title = 'Standardized temperature', showlegend = F)) %>%
  colorbar(title = "Selection probability")

fig2 <- plot_ly(
  x = q1.do,
  y = q1.temp,
  z = t(q1),
  type = "contour",
  colorscale = 'YlOrRd',
  reversescale = T,
  autocontour = F, 
  contours = list(
    start = 2903283,
    end = 0,
    size = 100000,
    showlabels = T))%>%
  layout(title = 'Norwood midnight - 6am',xaxis = list(title = 'Standardized dissolved oxygen'), 
         yaxis = list(title = 'Standardized temperature'))%>%
  colorbar(title = "Selection probability") %>%
  add_trace(x = quart1$standardized.do,
            y = quart1$standardized.temp,
            type = 'scatter',
            mode = 'markers',
            color = I("gray6"),
            opacity = 0.75,
            marker = list(size = 3),
            name = 'Available habitat',
            showlegend = TRUE)

fig3 <- plot_ly(
  x = q1.do,
  y = q1.temp,
  z = t(q1),
  type = "contour",
  colorscale = 'YlOrRd',
  reversescale = T,
  autocontour = F, 
  contours = list(
    start = 2903283,
    end = 0,
    size = 100000,
    showlabels = T))%>%
  colorbar(title = "Selection probability")%>%
  add_trace(x = q1.ib$standardized.do,
            y = q1.ib$standardized.temp,
            type = 'scatter',
            mode = "markers",
            color = I("brown4"),
            marker = list(size = 3),
            symbol = I('o'),
            name = "Selected habitat")%>%
  layout(title = 'Norwood midnight - 6am', 
         xaxis = list(title = 'Standardized dissolved oxygen'), 
         yaxis = list(title = 'Standardized temperature'), 
         showlegend = T) 

# Create zoom plots -------------------------------------------------------

fig1.zoom<- plot_ly(
  x = q1.do,
  y = q1.temp,
  z = t(q1),
  type = "contour",
  colorscale = 'YlOrRd',
  reversescale = T, 
  autocontour = F, 
  contours = list(
    start = 20,
    end = 0,
    size = 2,
    showlabels = T))%>%
  layout(title = 'Norwood midnight - 6am', 
         xaxis = list(range = c(-1.8, 0), title = 'Standardized dissolved oxygen'), 
         yaxis = list(range = c(-1.5, 0.75), title = 'Standardized temperature'), 
         showlegend = T)  %>%
  colorbar(title = "Selection probability")

fig2.zoom <- plot_ly(
  x = q1.do,
  y = q1.temp,
  z = t(q1),
  type = "contour",
  colorscale = 'YlOrRd',
  reversescale = T,
  autocontour = F, 
  contours = list(
    start = 20,
    end = 0,
    size = 2,
    showlabels = T))%>%
  layout(title = 'Norwood midnight - 6am', 
         xaxis = list(range = c(-1.8, 0), title = 'Standardized dissolved oxygen'), 
         yaxis = list(range = c(-1.5, 0.75), title = 'Standardized temperature'), 
         showlegend = T) %>%
  colorbar(title = "Selection probability") %>%
  add_trace(x = quart1$standardized.do,
            y = quart1$standardized.temp,
            type = 'scatter',
            opacity = 0.75,
            mode = 'markers',
            color = I("gray6"),
            marker = list(size = 3),
            name = 'Available habitat',
            showlegend = TRUE)

fig3.zoom <- plot_ly(
  x = q1.do,
  y = q1.temp,
  z = t(q1),
  type = "contour",
  coloring = 'heatmap',
  colorscale = 'YlOrRd',
  reversescale = T,
  autocontour = F, 
  contours = list(
    start = 20,
    end = 0,
    size = 2,
    showlabels = T))%>%
  colorbar(title = "Selection probability")%>%
  add_trace(x = q1.ib$standardized.do,
            y = q1.ib$standardized.temp,
            opacity = 0.75,
            type = 'scatter',
            mode = "markers",
            color = I("chartreuse4"),
            opacity = .85,
            marker = list(size = 3),
            symbol = I('o'),
            name = "Selected habitat")%>%
  layout(title = 'Norwood midnight - 6am', 
         xaxis = list(range = c(-1.8, 0), title = 'Standardized dissolved oxygen'), 
         yaxis = list(range = c(-1.5, 0.75), title = 'Standardized temperature'), 
         showlegend = T)


f<- subplot(fig1, fig2, fig3,
            nrows = 1,
            shareY = T,
            shareX = T)

f.zoom <- subplot(fig1.zoom, fig2.zoom, fig3.zoom,
                  nrows = 1,
                  shareX = T,
                  shareY = T)

saveWidget(f, "results/figures/hab.select.mod.figures/contour.plots/norwood.midnight.6am.og.range.html", selfcontained = T)

saveWidget(f.zoom, "results/figures/hab.select.mod.figures/contour.plots/norwood.midnight.6am.zoom.html", selfcontained = T)

saveWidget(fig3.zoom, "results/figures/hab.select.mod.figures/contour.plots/norwood.midnight.6am.selected.hab.html", selfcontained = T)

# Threshold quart1 --------------------------------------------------------

# at what temperature threshold value does DO become more important 
# than temperature?

summary(quart1.clogit)
# y = a + bT + cDO + dTDO
# y = a  bT + DO(c + dT)
# pull out c + dT
# c + dT = 0 
# T* = -c/d
# c = coef st.do. d = coef st.do:st.temp

q1.temp.thresh.std <- (-(-0.999792/-4.553744))

# Calculate temp from standardized temp
# st.T* = (T* - mean)/sd
# (st.T*sd) + mean = T*

nor.sd <- sd(nor.ib$temperature)
nor.mean <- mean(nor.ib$temperature)

q1.temp.thresh <-  (q1.temp.thresh.std * nor.sd) + nor.mean

# Span of temp/DO values during quart2 -----------------------------------------

q2.do<-seq(min(quart2$standardized.do, na.rm = T), max(quart2$standardized.do, na.rm = T), length.out = 100)
q2.temp <- seq(min(quart2$standardized.temp, na.rm = T), max(quart2$standardized.temp, na.rm = T), length.out = 100)

q2<- matrix(NA, 100, 100)

summary(quart2.clogit) 

for(i in 1:length(q2.do)){
  for(j in 1:length(q2.temp)){
    q2[i,j] <- exp(-4.613802*q2.do[i]-0.957018*q2.temp[j]-3.896848*(q2.do[i]*q2.temp[j]))
  }
}

max(q2)
min(q2)

fig1<- plot_ly(
  x = q2.do,
  y = q2.temp,
  z = t(q2),
  type = "contour",
  colorscale = 'YlOrRd',
  reversescale = T, 
  autocontour = F, 
  contours = list(
    start = 183479583,
    end = 0,
    size = 500000,
    showlabels = T))%>%
  layout(title = 'Norwood 6am - noon', xaxis = list(title = 'Standardized dissolved oxygen'), 
         yaxis = list(title = 'Standardized temperature', showlegend = F)) %>%
  colorbar(title = "Selection probability")

fig2 <- plot_ly(
  x = q2.do,
  y = q2.temp,
  z = t(q2),
  type = "contour",
  colorscale = 'YlOrRd',
  reversescale = T,
  autocontour = F, 
  contours = list(
    start = 183479583,
    end = 0,
    size = 500000,
    showlabels = T))%>%
  layout(title = 'Norwood 6am - noon', xaxis = list(title = 'Standardized dissolved oxygen'), 
         yaxis = list(title = 'Standardized temperature', showlegend = T)) %>%
  colorbar(title = "Selection probability") %>%
  add_trace(x = quart2$standardized.do,
            y = quart2$standardized.temp,
            type = 'scatter',
            mode = 'markers',
            color = I("gray6"),
            opacity = 0.75,
            marker = list(size = 3),
            name = 'Available habitat',
            showlegend = TRUE)

fig3 <- plot_ly(
  x = q2.do,
  y = q2.temp,
  z = t(q2),
  type = "contour",
  colorscale = 'YlOrRd',
  reversescale = T,
  autocontour = F, 
  contours = list(
    start = 183479583,
    end = 0,
    size = 500000,
    showlabels=T))%>%
  colorbar(title = "Selection probability")%>%
  add_trace(x = q2.ib$standardized.do,
            y = q2.ib$standardized.temp,
            type = 'scatter',
            mode = "markers",
            color = I("brown4"),
            marker = list(size = 3),
            symbol = I('o'),
            name = "Selected habitat")%>%
  layout(title = 'Norwood 6am-noon', 
         xaxis = list(title = 'Standardized dissolved oxygen'), 
         yaxis = list(title = 'Standardized temperature'), 
         showlegend = T) 

# Create zoomed plots ------------------------------------------------------

fig1.zoom<- plot_ly(
  x = q2.do,
  y = q2.temp,
  z = t(q2),
  type = "contour",
  colorscale = 'YlOrRd',
  reversescale = T, 
  autocontour = F, 
  contours = list(
    start = 100,
    end = 5,
    size = 5,
    showlabels = T))%>%
  layout(title = 'Norwood 6am - noon', xaxis = list(range = c(-1.8, 0), title ='Standardized dissolved oxygen'), 
         yaxis = list(range = c(-1.5, 0.55), title = 'Standardized temperature', showlegend = T)) %>%
  colorbar(title = "Selection probability")

fig2.zoom <- plot_ly(
  x = q2.do,
  y = q2.temp,
  z = t(q2),
  type = "contour",
  colorscale = 'YlOrRd',
  reversescale = T,
  autocontour = F, 
  contours = list(
    start = 100,
    end = 5,
    size = 5,
    showlabels = T))%>%
  layout(title = 'Norwood 6am - noon', xaxis = list(range = c(-1.8, 0), title ='Standardized dissolved oxygen'), 
         yaxis = list(range = c(-1.5, 0.55), title = 'Standardized temperature', showlegend = T)) %>%
  colorbar(title = "Selection probability") %>%
  add_trace(x = quart2$standardized.do,
            y = quart2$standardized.temp,
            type = 'scatter',
            mode = 'markers',
            color = I("gray6"),
            opacity = 0.75,
            marker = list(size = 3),
            name = 'Available habitat',
            showlegend = TRUE)

fig3.zoom <- plot_ly(
  x = q2.do,
  y = q2.temp,
  z = t(q2),
  type = "contour",
  colorscale = 'YlOrRd',
  reversescale = T,
  autocontour = F, 
  contours = list(
    start = 100,
    end = 5,
    size = 5,
    showlabels = T))%>%
  colorbar(title = "Selection probability")%>%
  add_trace(x = q2.ib$standardized.do,
            y = q2.ib$standardized.temp,
            type = 'scatter',
            mode = "markers",
            color = I("chartreuse4"),
            opacity = 0.75,
            marker = list(size = 3),
            symbol = I('o'),
            name = "Selected habitat")%>%
  layout(title = 'Norwood 6am - noon', xaxis = list(range = c(-1.8, 0), title ='Standardized dissolved oxygen'), 
         yaxis = list(range = c(-1.5, 0.55), title = 'Standardized temperature', showlegend = T)) 

f<- subplot(fig1, fig2, fig3,
            nrows = 1,
            shareY = T,
            shareX = T)

f.zoom<- subplot(fig1.zoom, fig2.zoom, fig3.zoom,
                 nrows = 1,
                 shareY = T,
                 shareX = T)

saveWidget(f, "results/figures/hab.select.mod.figures/contour.plots/norwood.6am.noon.og.range.html", selfcontained = T)

saveWidget(f.zoom, "results/figures/hab.select.mod.figures/contour.plots/norwood.6am.noon.zoom.html", selfcontained = T)

saveWidget(fig3.zoom, "results/figures/hab.select.mod.figures/contour.plots/norwood.6am.noon.selected.hab.html", selfcontained = T)

# Threshold quart2 --------------------------------------------------------

# at what temperature threshold value does DO become more important 
# than temperature?

summary(quart2.clogit)
# y = a + bT + cDO + dTDO
# y = a  bT + DO(c + dT)
# pull out c + dT
# c + dT = 0 
# T* = -c/d
# c = coef st.do. d = coef st.do:st.temp

q2.temp.thresh.std <- (-(-4.613802/-3.896848))

# Calculate temp from standardized temp
# st.T* = (T* - mean)/sd
# (st.T*sd) + mean = T*

q2.temp.thresh <-  (q2.temp.thresh.std * nor.sd) + nor.mean

 # Span of temp/DO values during quart3 -----------------------------------------
 # Noon - 6pm
 q3.do<-seq(min(quart3$standardized.do, na.rm = T), max(quart3$standardized.do, na.rm = T), length.out = 100)
 q3.temp <- seq(min(quart3$standardized.temp, na.rm = T), max(quart3$standardized.temp, na.rm = T), length.out = 100)
 
 q3<- matrix(NA, 100, 100)
 
 summary(quart3.clogit)
 
 for(i in 1:length(q3.do)){
   for(j in 1:length(q3.temp)){
     q3[i,j]<-exp(5.055630*q3.do[i]-5.406024*q3.temp[j]+1.224542*(q3.do[i]*q3.temp[j]))
   }
 }
 
 max(q3)
 min(q3)
 
 fig1<- plot_ly(
   x = q3.do,
   y = q3.temp,
   z = t(q3),
   type = "contour",
   colorscale = 'YlOrRd',
   reversescale = T, 
   autocontour = F, 
   contours = list(
     start = 4825437,
     end = 0,
     size = 500000,
     showlabels = T))%>%
   layout(title = 'Norwood noon-6pm',xaxis = list(title = 'Standardized dissolved oxygen'), 
          yaxis = list(title = 'Standardized temperature', showlegend = F)) %>%
   colorbar(title = "Selection probability")
 
 fig2 <- plot_ly(
   x = q3.do,
   y = q3.temp,
   z = t(q3),
   type = "contour",
   colorscale = 'YlOrRd',
   reversescale = T,
   autocontour = F, 
   contours = list(
     start = 4825437,
     end = 0,
     size = 500000,
     showlabels = T))%>%
   layout(title = 'Norwood noon-6pm',xaxis = list(title = 'Standardized dissolved oxygen'), 
          yaxis = list(title = 'Standardized temperature')) %>%
   colorbar(title = "Selection probability") %>%
   add_trace(x = quart3$standardized.do,
             y = quart3$standardized.temp,
             type = 'scatter',
             mode = 'markers',
             color = I("gray6"),
             opacity = 0.75,
             marker = list(size = 3),
             name = 'Available habitat',
             showlegend = TRUE)
 
 fig3 <- plot_ly(
   x = q3.do,
   y = q3.temp,
   z = t(q3),
   type = "contour",
   colorscale = 'YlOrRd',
   reversescale = T,
   autocontour = F, 
   contours = list(
     start = 4825437,
     end = 0,
     size = 500000,
     showlabels = T))%>%
   colorbar(title = "Selection probability")%>%
   add_trace(x = q3.ib$standardized.do,
             y = q3.ib$standardized.temp,
             type = 'scatter',
             mode = "markers",
             color = I("brown4"),
             marker = list(size = 3),
             symbol = I('o'),
             name = "Selected habitat")%>%
   layout(title = 'Norwood noon-6pm', 
          xaxis = list(title = 'Standardized dissolved oxygen'), 
          yaxis = list(title = 'Standardized temperature'), 
          showlegend = T) 
 
 
 # Create zoom plots -------------------------------------------------------
 
 fig1.zoom<- plot_ly(
   x = q3.do,
   y = q3.temp,
   z = t(q3),
   type = "contour",
   colorscale = 'YlOrRd',
   reversescale = T,
   autocontour = F, 
   contours = list(
     start = 1000,
     end = 10,
     size = 50,
     showlabels = T))%>%
   layout(title = 'Norwood noon-6pm', xaxis = list(range = c(-1.1, 1), title = 'Standardized dissolved oxygen'), 
          yaxis = list(range = c(-1.5, 0.5), title = 'Standardized temperature'))  %>%
   colorbar(title = "Selection probability")
 
 fig2.zoom <- plot_ly(
   x = q3.do,
   y = q3.temp,
   z = t(q3),
   type = "contour",
   colorscale = 'YlOrRd',
   reversescale = T,
   autocontour = F, 
   contours = list(
     start = 1000,
     end = 10,
     size = 50,
     showlabels = T))%>%
   layout(title = 'Norwood noon-6pm', xaxis = list(range = c(-0.5, 1), title = 'Standardized dissolved oxygen'), 
          yaxis = list(range = c(-1.1, 0.5), title = 'Standardized temperature'))  %>%
   colorbar(title = "Selection probability") %>%
   add_trace(x = quart3$standardized.do,
             y = quart3$standardized.temp,
             type = 'scatter',
             mode = 'markers',
             color = I("gray6"),
             opacity = 0.75,
             marker = list(size = 3),
             name = 'Available habitat',
             showlegend = TRUE)
 
 fig3.zoom <- plot_ly(
   x = q3.do,
   y = q3.temp,
   z = t(q3),
   type = "contour",
   colorscale = 'YlOrRd',
   reversescale = T,
   autocontour = F, 
   contours = list(
     start = 1000,
     end = 10,
     size = 50,
     showlabels = T))%>%
   colorbar(title = "Selection probability")%>%
   add_trace(x = q3.ib$standardized.do,
             y = q3.ib$standardized.temp,
             type = 'scatter',
             mode = "markers",
             color = I("chartreuse4"),
             opacity = 0.75,
             marker = list(size = 3),
             symbol = I('o'),
             name = "Selected habitat")%>%
   layout(title = 'Norwood noon-6pm', xaxis = list(range = c(-0.5, 1), title = 'Standardized dissolved oxygen'), 
          yaxis = list(range = c(-1.1, 0.5), title = 'Standardized temperature')) 
 
 f<- subplot(fig1, fig2, fig3,
             nrows = 1,
             shareY = T,
             shareX = T)
 
 f.zoom<- subplot(fig1.zoom, fig2.zoom, fig3.zoom,
                  nrows = 1,
                  shareY = T,
                  shareX = T)
 
 saveWidget(f, "results/figures/hab.select.mod.figures/contour.plots/norwood.noon.6pm.og.range.html", selfcontained = T)
 
 saveWidget(f.zoom, "results/figures/hab.select.mod.figures/contour.plots/norwood.noon.6pm.zoom.html", selfcontained = T)
 
 saveWidget(fig3.zoom, "results/figures/hab.select.mod.figures/contour.plots/norwood.noon.6pm.selected.hab.html", selfcontained = T)
 
 # Threshold quart3 --------------------------------------------------------
 
 # at what temperature threshold value does DO become more important 
 # than temperature?
 
 summary(quart3.clogit)
 # y = a + bT + cDO + dTDO
 # y = a  bT + DO(c + dT)
 # pull out c + dT
 # c + dT = 0 
 # T* = -c/d
 # c = coef st.do. d = coef st.do:st.temp
 
 q3.temp.thresh.std <- (-(5.055630/1.224542))
 
 # Calculate temp from standardized temp
 # st.T* = (T* - mean)/sd
 # (st.T*sd) + mean = T*
 
 q3.temp.thresh <-  (q3.temp.thresh.std * nor.sd) + nor.mean
 # Span of temp/DO values during quart4 -----------------------------------------
 # 6pm - midnight
 
 q4.do<-seq(min(quart4$standardized.do, na.rm = T), max(quart4$standardized.do, na.rm = T), length.out = 100)
 q4.temp <- seq(min(quart4$standardized.temp, na.rm = T), max(quart4$standardized.temp, na.rm = T), length.out = 100)
 
 q4<- matrix(NA, 100, 100)
 
 summary(quart4.clogit) 
 
 for(i in 1:length(q4.do)){
   for(j in 1:length(q4.temp)){
     q4[i,j]<- exp(-0.54802*q4.do[i]-1.54043*q4.temp[j]-0.67624*(q4.do[i]*q4.temp[j]))
   }
 }
 
 max(q4)
 min(q4)
 
 fig1<- plot_ly(
   x = q4.do,
   y = q4.temp,
   z = t(q4),
   type = "contour",
   colorscale = 'YlOrRd',
   reversescale = T, 
   autocontour = F, 
   contours = list(
     start = 30,
     end = 0,
     size = 2,
     showlabels = T))%>%
   layout(title = 'Norwood 6pm-midnight',xaxis = list(title = 'Standardized dissolved oxygen'), 
          yaxis = list(title = 'Standardized temperature', showlegend = F)) %>%
   colorbar(title = "Selection probability")
 
 fig2 <- plot_ly(
   x = q4.do,
   y = q4.temp,
   z = t(q4),
   type = "contour",
   colorscale = 'YlOrRd',
   reversescale = T,
   autocontour = F, 
   contours = list(
     start = 30,
     end = 0,
     size = 2,
     showlabels = T))%>%
   layout(title = 'Norwood 6pm-midnight',xaxis = list(title = 'Standardized dissolved oxygen'), 
          yaxis = list(title = 'Standardized temperature')) %>%
   colorbar(title = "Selection probability") %>%
   add_trace(x = quart4$standardized.do,
             y = quart4$standardized.temp,
             type = 'scatter',
             mode = 'markers',
             color = I("gray6"),
             marker = list(size = 3),
             name = 'Available habitat',
             showlegend = TRUE)
 
 
 fig3 <- plot_ly(
   x = q4.do,
   y = q4.temp,
   z = t(q4),
   type = "contour",
   colorscale = 'YlOrRd',
   reversescale = T,
   autocontour = F, 
   contours = list(
     start = 30,
     end = 0,
     size = 2,
     showlabels = T))%>%
   colorbar(title = "Selection probability")%>%
   add_trace(x = q4.ib$standardized.do,
             y = q4.ib$standardized.temp,
             type = 'scatter',
             mode = "markers",
             color = I("brown4"),
             marker = list(size = 3),
             symbol = I('o'),
             name = "Selected habitat")%>%
   layout(title = 'Norwood 6pm-midnight', 
          xaxis = list(title = 'Standardized dissolved oxygen'), 
          yaxis = list(title = 'Standardized temperature'), 
          showlegend = T) 
 
 # Create zoom plots -------------------------------------------------------
 
 fig1.zoom<- plot_ly(
   x = q4.do,
   y = q4.temp,
   z = t(q4),
   type = "contour",
   colorscale = 'YlOrRd',
   reversescale = T, 
   autocontour = F, 
   contours = list(
     start = 10,
     end = 0,
     size = .5,
     showlabels = T))%>%
   layout(title = 'Norwood 6pm-midnight', 
          xaxis = list(range = c(-1.15, 1.4), title = 'Standardized dissolved oxygen'), 
          yaxis = list(range = c(-1.5, 1),title = 'Standardized temperature'), 
          showlegend = T) %>%
   colorbar(title = "Selection probability")
 
 fig2.zoom <- plot_ly(
   x = q4.do,
   y = q4.temp,
   z = t(q4),
   type = "contour",
   colorscale = 'YlOrRd',
   reversescale = T,
   autocontour = F, 
   contours = list(
     start = 10,
     end = 0,
     size = .5,
     showlabels = T))%>%
   layout(title = 'Norwood 6pm-midnight', 
          xaxis = list(range = c(-1.15, 1.4), title = 'Standardized dissolved oxygen'), 
          yaxis = list(range = c(-1.5, 1),title = 'Standardized temperature'), 
          showlegend = T) %>%
   add_trace(x = quart4$standardized.do,
             y = quart4$standardized.temp,
             type = 'scatter',
             mode = 'markers',
             color = I("gray6"),
             opacity = 0.75,
             marker = list(size = 3),
             name = 'Available habitat',
             showlegend = TRUE)
 
 
 fig3.zoom <- plot_ly(
   x = q4.do,
   y = q4.temp,
   z = t(q4),
   type = "contour",
   colorscale = 'YlOrRd',
   reversescale = T,
   autocontour = F, 
   contours = list(
     start = 10,
     end = 0,
     size = .5,
     showlabels = T))%>%
   colorbar(title = "Selection probability")%>%
   add_trace(x = q4.ib$standardized.do,
             y = q4.ib$standardized.temp,
             type = 'scatter',
             mode = "markers",
             color = I("chartreuse4"),
             opacity = 0.75,
             marker = list(size = 3),
             symbol = I('o'),
             name = "Selected habitat")%>%
   layout(title = 'Norwood 6pm-midnight', 
          xaxis = list(range = c(-1.15, 1.4), title = 'Standardized dissolved oxygen'), 
          yaxis = list(range = c(-1.5, 1),title = 'Standardized temperature'), 
          showlegend = T) 
 
 f<- subplot(fig1, fig2, fig3,
             nrows = 1,
             shareY = T,
             shareX = T)
 
 f.zoom<- subplot(fig1.zoom, fig2.zoom, fig3.zoom,
                  nrows = 1,
                  shareY = T,
                  shareX = T)
 
 saveWidget(f, "results/figures/hab.select.mod.figures/contour.plots/norwood.6pm.midnight.og.range.html", selfcontained = T)
 
 saveWidget(f.zoom, "results/figures/hab.select.mod.figures/contour.plots/norwood.6pm.midnight.zoom.html", selfcontained = T)
 
 saveWidget(fig3.zoom, "results/figures/hab.select.mod.figures/contour.plots/norwood.6pm.midnight.selected.hab.html", selfcontained = T)
 
 # Threshold quart4 --------------------------------------------------------
 
 # at what temperature threshold value does DO become more important 
 # than temperature?
 
 summary(quart4.clogit)
 # y = a + bT + cDO + dTDO
 # y = a  bT + DO(c + dT)
 # pull out c + dT
 # c + dT = 0 
 # T* = -c/d
 # c = coef st.do. d = coef st.do:st.temp
 
 q4.temp.thresh.std <- (-(-0.54802/-0.67624))
 
 # Calculate temp from standardized temp
 # st.T* = (T* - mean)/sd
 # (st.T*sd) + mean = T*
 
 q4.temp.thresh <-  (q4.temp.thresh.std * nor.sd) + nor.mean
 
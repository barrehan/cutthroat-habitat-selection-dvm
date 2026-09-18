library(readr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(RColorBrewer)
library(viridis)
library(grid)
library(gridExtra)
library(data.table)
library(ggpubr)


 df <- read_csv("data/modif.data/ibutton/all.ibuttons.depth.do.interpolation.resolvable.only.csv") %>%
  rename(
    fish.depth = fish_depth,
    temperature = ibutton.temp
  ) %>%
  mutate(
    date.time = parse_date_time (
      date.time,
      orders = "ymd HMS",
      tz = "America/Los_Angeles"
    ),
    date = as.Date(date.time),
    hour_numeric =
      hour(date.time) +
      minute(date.time) / 60
  ) %>%
  filter(
    date >= as.Date("2021-07-29"),
    date <= as.Date("2021-08-05")
  )

nor <- df[df$site == "norwood",]
br <- df[df$site == "blue.ruin",]

tag.id<- unique(nor$ibutton)

range(nor$temperature, na.rm = TRUE)
range(nor$fish.depth, na.rm = TRUE)

for(i in tag.id){
  
  p1 <- ggplot(
    data = subset(nor, ibutton == i),
    aes(date.time, fish.depth)
  ) +
    geom_point(aes(colour = hour_numeric), size = 2) +
    scale_color_gradientn(
      name = "Hour",
      colours = c(
        "#291919", "#532A34", "#7C5467",
        "#878195", "#AEB2B7", "#D4D9DD"
      )
    ) +
    scale_y_reverse(
      limits = c(1.6, 0.2),
      breaks = seq(0, 1.6, by = 0.4)
    ) +
    xlab("Date") +
    ylab("Fish depth (m)") +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(colour = "black"),
      legend.key = element_rect(fill = "white"),
      text = element_text(size = 18, family = "serif"),
      legend.title = element_text(family = "serif"),
      legend.text = element_text(family = "serif")
    )
  
  p2 <- ggplot(
    data = subset(nor, ibutton == i),
    aes(date.time, temperature)
  ) +
    geom_point(aes(colour = hour_numeric), 
               size = 2) +
    scale_y_continuous(
      limits = c(10, 22),
      breaks = seq(10, 22, by = 2)
    ) +
    scale_color_gradientn(
      name = "Hour",
      colours = c(
        "#291919", "#532A34", "#7C5467",
        "#878195", "#AEB2B7", "#D4D9DD"
      )
    ) +
    scale_x_datetime(
      date_labels = "%b %d",
      date_breaks = "2 days"
    ) +
    xlab("Date") +
    ylab("Fish temperature (\u00B0C)") +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(colour = "black"),
      legend.key = element_rect(fill = "white"),
      text = element_text(size = 18, family = "serif"),
      legend.title = element_text(family = "serif"),
      legend.text = element_text(family = "serif")
    )
  
  figure <- ggarrange(
    p2,
    p1,
    ncol = 1,
    nrow = 2,
    heights = c(1, 1.1),
    common.legend = TRUE,
    legend = "right"
  )
  
  fig <- annotate_figure(
    figure,
    top = text_grob(
      paste0("Norwood iButton #", i),
      family = "serif",
      face = "bold",
      size = 20
    )
  )
  
  ggsave(
    filename = paste0(
      "final figures/supplemental figures/norwood.ibutton.",
      i,
      ".do.temp.depth.jpg"
    ),
    plot = fig,
    width = 12,
    height = 8,
    units = "in"
  )
  
}

range(br$temperature, na.rm = TRUE)
range(br$fish.depth, na.rm = TRUE)

tag.id<- unique(br$ibutton)

for(i in tag.id){
  
  p1 <- ggplot(
    data = subset(br, ibutton == i),
    aes(date.time, fish.depth)
  ) +
    geom_point(aes(colour = hour_numeric), size = 2) +
    scale_color_gradientn(
      name = "Hour",
      colours = c(
        "#291919", "#532A34", "#7C5467",
        "#878195", "#AEB2B7", "#D4D9DD"
      )
    ) +
    scale_y_reverse(
      limits = c(1.6, 0),
      breaks = seq(0, 1.6, by = 0.4)
    ) +
    xlab("Date") +
    ylab("Fish depth (m)") +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(colour = "black"),
      legend.key = element_rect(fill = "white"),
      text = element_text(size = 18, family = "serif"),
      legend.title = element_text(family = "serif"),
      legend.text = element_text(family = "serif")
    )
  
  p2 <- ggplot(
    data = subset(br, ibutton == i),
    aes(date.time, temperature)
  ) +
    geom_point(aes(colour = hour_numeric), 
               size = 2) +
    scale_y_continuous(
      limits = c(10, 20),
      breaks = seq(10, 20, by = 2)
    ) +
    scale_color_gradientn(
      name = "Hour",
      colours = c(
        "#291919", "#532A34", "#7C5467",
        "#878195", "#AEB2B7", "#D4D9DD"
      )
    ) +
    scale_x_datetime(
      date_labels = "%b %d",
      date_breaks = "2 days"
    ) +
    xlab("Date") +
    ylab("Fish temperature (\u00B0C)") +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(colour = "black"),
      legend.key = element_rect(fill = "white"),
      text = element_text(size = 18, family = "serif"),
      legend.title = element_text(family = "serif"),
      legend.text = element_text(family = "serif")
    )
  
  figure <- ggarrange(
    p2,
    p1,
    ncol = 1,
    nrow = 2,
    heights = c(1, 1.1),
    common.legend = TRUE,
    legend = "right"
  )
  
  fig <- annotate_figure(
    figure,
    top = text_grob(
      paste0("Blue Ruin iButton #", i),
      family = "serif",
      face = "bold",
      size = 20
    )
  )
  
  ggsave(
    filename = paste0(
      "final figures/supplemental figures/blueruin.ibutton.",
      i,
      ".do.temp.depth.jpg"
    ),
    plot = fig,
    width = 12,
    height = 8,
    units = "in"
  )
  
}

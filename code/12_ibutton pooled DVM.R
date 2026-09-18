library(dplyr)
library(lubridate)
library(ggplot2)
library(readr)
library(ggpubr)

# Load and preprocess data


fish <- read_csv("data/modif.data/ibutton/all.ibuttons.depth.do.interpolation.resolvable.only.csv") %>%
  rename(
    depth = fish_depth,
    temperature = ibutton.temp,
    ibutton.id = ibutton
  ) %>%
  mutate(
    date.time = ymd_hms(date.time),
    date = as.Date(date.time),
  ) 

fish <- fish %>%
  mutate(date.time = parse_date_time(date.time, orders = "ymd HMS")) %>%
  filter(date.time >= as.POSIXct("2021-07-29", tz = "America/Los_Angeles"),
         date.time <  as.POSIXct("2021-08-06", tz = "America/Los_Angeles")) %>%
  mutate(
    hour = hour(date.time),
    time = format(date.time, format = "%H:%M:%S"),
    fake.date = "2021-01-01",
    time2 = ymd_hms(paste(fake.date, time))
  )

# Split datasets by habitat
norwood_fish <- fish %>% filter(site == "norwood")
blueruin_fish <- fish %>% filter(site == "blue.ruin")

# Norwood plot
norwood_fish$hour_numeric <- hour(norwood_fish$time2) + minute(norwood_fish$time2) / 60
p_norwood <- ggplot(norwood_fish, aes(x = hour_numeric, y = depth)) +
  geom_smooth(
    fill = "#7C5467",
    colour = "#291919",
    span = 0.3,
    linewidth = 0.4,
    alpha = 0.5
  )+
  scale_y_reverse(
    limits = c(1.6, 0.2),
    breaks = seq(0, 1.6, by = 0.4)
  ) +
  scale_x_continuous(
    breaks = seq(0, 24, 4),
    limits = c(0, 24),
    expand = c(0, 0)
  ) +
  labs(
    x = "Hour of Day",
    y = "Depth (m)"
  ) +
  theme_minimal(base_size = 22) +
  theme(
    panel.grid = element_blank(),
    legend.key = element_blank(),
    text = element_text(family = "serif"),
    
    # add axes
    axis.line = element_line(colour = "black", linewidth = 0.8),
    axis.ticks = element_line(colour = "black", linewidth = 0.8),
    axis.ticks.length = unit(0.2, "cm")
  )

# Blue Ruin plot
blueruin_fish$hour_numeric <- hour(blueruin_fish$time2) + minute(blueruin_fish$time2) / 60
p_blueruin <- ggplot(blueruin_fish, aes(x = hour_numeric, y = depth)) +
  geom_smooth(
    fill = "#7C5467",
    colour = "#291919",
    span = 0.3,
    linewidth = 0.4,
    alpha = 0.5
  )+
  scale_y_reverse(
    limits = c(1.6, 0.2),
    breaks = seq(0, 1.6, by = 0.4)
  ) +
  scale_x_continuous(
    breaks = seq(0, 24, 4),
    limits = c(0, 24),
    expand = c(0, 0)
  ) +
  labs(
    x = "Hour of Day",
    y = "Depth (m)"
  ) +
  theme_minimal(base_size = 22) +
  theme(
    panel.grid = element_blank(),
    legend.key = element_blank(),
    text = element_text(family = "serif"),
    
    # add axes
    axis.line = element_line(colour = "black", linewidth = 0.8),
    axis.ticks = element_line(colour = "black", linewidth = 0.8),
    axis.ticks.length = unit(0.2, "cm")
  )

# Print plots separately
print(p_norwood)
print(p_blueruin)

final.figbd <- ggarrange(
  p_blueruin,
  p_norwood,
  ncol = 1,
  nrow = 2,
  labels = c("B", "D"),
  font.label = list(
    family = "serif",
    face = "bold",
    size = 22
  ),
  align = "hv"
)

ggsave(final.figbd, filename = paste("final figures/figure 3_pooled iButton 7 day.png"), width = 8, height = 14, units = "cm")





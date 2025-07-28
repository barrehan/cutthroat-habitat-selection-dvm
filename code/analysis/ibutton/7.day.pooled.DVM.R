library(dplyr)
library(lubridate)
library(ggplot2)
library(readr)

# Load and preprocess data
fish <- read_csv("data/modif.data/ibutton/all.ib.interp.depth.csv")

fish <- fish %>%
  mutate(date.time = parse_date_time(date.time, orders = "mdy HM")) %>%
  filter(case == 1,
         date.time >= as.POSIXct("2021-08-01", tz = "America/Los_Angeles"),
         date.time <  as.POSIXct("2021-08-08", tz = "America/Los_Angeles")) %>%
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
  geom_smooth(fill = "#7C5467", colour = "#291919", span = 0.3) +
  scale_y_reverse() +
  scale_x_continuous(
    breaks = seq(0, 24, 2),
    limits = c(0, 24),
    expand = c(0, 0)
  ) +
  labs(
    x = "Hour of Day",
    y = "Depth (m)"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_blank(),
    text = element_text(family = "serif")
  )

# Blue Ruin plot
blueruin_fish$hour_numeric <- hour(blueruin_fish$time2) + minute(blueruin_fish$time2) / 60
p_blueruin <- ggplot(blueruin_fish, aes(x = hour_numeric, y = depth)) +
  geom_smooth(fill = "#7C5467", colour = "#291919", span = 0.3) +
  scale_y_reverse() +
  scale_x_continuous(
    breaks = seq(0, 24, 2),
    limits = c(0, 24),
    expand = c(0, 0)
  ) +
  labs(
    x = "Hour of Day",
    y = "Depth (m)"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_blank(),
    text = element_text(family = "serif")
  )

# Print plots separately
print(p_norwood)
print(p_blueruin)

ggsave(p_norwood, filename = paste("results/figures/ibutton.simulation/smoothed.7day.depth.norwood.png"), width = 13, height = 9, units = "cm")
ggsave(p_blueruin, filename = paste("results/figures/ibutton.simulation/smoothed.7day.depth.blueruin.png"), width = 13, height = 9, units = "cm")




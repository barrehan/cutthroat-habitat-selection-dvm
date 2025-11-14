# --- packages ---
library(tidyverse)
library(lubridate)
library(viridis)      # or viridisLite
library(ggpubr)       # to arrange both plots in one window

# --- read & time parsing (keeps your local-clock assumption) ---
br.ib <- read.csv("data/modif.data/hab.select.mod/br.ibutton.data/br.ibutton.pooled.csv")
br.ib$date.time <- force_tz(ymd_hms(br.ib$date.time), "America/Los_Angeles")

br.ib <- br.ib %>%
  filter(
    date.time >= ymd_hms("2021-07-30 00:00:00", tz = "America/Los_Angeles"),
    date.time <  ymd_hms("2021-08-06 00:00:00", tz = "America/Los_Angeles")
  ) %>%
  mutate(
    temperature       = as.numeric(temperature),
    dissolved.oxygen  = as.numeric(dissolved.oxygen),
    depth             = as.numeric(depth)
  )

# high 2-hour interval only + time-of-day (hours)
br_high <- br.ib %>%
  filter(highlowDO.2hr == "high") %>%
  mutate(
    date = as.Date(date.time),
    tod  = hour(date.time) + minute(date.time)/60 + second(date.time)/3600
  )

# axis helpers
x_breaks <- seq(17, 19, by = 0.5)
x_labels <- sprintf("%02d:%02d", floor(x_breaks), (x_breaks %% 1) * 60)

# ----------------------------
# OPTION A: stat_summary_2d()
# ----------------------------
# Pixelated heatmap by bin-averaging (adjust bins to taste)
bins_time  <- 24  # more = finer x pixels
bins_depth <- 40  # more = finer y pixels

p_temp <- ggplot(br_high, aes(tod, depth)) +
  stat_summary_2d(aes(z = temperature), bins = c(bins_time, bins_depth), fun = mean) +
  scale_y_reverse() +
  scale_x_continuous(limits = c(17, 19), breaks = x_breaks, labels = x_labels, expand = c(0,0)) +
  scale_fill_viridis(name = "Temperature (°C)") +
  facet_wrap(~ date, ncol = 4) +
  labs(x = "Time of day", y = "Depth (m)") +
  theme_bw()

p_do <- ggplot(br_high, aes(tod, depth)) +
  stat_summary_2d(aes(z = dissolved.oxygen), bins = c(bins_time, bins_depth), fun = mean) +
  scale_y_reverse() +
  scale_x_continuous(limits = c(17, 19), breaks = x_breaks, labels = x_labels, expand = c(0,0)) +
  scale_fill_viridis(name = "Dissolved oxygen (mg/L)", option = "B") +
  facet_wrap(~ date, ncol = 4) +
  labs(x = "Time of day", y = "Depth (m)") +
  theme_bw()

p_temp
p_do

# ----------------------------
# OPTION B: geom_tile() on raw points
# ----------------------------
# Fixed tile sizes (overlap a bit to reduce tiny gaps)
tile_w <- 0.12   # ~7 minutes wide
tile_h <- 0.10   # ~10 cm tall

p_temp_tile <- ggplot(br_high, aes(tod, depth, fill = temperature)) +
  geom_tile(width = tile_w, height = tile_h) +
  scale_y_reverse() +
  scale_x_continuous(limits = c(17, 19), breaks = x_breaks, labels = x_labels, expand = c(0,0)) +
  scale_fill_viridis(name = "Temperature (°C)") +
  facet_wrap(~ date, ncol = 4) +
  labs(x = "Time of day", y = "Depth (m)") +
  theme_bw()

p_do_tile <- ggplot(br_high, aes(tod, depth, fill = dissolved.oxygen)) +
  geom_tile(width = tile_w, height = tile_h) +
  scale_y_reverse() +
  scale_x_continuous(limits = c(17, 19), breaks = x_breaks, labels = x_labels, expand = c(0,0)) +
  scale_fill_viridis(name = "Dissolved oxygen (mg/L)", option = "B") +
  facet_wrap(~ date, ncol = 4) +
  labs(x = "Time of day", y = "Depth (m)") +
  theme_bw()

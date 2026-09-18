library(dplyr)
library(lubridate)
library(ggplot2)
library(readr)

# ---------------------------------------
# Load + filter 
# ---------------------------------------
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
  filter(
    date.time >= as.POSIXct("2021-07-29 00:00:00", tz = "America/Los_Angeles"),
    date.time <  as.POSIXct("2021-08-06 00:00:00", tz = "America/Los_Angeles")
  )

# ---------------------------------------
# Plot (LOESS multi-day) 
# ---------------------------------------
p_multiday <- ggplot(fish, aes(x = date.time, y = depth)) +
  geom_smooth(
    method = "loess",
    span = 0.1,
    fill = "#7C5467",
    colour = "#291919",
    linewidth = 1
  ) +
  scale_y_reverse() +
  facet_wrap(
    ~site,
    ncol = 1,
    scales = "free_x",
    labeller = labeller(
      site = c(
        "blue.ruin" = "Blue Ruin",
        "norwood"   = "Norwood"
      )
    )
  ) +
  scale_x_datetime(
    date_breaks = "1 day",
    date_labels = "%b %d"
  ) +
  labs(
    x = "Date",
    y = "Depth (m)"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_blank(),
    text = element_text(family = "serif"),
    strip.background = element_blank(),
    strip.text = element_text(
      size = 18,      # same as axis-title size from base_size = 18
      face = "plain"
    )
  )

print(p_multiday)

p_multiday <- ggplot(fish, aes(x = date.time, y = depth)) +
  geom_smooth(
    method = "loess",
    span = 0.1,
    fill = "#7C5467",
    colour = "#291919",
    linewidth = 1
  ) +
  scale_y_reverse() +
  facet_wrap(
    ~site,
    ncol = 1,
    scales = "free_x",
    labeller = labeller(
      site = c(
        "blue.ruin" = "Blue Ruin",
        "norwood"   = "Norwood"
      )
    )
  ) +
  scale_x_datetime(
    date_breaks = "1 day",
    date_labels = "%b %d"
  ) +
  labs(
    x = "Date",
    y = "Depth (m)"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_blank(),
    text = element_text(family = "serif"),
    strip.background = element_blank(),
    strip.text = element_text(
      size = 18,      # same as axis-title size from base_size = 18
      face = "plain"
    )
  )

print(p_multiday)

ggsave(
  filename = "final figures/supplemental figures/depth_multiday_loess.png",
  plot = p_multiday,
  width = 10,
  height = 6,
  units = "in",
  dpi = 300
)

ggplot(fish, aes(date.time, depth)) +
  geom_point(alpha = 0.05, size = 0.5) +
  geom_smooth(
    method = "loess",
    span = 0.1,
    se = FALSE,
    colour = "red"
  ) +
  scale_y_reverse() +
  facet_wrap(~site, ncol = 1)

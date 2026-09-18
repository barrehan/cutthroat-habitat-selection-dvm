# Multiday plot for blue ruin netpen logger array - using DO from array 5 
# sensors at 1.35 and 0.4m depth - removed DO from netpen sensors at 1.55 
# and 0.5m depth



br.logger.array <- read.csv(
  "data/modif.data/logger.array/blue.ruin.netpen.array.do.temp.csv"
) %>%
  mutate(
    date.time = mdy_hm(date.time),
    date.time = force_tz(date.time, "America/Los_Angeles"),
    date.time = round_date(date.time, "5 minutes"),
    sensor.depth = factor(sensor.depth)
  ) %>%
  filter(
    date.time > ymd_hms("2021-07-25 12:00:00"),
    date.time < ymd_hms("2021-08-14 00:00:00")
  )

unique(br.logger.array$sensor.depth)

# Confirming we only have DO from the loggers from site 5 array, no temperature

br.logger.array %>%
  filter(sensor.depth %in% c("1.35", "0.4")) %>%
  summarise(
    n_temp = sum(!is.na(temperature)),
    n_do   = sum(!is.na(dissolved.oxygen))
  )

colors <- c("#8C2B0E", "#C5692D", "#FEB359","#81A88D", "#132F5B", "#435F90", "#426737",  "#291919")

a <- ggplot(data = br.logger.array, aes(x = date.time)) +
  geom_jitter(aes(y = temperature, colour = sensor.depth), size = 1) +
  geom_jitter(aes(y = dissolved.oxygen, colour = sensor.depth), size = 1) +
  scale_y_continuous(
    breaks = seq(0, 24, by = 3)
  ) +
  expand_limits(y = 0) +
  scale_colour_manual(
    values = colors,
    name = "Logger depth (m)"
  ) +
  scale_x_datetime(
    date_labels = "%b %d",
    date_breaks = "2 days"
  ) +
  labs(
    x = "Date",
    y = NULL
  ) +
  annotate(
    "text",
    x = -Inf,
    y = 18,
    label = "Temperature (°C)",
    hjust = 1.3,
    family = "serif",
    size = 6
  ) +
  annotate(
    "text",
    x = -Inf,
    y = 5,
    label = expression("DO (" * mg ~ L^{-1} * ")"),
    hjust = 1.3,
    family = "serif",
    size = 6
  ) +
  coord_cartesian(clip = "off") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.position = "bottom",
    legend.key = element_blank(),
    legend.background = element_blank(),
    text = element_text(size = 18, family = "serif"),
    
    # increase left margin for labels
    plot.margin = margin(
      t = 10,
      r = 10,
      b = 10,
      l = 180
    )
  )


# Norwood logger array

nor.logger.array <- read.csv(
  "data/raw.data/logger.array/norwood.mouth.temp.do.csv"
) %>%
  mutate(
    date.time = mdy_hm(date.time),
    date.time = force_tz(date.time, "America/Los_Angeles"),
    date.time = round_date(date.time, "5 minutes"),
    sensor.depth = factor(sensor.depth)
  ) %>%
  filter(
    date.time > ymd_hms("2021-07-25 12:00:00"),
    date.time < ymd_hms("2021-08-14 00:00:00")
  )


colors <- c("#8C2B0E", "#C5692D", "#FEB359","#81A88D", "#132F5B", "#435F90", "#426737",  "#291919")

b <- ggplot(data = nor.logger.array, aes(x = date.time)) +
  geom_jitter(aes(y = temperature, colour = sensor.depth), size = 1) +
  geom_jitter(aes(y = dissolved.oxygen, colour = sensor.depth), size = 1) +
  scale_y_continuous(
    breaks = seq(0, 27, by = 3)
  ) +
  expand_limits(y = 0) +
  scale_colour_manual(
    values = colors,
    name = "Logger depth (m)"
  ) +
  scale_x_datetime(
    date_labels = "%b %d",
    date_breaks = "2 days"
  ) +
  labs(
    x = "Date",
    y = NULL
  ) +
  annotate(
    "text",
    x = -Inf,
    y = 18,
    label = "Temperature (°C)",
    hjust = 1.3,
    family = "serif",
    size = 6
  ) +
  annotate(
    "text",
    x = -Inf,
    y = 5,
    label = expression("DO (" * mg ~ L^{-1} * ")"),
    hjust = 1.3,
    family = "serif",
    size = 6
  ) +
  coord_cartesian(clip = "off") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.position = "bottom",
    legend.key = element_blank(),
    legend.background = element_blank(),
    text = element_text(size = 18, family = "serif"),
    
    # increase left margin for labels
    plot.margin = margin(
      t = 10,
      r = 10,
      b = 10,
      l = 180
    )
  )

ggsave(
  filename = "final figures/supplemental figures/blueruin.netpen.do.temp.png",
  plot = a,
  width = 30,
  height = 12,
  units = "cm",
  dpi = 600
)
ggsave(
  filename = "final figures/supplemental figures/norwood.netpen.do.temp.png",
  plot = b,
  width = 30,
  height = 12,
  units = "cm",
  dpi = 600
)



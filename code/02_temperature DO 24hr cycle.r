library(tidyverse)
library(lubridate)
library(mgcv)
library(patchwork)
library(ggpubr)

#---------------------------------------
# colors
#---------------------------------------

do_min_col <- "#289A84"
do_max_col <- "#8FF7BD"

#==============================================================================
# NORWOOD
#==============================================================================

dat <- read_csv(
  "data/raw.data/logger.array/norwood.mouth.temp.do.csv",
  show_col_types = FALSE
) %>%
  mutate(
    date.time = mdy_hm(date.time),
    date = as.Date(date.time),
    hour = hour(date.time) + minute(date.time) / 60
  )

aug1 <- dat %>%
  filter(date == as.Date("2021-08-01"))

# all DO sensors
do_dat <- aug1 %>%
  filter(!is.na(dissolved.oxygen))

do_dat <- do_dat %>%
  mutate(variable = "DO")

# all temperature sensors
temp_dat <- aug1 %>%
  filter(!is.na(temperature))

temp_dat <- temp_dat %>%
  mutate(variable = "Temperature")

#---------------------------------------
# DO-min and DO-max windows
#---------------------------------------

do_mean <- do_dat %>%
  group_by(hour) %>%
  summarise(
    mean_do = mean(
      dissolved.oxygen,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

do_gam <- gam(
  mean_do ~ s(hour, k = 5),
  data = do_mean
)

pred_grid <- tibble(
  hour = seq(0, 24, by = 0.01)
)

pred_grid$do_pred <- predict(
  do_gam,
  newdata = pred_grid
)

do_min_hr <- pred_grid$hour[
  which.min(pred_grid$do_pred)
]

do_max_hr <- pred_grid$hour[
  which.max(pred_grid$do_pred)
]

#==============================================================================
# BLUE RUIN
#==============================================================================

datbr <- read_csv(
  "data/modif.data/logger.array/blue.ruin.netpen.array.do.temp.csv",
  show_col_types = FALSE
) %>%
  mutate(
    date.time = mdy_hm(date.time),
    date = as.Date(date.time),
    hour = hour(date.time) + minute(date.time) / 60
  )

aug1br <- datbr %>%
  filter(date == as.Date("2021-08-01"))

# all DO sensors
do_dat_br <- aug1br %>%
  filter(!is.na(dissolved.oxygen))

do_dat_br <- do_dat_br %>%
  mutate(variable = "DO")

# all temperature sensors
temp_dat_br <- aug1br %>%
  filter(!is.na(temperature))

temp_dat_br <- temp_dat_br %>%
mutate(variable = "Temperature")

#---------------------------------------
# DO-min and DO-max windows
#---------------------------------------

do_mean_br <- do_dat_br %>%
  group_by(hour) %>%
  summarise(
    mean_do = mean(
      dissolved.oxygen,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

do_gam_br <- gam(
  mean_do ~ s(hour, k = 5),
  data = do_mean_br
)

pred_grid_br <- tibble(
  hour = seq(0, 24, by = 0.01)
)

pred_grid_br$do_pred <- predict(
  do_gam_br,
  newdata = pred_grid_br
)

do_min_hr_br <- pred_grid_br$hour[
  which.min(pred_grid_br$do_pred)
]

do_max_hr_br <- pred_grid_br$hour[
  which.max(pred_grid_br$do_pred)
]

#==============================================================================
# CREATE COMMON DEPTH COLOR PALETTE
#==============================================================================

all_depths <- sort(
  unique(
    c(
      do_dat$sensor.depth,
      temp_dat$sensor.depth,
      do_dat_br$sensor.depth,
      temp_dat_br$sensor.depth
    )
  )
)

library(scales)

library(scales)

library(scales)


depth_cols <- setNames(
  alpha(
    c(
      "#123B48",
      "#1D4F5F",
      "#275E6E",
      "#316D7A",
      "#3B7C87",
      "#478C92",
      "#559C9D",
      "#66ADA7",
      "#79BDB2",
      "#8CCBBC",
      "#A2D8C8",
      "#C0E7DA",
      "#E8F8F0"
    )
    [1:length(all_depths)],
  0.9
),
  all_depths
)



names(depth_cols) <- all_depths

#==============================================================================
# NORWOOD PANEL
#==============================================================================

p_nor <- ggplot() +
  
  annotate(
    "rect",
    xmin = do_min_hr - 2,
    xmax = do_min_hr + 2,
    ymin = -Inf,
    ymax = Inf,
    fill = do_min_col,
    alpha = 0.65
  ) +
  
  annotate(
    "rect",
    xmin = do_max_hr - 2,
    xmax = do_max_hr + 2,
    ymin = -Inf,
    ymax = Inf,
    fill = do_max_col,
    alpha = 0.65
  ) +
  
  geom_smooth(
    data = do_dat,
    aes(
      hour,
      dissolved.oxygen,
      colour = factor(sensor.depth),
      linetype = variable
    ),
    method = "gam",
    formula = y ~ s(x, k = 5),
    se = FALSE,
    linewidth = 1.1
  ) +
  
  geom_smooth(
    data = temp_dat,
    aes(
      hour,
      temperature,
      colour = factor(sensor.depth),
      linetype = variable
    ),
    method = "gam",
    formula = y ~ s(x, k = 5),
    se = FALSE,
    linewidth = 1.1
  ) +
  
  annotate(
    "text",
    x = do_min_hr,
    y = 27,
    label = "DO-min",
    family = "serif",
    size = 4.5
  ) +
  
  annotate(
    "text",
    x = do_max_hr,
    y = 27,
    label = "DO-max",
    family = "serif",
    size = 4.5
  ) +
  
  scale_colour_manual(
    values = depth_cols,
    breaks = as.character(sort(all_depths)),
    name = "Sensor depth (m)"
  ) +
  
  scale_linetype_manual(
    name = "Variable",
    breaks = c("Temperature", "DO"),
    values = c(
      "Temperature" = "22",
      "DO" = "solid"
    )
  ) +
  
  guides(
    colour = guide_legend(
      order = 1,
      override.aes = list(
        linetype = 1,
        linewidth = 1.3,
        alpha = 0.7
      )
    ),
    linetype = guide_legend(
      order = 2,
      override.aes = list(
        colour = "black",
        linewidth = 1.3
      )
    )
  ) +
  
  scale_x_continuous(
    breaks = seq(0, 24, 2),
    limits = c(0, 24)
  ) +
  
  scale_y_continuous(
    breaks = seq(0, 30, by = 5)
  ) +
  
  labs(
    x = "Hour of day",
    y = expression(
      paste(
        "DO (mg L"^-1,") and Temperature (", degree, "C)"
      )
    )
  ) +
  
  theme_classic(
    base_family = "serif",
    base_size = 16
  ) +
  
  theme(
    legend.position = "bottom",
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    plot.title = element_text(
      hjust = 0.5,
      size = 25
    )
  )

#==============================================================================
# BLUE RUIN PANEL
#==============================================================================

p_br <- ggplot() +
  
  annotate(
    "rect",
    xmin = do_min_hr_br - 2,
    xmax = do_min_hr_br + 2,
    ymin = -Inf,
    ymax = Inf,
    fill = do_min_col,
    alpha = 0.65
  ) +
  
  annotate(
    "rect",
    xmin = do_max_hr_br - 2,
    xmax = do_max_hr_br + 2,
    ymin = -Inf,
    ymax = Inf,
    fill = do_max_col,
    alpha = 0.65
  ) +
  
  geom_smooth(
    data = do_dat_br,
    aes(
      hour,
      dissolved.oxygen,
      colour = factor(sensor.depth),
      linetype = variable
    ),
    method = "gam",
    formula = y ~ s(x, k = 5),
    se = FALSE,
    linewidth = 1.1
  ) +
  
  geom_smooth(
    data = temp_dat_br,
    aes(
      hour,
      temperature,
      colour = factor(sensor.depth),
      linetype = variable
    ),
    method = "gam",
    formula = y ~ s(x, k = 5),
    se = FALSE,
    linewidth = 1.1
  ) +
  
  annotate(
    "text",
    x = do_min_hr_br,
    y = 27,
    label = "DO-min",
    family = "serif",
    size = 4.5
  ) +
  
  annotate(
    "text",
    x = do_max_hr_br,
    y = 27,
    label = "DO-max",
    family = "serif",
    size = 4.5
  ) +
  
  scale_colour_manual(
    values = depth_cols,
    breaks = as.character(sort(all_depths)),
    name = "Sensor depth (m)"
  ) +
  
  scale_linetype_manual(
    name = "Variable",
    breaks = c("Temperature", "DO"),
    values = c(
      "Temperature" = "22",
      "DO" = "solid"
    )
  ) +
  
  guides(
    colour = guide_legend(
      order = 1,
      override.aes = list(
        linetype = 1,
        linewidth = 1.3,
        alpha = 0.7
      )
    ),
    linetype = guide_legend(
      order = 2,
      override.aes = list(
        colour = "black",
        linewidth = 1.3
      )
    )
  ) +
  
  scale_x_continuous(
    breaks = seq(0, 24, 2),
    limits = c(0, 24)
  ) +
  
  scale_y_continuous(
    breaks = seq(0, 30, by = 5)
  ) +
  
  labs(
    x = "Hour of day",
    y = expression(
      paste(
        "DO (mg L"^-1,") and Temperature (", degree, "C)"
      )
    )
  ) +
  
  theme_classic(
    base_family = "serif",
    base_size = 16
  ) +
  
  theme(
    legend.position = "bottom",
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    plot.title = element_text(
      hjust = 0.5,
      size = 25
    )
  )

#==============================================================================
# MOVE LEGENDS TO RIGHT
#==============================================================================


p_br <- p_br +
  theme(
    legend.position = "right"
  )

p_nor <- p_nor +
  theme(
    legend.position = "right"
  )


combined_plot <- ggarrange(
  p_br,
  p_nor,
  ncol = 1,
  nrow = 2,
  labels = c("A", "B"),
  hjust = -0.2,
  vjust = 1,
  font.label = list(
    family = "serif",
    face = "bold",
    size = 18
  )
)

combined_plot


ggsave(
  "final figures/figure 1_24h temp do panels.png",
  plot = combined_plot,
  width = 18,
  height = 25,
  units = "cm",
  dpi = 600
)



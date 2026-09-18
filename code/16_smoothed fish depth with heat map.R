library(tidyverse)
library(lubridate)
library(ggpubr)

# -----------------------------
# Load fish depth data
# -----------------------------

fish <- read_csv(
  "data/modif.data/ibutton/all.ibuttons.depth.do.interpolation.resolvable.only.csv"
) %>%
  rename(
    depth = fish_depth,
    temperature = ibutton.temp,
    ibutton.id = ibutton
  ) %>%
  mutate(
    date.time = ymd_hms(date.time),
    
    # USE FILE DATE COLUMN
    date = as.Date(date),
    
    hour_numeric =
      hour(date.time) +
      minute(date.time) / 60
  ) %>%
  filter(
    date >= as.Date("2021-07-29"),
    date <= as.Date("2021-08-05")
  )

# -----------------------------
# Logger files
# -----------------------------

logger_files <- c(
  "data/modif.data/logger.array/br.unif.do.temp.set.depth.simulation.csv",
  "data/modif.data/logger.array/nor.unif.do.temp.set.depth.simulation.csv"
)

temp_limits <- c(12, 22)
do_limits   <- c(1, 10)
depth_limits <- c(1.6, 0.2)

for(file_path in logger_files){
  
  if(grepl("br\\.", file_path)){
    
    site_name  <- "blue.ruin"
    site_label <- "Blue Ruin"
    
  } else {
    
    site_name  <- "norwood"
    site_label <- "Norwood"
    
  }
  
  array <- read.csv(file_path)
  
  array <- array[, 1:4]
  
  array$date.time <- ymd_hms(
    array$date.time,
    tz = "America/Los_Angeles"
  )
  
  array$hour <-
    hour(array$date.time) +
    minute(array$date.time) / 60
  
  array$date <- as.Date(array$date.time)
  
  array <- array %>%
    drop_na(depth) %>%
    filter(
      date >= as.Date("2021-07-29"),
      date <= as.Date("2021-08-06")
    )
  
  fish_site <- fish %>%
    filter(site == site_name)
  
 dates <- sort(unique(array$date))

for(d in dates){

  day_env <- array %>%
    filter(as.Date(date) == as.Date(d))

  day_fish <- fish_site %>%
    filter(as.Date(date) == as.Date(d))
    
    if(
      nrow(day_env) == 0 ||
      nrow(day_fish) < 20
    ) next
    
    # -----------------------------
    # Manual LOESS fit
    # -----------------------------
    
    fit <- loess(
      depth ~ hour_numeric,
      data = day_fish,
      span = 0.5,
      degree = 2
    )
    
    pred_grid <- tibble(
      hour_numeric = seq(
        min(day_fish$hour_numeric, na.rm = TRUE),
        max(day_fish$hour_numeric, na.rm = TRUE),
        by = 0.05
      )
    )
    
    pred <- predict(
      fit,
      newdata = pred_grid,
      se = TRUE
    )
    
    pred_grid <- pred_grid %>%
      mutate(
        depth = pred$fit,
        lower = pred$fit - 1.96 * pred$se.fit,
        upper = pred$fit + 1.96 * pred$se.fit
      )
    
    plot_date <- format(
      as.Date(d),
      "%b %d, %Y"
    )
    
    # -----------------------------
    # Temperature
    # -----------------------------
    
    p_temp <- ggplot() +
      
      geom_tile(
        data = day_env,
        aes(
          x = hour,
          y = depth,
          fill = temperature
        )
      ) +
      
      geom_ribbon(
        data = pred_grid,
        aes(
          x = hour_numeric,
          ymin = lower,
          ymax = upper
        ),
        inherit.aes = FALSE,
        fill = "black",
        alpha = 0.2
      ) +
      
      geom_line(
        data = pred_grid,
        aes(
          hour_numeric,
          depth
        ),
        colour = "black",
        linewidth = 1.2
      ) +
      
      scale_fill_gradientn(
        name = "Temp (°C)",
        colours = c(
          "#4575B4",
          "#91BFDB",
          "#D9F0F3",
          "#FFFFBF",
          "#FEE08B",
          "#FC8D59",
          "#D73027"
        ),
        limits = temp_limits,
        oob = scales::squish
      ) +
      
      scale_y_reverse(
        limits = depth_limits
      ) +
      
      scale_x_continuous(
        breaks = seq(0, 24, 4),
        limits = c(0, 24)
      ) +
      
      labs(
        title = paste(
          site_label,
          "-",
          plot_date,
          "Temperature"
        ),
        x = "Hour of Day",
        y = "Depth (m)"
      ) +
      
      theme_classic(
        base_family = "serif"
      ) +
      theme(
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        plot.title = element_text(size = 22)
      )
    
    # -----------------------------
    # Dissolved oxygen
    # -----------------------------
    
    p_do <- ggplot() +
      
      geom_tile(
        data = day_env,
        aes(
          x = hour,
          y = depth,
          fill = dissolved.oxygen
        )
      ) +
      
      geom_ribbon(
        data = pred_grid,
        aes(
          x = hour_numeric,
          ymin = lower,
          ymax = upper
        ),
        inherit.aes = FALSE,
        fill = "black",
        alpha = 0.2
      ) +
      
      geom_line(
        data = pred_grid,
        aes(
          hour_numeric,
          depth
        ),
        colour = "black",
        linewidth = 1.2
      ) +
      
      scale_fill_viridis_c(
        name = "DO (mg/L)",
        option = "inferno",
        direction = -1,
        limits = do_limits,
        oob = scales::squish
      ) +
      
      scale_y_reverse(
        limits = depth_limits
      ) +
      
      scale_x_continuous(
        breaks = seq(0, 24, 4),
        limits = c(0, 24)
      ) +
      
      labs(
        title = paste(
          site_label,
          "-",
          plot_date,
          "Dissolved Oxygen"
        ),
        x = "Hour of Day",
        y = "Depth (m)"
      ) +
      
      theme_classic(
        base_family = "serif"
      ) +
      theme(
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        plot.title = element_text(size = 22)
      )
    
    combined <- ggarrange(
      p_temp,
      p_do,
      ncol = 2
    )
    
    print(combined)
    
    ggsave(
      filename = paste0(
        "final figures/",
        gsub(" ", "_", site_label),
        "_",
        format(as.Date(d), "%Y-%m-%d"),
        ".png"
      ),
      plot = combined,
      width = 14,
      height = 7,
      dpi = 300
    )
  }
}















#==============================================================================
# AUGUST 1 4-PANEL FIGURE
#==============================================================================

#---------------------------------------
# Blue Ruin
#---------------------------------------

br_env <- read.csv(
  "data/modif.data/logger.array/br.unif.do.temp.set.depth.simulation.csv"
)

br_env$date.time <- ymd_hms(
  br_env$date.time,
  tz = "America/Los_Angeles"
)

br_env <- br_env %>%
  mutate(
    date = as.Date(date.time),
    hour = hour(date.time) +
      minute(date.time) / 60
  ) %>%
  filter(date == as.Date("2021-08-01"))

br_fish <- fish %>%
  filter(
    site == "blue.ruin",
    date == as.Date("2021-08-01")
  )

fit_br <- loess(
  depth ~ hour_numeric,
  data = br_fish,
  span = 0.5,
  degree = 2
)

pred_br <- tibble(
  hour_numeric = seq(0, 24, by = 0.05)
)

pred_out_br <- predict(
  fit_br,
  newdata = pred_br,
  se = TRUE
)

pred_br <- pred_br %>%
  mutate(
    depth = pred_out_br$fit,
    lower = pred_out_br$fit - 1.96 * pred_out_br$se.fit,
    upper = pred_out_br$fit + 1.96 * pred_out_br$se.fit
  )

#---------------------------------------
# Norwood
#---------------------------------------

nor_env <- read.csv(
  "data/modif.data/logger.array/nor.unif.do.temp.set.depth.simulation.csv"
)

nor_env$date.time <- ymd_hms(
  nor_env$date.time,
  tz = "America/Los_Angeles"
)

nor_env <- nor_env %>%
  mutate(
    date = as.Date(date.time),
    hour = hour(date.time) +
      minute(date.time) / 60
  ) %>%
  filter(date == as.Date("2021-08-01"))

nor_fish <- fish %>%
  filter(
    site == "norwood",
    date == as.Date("2021-08-01")
  )

fit_nor <- loess(
  depth ~ hour_numeric,
  data = nor_fish,
  span = 0.5,
  degree = 2
)

pred_nor <- tibble(
  hour_numeric = seq(0, 24, by = 0.05)
)

pred_out_nor <- predict(
  fit_nor,
  newdata = pred_nor,
  se = TRUE
)

pred_nor <- pred_nor %>%
  mutate(
    depth = pred_out_nor$fit,
    lower = pred_out_nor$fit - 1.96 * pred_out_nor$se.fit,
    upper = pred_out_nor$fit + 1.96 * pred_out_nor$se.fit
  )

#---------------------------------------
# Common theme for all panels
#---------------------------------------

common_theme <-
  theme_classic(
    base_size = 18,
    base_family = "serif"
  ) +
  theme(
    # A–D labels
    plot.title = element_text(
      face = "bold",
      size = 30,
      hjust = 0
    ),
    
    # Axis titles and legend titles
    axis.title = element_text(
      size = 22
    ),
    legend.title = element_text(
      size = 22
    ),
    
    # Axis numbers and legend numbers
    axis.text = element_text(
      size = 18
    ),
    legend.text = element_text(
      size = 18
    )
  )
#---------------------------------------
# A: Blue Ruin Temperature
#---------------------------------------

pA <- ggplot() +
  
  geom_tile(
    data = br_env,
    aes(
      x = hour,
      y = depth,
      fill = temperature
    )
  ) +
  
  geom_ribbon(
    data = pred_br,
    aes(
      x = hour_numeric,
      ymin = lower,
      ymax = upper
    ),
    inherit.aes = FALSE,
    fill = "black",
    alpha = 0.2
  ) +
  
  geom_line(
    data = pred_br,
    aes(
      hour_numeric,
      depth
    ),
    colour = "black",
    linewidth = 1.2
  ) +
  
  scale_fill_gradientn(
    name = "Temp (°C)",
    colours = c(
      "#4575B4",
      "#91BFDB",
      "#D9F0F3",
      "#FFFFBF",
      "#FEE08B",
      "#FC8D59",
      "#D73027"
    ),
    limits = temp_limits,
    oob = scales::squish
  ) +
  
  scale_y_reverse(
    limits = depth_limits
  ) +
  
  scale_x_continuous(
    breaks = seq(0, 24, 4),
    limits = c(0, 24)
  ) +
  
  labs(
    title = "A",
    x = "Hour of Day",
    y = "Depth (m)"
  ) +
  
  common_theme

#---------------------------------------
# B: Blue Ruin DO
#---------------------------------------

pB <- ggplot() +
  
  geom_tile(
    data = br_env,
    aes(
      x = hour,
      y = depth,
      fill = dissolved.oxygen
    )
  ) +
  
  geom_ribbon(
    data = pred_br,
    aes(
      x = hour_numeric,
      ymin = lower,
      ymax = upper
    ),
    inherit.aes = FALSE,
    fill = "black",
    alpha = 0.2
  ) +
  
  geom_line(
    data = pred_br,
    aes(
      hour_numeric,
      depth
    ),
    colour = "black",
    linewidth = 1.2
  ) +
  
  scale_fill_viridis_c(
    name = "DO (mg/L)",
    option = "inferno",
    direction = -1,
    limits = do_limits,
    oob = scales::squish
  ) +
  
  scale_y_reverse(
    limits = depth_limits
  ) +
  
  scale_x_continuous(
    breaks = seq(0, 24, 4),
    limits = c(0, 24)
  ) +
  
  labs(
    title = "B",
    x = "Hour of Day",
    y = "Depth (m)"
  ) +
  
  common_theme

#---------------------------------------
# C: Norwood Temperature
#---------------------------------------

pC <- ggplot() +
  
  geom_tile(
    data = nor_env,
    aes(
      x = hour,
      y = depth,
      fill = temperature
    )
  ) +
  
  geom_ribbon(
    data = pred_nor,
    aes(
      x = hour_numeric,
      ymin = lower,
      ymax = upper
    ),
    inherit.aes = FALSE,
    fill = "black",
    alpha = 0.2
  ) +
  
  geom_line(
    data = pred_nor,
    aes(
      hour_numeric,
      depth
    ),
    colour = "black",
    linewidth = 1.2
  ) +
  
  scale_fill_gradientn(
    name = "Temp (°C)",
    colours = c(
      "#4575B4",
      "#91BFDB",
      "#D9F0F3",
      "#FFFFBF",
      "#FEE08B",
      "#FC8D59",
      "#D73027"
    ),
    limits = temp_limits,
    oob = scales::squish
  ) +
  
  scale_y_reverse(
    limits = depth_limits
  ) +
  
  scale_x_continuous(
    breaks = seq(0, 24, 4),
    limits = c(0, 24)
  ) +
  
  labs(
    title = "C",
    x = "Hour of Day",
    y = "Depth (m)"
  ) +
  
  common_theme

#---------------------------------------
# D: Norwood DO
#---------------------------------------

pD <- ggplot() +
  
  geom_tile(
    data = nor_env,
    aes(
      x = hour,
      y = depth,
      fill = dissolved.oxygen
    )
  ) +
  
  geom_ribbon(
    data = pred_nor,
    aes(
      x = hour_numeric,
      ymin = lower,
      ymax = upper
    ),
    inherit.aes = FALSE,
    fill = "black",
    alpha = 0.2
  ) +
  
  geom_line(
    data = pred_nor,
    aes(
      hour_numeric,
      depth
    ),
    colour = "black",
    linewidth = 1.2
  ) +
  
  scale_fill_viridis_c(
    name = "DO (mg/L)",
    option = "inferno",
    direction = -1,
    limits = do_limits,
    oob = scales::squish
  ) +
  
  scale_y_reverse(
    limits = depth_limits
  ) +
  
  scale_x_continuous(
    breaks = seq(0, 24, 4),
    limits = c(0, 24)
  ) +
  
  labs(
    title = "D",
    x = "Hour of Day",
    y = "Depth (m)"
  ) +
  
  common_theme

#---------------------------------------
# Combine
#---------------------------------------

supp_aug1 <- ggarrange(
  pA,
  pB,
  pC,
  pD,
  ncol = 2,
  nrow = 2
)

supp_aug1

ggsave(
  "final figures/figure5_aug1 4panel heatmap.png",
  plot = supp_aug1,
  width = 16,
  height = 12,
  units = "in",
  dpi = 600
)

# ============================================================
# TDObal habitat suitability heatmaps
# Site-specific scaling from logger array observations
# Pooled across days within site
# ============================================================

library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(grid)

# ------------------------------------------------------------
# Function to build heatmap for one site
# ------------------------------------------------------------

make_tdobal_heatmap <- function(site_name, file_in){
  
  # ----------------------------------------------------------
  # Read data
  # ----------------------------------------------------------
  
  array <- read.csv(file_in)
  
  array$date.time <- ymd_hms(
    array$date.time,
    tz = "America/Los_Angeles"
  )
  
  week <- array %>%
    filter(
      date.time >= as.POSIXct(
        "2021-07-29 00:00:00",
        tz = "America/Los_Angeles"
      ),
      date.time < as.POSIXct(
        "2021-08-06 00:00:00",
        tz = "America/Los_Angeles"
      )
    ) %>%
    drop_na(
      temperature,
      dissolved.oxygen
    )
  
  # ----------------------------------------------------------
  # Site-specific min/max values
  # ----------------------------------------------------------
  
  t_min <- min(week$temperature, na.rm = TRUE)
  t_max <- max(week$temperature, na.rm = TRUE)
  
  do_min <- min(week$dissolved.oxygen, na.rm = TRUE)
  do_max <- max(week$dissolved.oxygen, na.rm = TRUE)
  
  cat("\n", site_name, "\n")
  cat("Temperature:", round(t_min, 1), "-", round(t_max, 1), "\n")
  cat("DO:", round(do_min, 1), "-", round(do_max, 1), "\n")
  
  # ----------------------------------------------------------
  # Suitability functions
  # ----------------------------------------------------------
  
  si_temp <- function(t) {
    pmin(
      pmax(
        (t_max - t) / (t_max - t_min),
        0
      ),
      1
    )
  }
  
  si_do <- function(d) {
    pmin(
      pmax(
        (d - do_min) / (do_max - do_min),
        0
      ),
      1
    )
  }
  
  # ----------------------------------------------------------
  # Create suitability surface
  # ----------------------------------------------------------
  
  grid <- expand.grid(
    temperature = seq(
      t_min,
      t_max,
      length.out = 200
    ),
    DO = seq(
      do_min,
      do_max,
      length.out = 200
    )
  )
  
  grid$SI_temp <- si_temp(grid$temperature)
  grid$SI_DO <- si_do(grid$DO)
  
  grid$TDObal <- sqrt(
    grid$SI_temp * grid$SI_DO
  )
  
  # ----------------------------------------------------------
  # Plot
  # ----------------------------------------------------------
  
  p <- ggplot(
    grid,
    aes(
      x = temperature,
      y = DO,
      fill = TDObal
    )
  ) +
    geom_raster(interpolate = TRUE) +
    geom_contour(
      data = grid,
      aes(
        x = temperature,
        y = DO,
        z = TDObal
      ),
      inherit.aes = FALSE,
      colour = "white",
      alpha = 0.4,
      linewidth = 0.3,
      bins = 10
    ) +
    scale_fill_viridis_c(
      limits = c(0, 1),
      option = "viridis",
      name = expression(TDO[bal])
    ) +
    labs(
      x = expression("Temperature (" * degree * C * ")"),
      y = expression(
        "Dissolved oxygen (mg L"^-1 * ")"
      )
    ) +
    
    coord_cartesian(expand = FALSE) +
    theme_minimal(base_size = 18) +
    theme(
      panel.grid = element_blank(),
      text = element_text(family = "serif"),
      legend.key.height = unit(1.2, "cm")
    )
  
  return(p)
}

# ------------------------------------------------------------
# Blue Ruin
# ------------------------------------------------------------

p_br <- make_tdobal_heatmap(
  site_name = "Blue Ruin",
  file_in =
    "data/modif.data/logger.array/br.unif.do.temp.set.depth.simulation.csv"
)

# ------------------------------------------------------------
# Norwood
# ------------------------------------------------------------

p_nw <- make_tdobal_heatmap(
  site_name = "Norwood",
  file_in =
    "data/modif.data/logger.array/nor.unif.do.temp.set.depth.simulation.csv"
)

# ------------------------------------------------------------
# Combine panels
# ------------------------------------------------------------

tdobal_fig <- ggarrange(
  p_br,
  p_nw,
  ncol = 2,
  nrow = 1,
  labels = c("A", "B"),
  label.x = 0.02,
  label.y = 1,
  font.label = list(
    family = "serif",
    face = "bold",
    size = 22
  ),
  align = "hv",
  common.legend = TRUE,
  legend = "right"
)

# ------------------------------------------------------------
# Save combined figure
# ------------------------------------------------------------

ggsave(
  filename = "final figures/supplemental figures/TDObal_heatmaps.png",
  plot = tdobal_fig,
  width = 20,
  height = 12,
  units = "cm",
  dpi = 300
)

print(tdobal_fig)
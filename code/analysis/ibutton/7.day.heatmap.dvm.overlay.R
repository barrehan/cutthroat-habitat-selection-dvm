library(tidyverse)
library(lubridate)
library(ggpubr)

# -----------------------------
# Load fish depth data
# -----------------------------
fish <- read_csv("data/modif.data/ibutton/all.ib.interp.depth.csv") %>%
  mutate(date.time = parse_date_time(date.time, orders = "mdy HM", tz = "America/Los_Angeles"),
         date = as.Date(date.time),
         hour_numeric = hour(date.time) + minute(date.time)/60) %>%
  filter(case == 1,
         date.time >= as.POSIXct("2021-08-01", tz = "America/Los_Angeles"),
         date.time <  as.POSIXct("2021-08-06", tz = "America/Los_Angeles"))

norwood_fish <- fish %>% filter(site == "norwood")
blueruin_fish <- fish %>% filter(site == "blue.ruin")

# -----------------------------
# Load logger array data
# -----------------------------

file_path <- "data/modif.data/logger.array/br.unif.do.temp.set.depth.simulation.csv"
# file_path <- "data/modif.data/logger.array/nor.unif.do.temp.set.depth.simulation.csv"

# Auto-detect site
site_name <- if (grepl("br\\.", file_path)) "blue.ruin" else "norwood"
site_label <- gsub("\\.", "", site_name)

# Load logger (environmental) data
array <- read.csv(file_path)
array <- array[, 1:4]  # columns: date.time, depth, temperature, dissolved.oxygen
array$date.time <- ymd_hms(array$date.time, tz = "America/Los_Angeles")
array$hour <- hour(array$date.time) + minute(array$date.time)/60
array$date <- as.Date(array$date.time)
array <- array %>% drop_na(depth)

# -----------------------------
# Prepare fish data for this site
# -----------------------------
fish_site <- fish %>% filter(site == site_name)

# -----------------------------
# Shared scale limits
# -----------------------------
temp_limits <- c(12, 22)
do_limits   <- c(1, 10)

# -----------------------------
# Loop through each available day
# -----------------------------
dates <- sort(unique(array$date))

for (d in dates) {
  # Subset to one day for logger + fish
  day_env  <- array %>% filter(date == d)
  day_fish <- fish_site %>% filter(date == d)
  
  if (nrow(day_env) == 0 | nrow(day_fish) == 0) next
  
  # --- Temperature panel ---
  p_temp <- ggplot() +
    geom_tile(data = day_env, aes(x = hour, y = depth, fill = temperature)) +
    scale_fill_gradientn(
      name = "Temp (°C)",
      colors = c("#4575B4", "#91BFDB", "#D9F0F3", "#FFFFBF",
                 "#FEE08B", "#FC8D59", "#D73027"),
      limits = temp_limits, oob = scales::squish) +
    geom_smooth(data = day_fish, aes(x = hour_numeric, y = depth),
                method = "loess", span = 0.4, se = FALSE,
                color = "black", size = 1.1) +
    scale_y_reverse(limits = if (site_name == "norwood") c(1.5, NA) else c(1.6, NA))+
    scale_x_continuous(breaks = seq(0,24,4), limits = c(0,24)) +
    labs(title = paste(site_label, "-", d, "Temperature"),
         x = "Hour of Day", y = "Depth (m)") +
    theme_minimal(base_size = 14) +
    theme(panel.grid = element_blank(),
          text = element_text(family = "serif"))
  
  # --- DO panel ---
  p_do <- ggplot() +
    geom_tile(data = day_env, aes(x = hour, y = depth, fill = dissolved.oxygen)) +
    scale_fill_viridis_c(name = "DO (mg/L)", option = "inferno",
                         direction = -1, limits = do_limits,
                         oob = scales::squish) +
    geom_smooth(data = day_fish, aes(x = hour_numeric, y = depth),
                method = "loess", span = 0.4, se = FALSE,
                color = "black", size = 1.1) +
    scale_y_reverse() +
    scale_x_continuous(breaks = seq(0,24,4), limits = c(0,24)) +
    labs(title = paste(site_label, "-", d, "Dissolved Oxygen"),
         x = "Hour of Day", y = "Depth (m)") +
    theme_minimal(base_size = 14) +
    theme(panel.grid = element_blank(),
          text = element_text(family = "serif"))
  
  # Combine and show
  combined <- ggarrange(p_temp, p_do, ncol = 2)
  print(combined)
  
  # Optional save
  # ggsave(paste0("results/figures/ibutton.simulation/", site_label, "_", d, ".png"),
  #        combined, width = 14, height = 7, dpi = 300)
}

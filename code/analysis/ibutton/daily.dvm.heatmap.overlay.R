library(tidyverse)
library(lubridate)
library(ggpubr)

# -----------------------------
# Load fish depth data
# -----------------------------
fish <- read_csv("data/modif.data/ibutton/all.ib.interp.depth.csv") %>%
  mutate(
    date.time = parse_date_time(date.time, orders = "mdy HM", tz = "America/Los_Angeles"),
    date = as.Date(date.time),
    hour_numeric = hour(date.time) + minute(date.time) / 60
  ) %>%
  filter(
    case == 1,
    date.time >= as.POSIXct("2021-07-30", tz = "America/Los_Angeles"),
    date.time <  as.POSIXct("2021-08-06", tz = "America/Los_Angeles")
  )

# -----------------------------
# Choose site
# -----------------------------
file_path <- "data/modif.data/logger.array/br.unif.do.temp.set.depth.simulation.csv"
# file_path <- "data/modif.data/logger.array/nor.unif.do.temp.set.depth.simulation.csv"

site_name  <- if (grepl("br\\.", file_path)) "blue.ruin" else "norwood"
site_label <- gsub("\\.", "", site_name)
fish_site  <- fish %>% filter(site == site_name)

# -----------------------------
# Load logger array data
# -----------------------------
array <- read.csv(file_path)
array <- array[, 1:4]  # expected columns: date.time, depth, temperature, dissolved.oxygen
array$date.time <- ymd_hms(array$date.time, tz = "America/Los_Angeles")
array$hour <- hour(array$date.time) + minute(array$date.time) / 60
array$date <- as.Date(array$date.time)
array <- array %>% drop_na(depth)

# -----------------------------
# Choose day(s) to plot manually
# -----------------------------
date_range <- as.Date("2021-08-05")   # <— change this date manually

day_env <- array %>%
  filter(date.time >= as.POSIXct(paste0(date_range, " 00:00:00"), tz = "America/Los_Angeles"),
         date.time <  as.POSIXct(paste0(date_range + 1, " 00:00:00"), tz = "America/Los_Angeles"))

day_fish <- fish_site %>%
  filter(date.time >= as.POSIXct(paste0(date_range, " 00:00:00"), tz = "America/Los_Angeles"),
         date.time <  as.POSIXct(paste0(date_range + 1, " 00:00:00"), tz = "America/Los_Angeles"))

# -----------------------------
# Shared scales
# -----------------------------
temp_limits <- c(12, 22)
do_limits   <- c(1, 10)


# -----------------------------
# Fit loess once outside plot
# -----------------------------

fit <- loess(depth ~ hour_numeric, data = day_fish, span = 0.4)
pred <- data.frame(
  hour_numeric = seq(0, 24, length.out = 200)
)
pred$depth <- predict(fit, newdata = pred)

# -----------------------------
# Temperature panel
# -----------------------------

p_temp <- ggplot() +
  geom_tile(data = day_env, aes(x = hour, y = depth, fill = temperature)) +
  scale_fill_gradientn(
    name = "Temp (°C)",
    colors = c("#4575B4", "#91BFDB", "#D9F0F3", "#FFFFBF",
               "#FEE08B", "#FC8D59", "#D73027"),
    limits = c(12, 22), oob = scales::squish
  ) +
  geom_line(data = pred, aes(x = hour_numeric, y = depth),
            color = "black", linewidth = 1.1) +
  scale_y_reverse(limits = if (site_name == "norwood") c(1.5, NA) else c(1.6, NA))+
  scale_x_continuous(breaks = seq(0, 24, 4), limits = c(0, 24)) +
  labs(x = "Hour of Day", y = "Depth (m)") +
  theme_minimal(base_size = 20) +
  theme(panel.grid = element_blank(),
        text = element_text(family = "serif"))


# -----------------------------
# Dissolved Oxygen panel
# -----------------------------

p_do <- ggplot() +
  geom_tile(data = day_env, aes(x = hour, y = depth, fill = dissolved.oxygen)) +
  scale_fill_viridis_c(name = "DO (mg/L)", option = "inferno",
                       direction = -1, limits = c(1, 10),
                       oob = scales::squish) +
  geom_line(data = pred, aes(x = hour_numeric, y = depth),
            color = "black", linewidth = 1.1) +
  scale_y_reverse(limits = if (site_name == "norwood") c(1.5, NA) else c(1.6, NA))+
  scale_x_continuous(breaks = seq(0, 24, 4), limits = c(0, 24)) +
  labs(x = "Hour of Day", y = "Depth (m)") +
  theme_minimal(base_size = 20) +
  theme(panel.grid = element_blank(),
        text = element_text(family = "serif"))


# -----------------------------
# Combine and print
# -----------------------------
combined <- ggarrange(p_temp, p_do, ncol = 2)

combined <- annotate_figure(
  combined,
  top = text_grob(
    paste(date_range),
    family = "serif", face = "bold", size = 20
  )
)

print(combined)


ggsave(paste0("results/figures/ibutton/", site_label, "_", date_range, ".png"),
        combined, width = 14, height = 7, dpi = 300)

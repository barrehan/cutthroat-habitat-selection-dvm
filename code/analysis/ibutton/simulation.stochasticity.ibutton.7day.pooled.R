library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(readr)
library(ggpubr)

# -------------------------------------
# SECTION 1: Load Simulation File and Detect Site
# -------------------------------------

# Set simulation input file
# file_path <- "data/modif.data/logger.array/br.unif.do.temp.set.depth.simulation.csv"
file_path <- "data/modif.data/logger.array/nor.unif.do.temp.set.depth.simulation.csv"

# Auto-detect site name
site_name <- if (grepl("br\\.", file_path)) "blue.ruin" else "norwood"
site_label <- gsub("\\.", "", site_name)

# Load and preprocess
array <- read.csv(file_path)
array <- array[, c(1:4)]
array$date.time <- ymd_hms(array$date.time)
array$hour <- hour(array$date.time)
array$date <- as.Date(array$date.time)

# Filter for a single day (e.g., August 1)
array$date.time <- ymd_hms(array$date.time, tz = "America/Los_Angeles")
day <- array[array$date.time >= "2021-08-01 00:00:00" & array$date.time < "2021-08-02 00:00:00", ]
day <- day %>% drop_na(depth)

# Unique depth and time values
di <- unique(day$depth)
ti <- unique(day$hour)

# Aggregate by hour and depth
dat <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("hour", "depth", "temperature", "dissolved.oxygen"))
cntr <- 0
for (i in 1:length(di)) {
  dep <- di[i]
  match <- day[day$depth == dep, ]
  for (j in 1:length(ti)) {
    time <- ti[j]
    t.match <- match[match$hour == time, ]
    t <- round(mean(t.match$temperature, na.rm = TRUE), 1)
    do <- round(mean(t.match$dissolved.oxygen, na.rm = TRUE), 1)
    cntr <- cntr + 1
    dat[cntr, ] <- list(time, dep, t, do)
  }
}

# Drop any rows with NA values
dat <- na.omit(dat)

# Calculate temperature and DO factors
t_min <- min(dat$temperature, na.rm = TRUE)
t_max <- max(dat$temperature, na.rm = TRUE)
do_min <- min(dat$dissolved.oxygen, na.rm = TRUE)
do_max <- max(dat$dissolved.oxygen, na.rm = TRUE)

t.fit <- lm(c(1, 0) ~ c(t_min, t_max))
do.fit <- lm(c(1, 0) ~ c(do_max, do_min))

t.cept <- t.fit$coef[1]
t.slope <- t.fit$coef[2]
do.cept <- do.fit$coef[1]
do.slope <- do.fit$coef[2]

dat$temp.fact <- round((t.slope * dat$temperature) + t.cept, 2)
dat$do.fact <- round((do.slope * dat$dissolved.oxygen) + do.cept, 2)
dat$WQI <- round(((dat$temp.fact * dat$do.fact)^0.5), 2)

# -------------------------------------
# SECTION 2: Run Stochastic Simulation
# -------------------------------------

sample_depth <- function(slice, varname, reps = 100, sharpness = 4) {
  scores <- slice[[varname]]
  scores <- ifelse(is.na(scores), 0, scores)
  scores <- scores^sharpness
  probs <- scores / sum(scores, na.rm = TRUE)
  depths <- slice$depth
  sampled <- sample(depths, size = reps, replace = TRUE, prob = probs)
  return(sampled)
}

set.seed(123)
reps <- 100000
sharpness_value <- 20
resamp_list <- list()

for (h in unique(dat$hour)) {
  slice <- dat[dat$hour == h, ]
  t_samples   <- sample_depth(slice, "temp.fact", reps, sharpness = sharpness_value)
  do_samples  <- sample_depth(slice, "do.fact", reps, sharpness = sharpness_value)
  wqi_samples <- sample_depth(slice, "WQI", reps, sharpness = sharpness_value)
  resamp_list[[as.character(h)]] <- data.frame(
    Hour = h,
    Strategy = rep(c("Tmin", "DOmax", "TDOopt"), each = reps),
    Depth = c(t_samples, do_samples, wqi_samples)
  )
}

depth_samples <- do.call(rbind, resamp_list)

depth_summary <- depth_samples %>%
  group_by(Hour, Strategy) %>%
  summarize(
    mean_depth = mean(Depth),
    lower = quantile(Depth, 0.10),
    upper = quantile(Depth, 0.90),
    .groups = "drop"
  )

# Smooth curves
smooth_ci_loess <- function(df, span = 0.5) {
  df %>%
    group_by(Strategy) %>%
    arrange(Hour) %>%
    mutate(
      smooth_mean  = predict(loess(mean_depth ~ Hour, span = span)),
      smooth_lower = predict(loess(lower ~ Hour, span = span)),
      smooth_upper = predict(loess(upper ~ Hour, span = span))
    ) %>%
    ungroup()
}

depth_summary_smooth <- smooth_ci_loess(depth_summary)

# Plot simulation
p <- ggplot(depth_summary_smooth, aes(x = Hour, linetype = Strategy)) +
  # Smoothed 95% confidence ribbon
  geom_ribbon(
    aes(ymin = smooth_lower, ymax = smooth_upper, group = Strategy),
    fill = "#7C5467", alpha = 0.2
  ) +
  
  # Smoothed mean line
  geom_line(aes(y = smooth_mean, color = Strategy), size = 1) +
  
  scale_y_reverse() +
  scale_x_continuous(breaks = seq(0, 24, 2)) +
  scale_linetype_manual(
    values = c("DOmax" = "solid", "TDOopt" = "dotted", "Tmin" = "dashed")
  ) +
  scale_color_manual(
    values = c("DOmax" = "black", "TDOopt" = "black", "Tmin" = "black"),
    guide = "none"  # Remove redundant legend
  ) +
  labs(
    x = "Hour of Day",
    y = "Depth (m)",
    linetype = "Strategy"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    panel.grid = element_blank(),
    legend.key = element_blank(),
    text = element_text(family = "serif")
  )
print(p)

# ggsave(p, filename = paste0("results/figures/ibutton.simulation/", site_label, ".depth.selection.simulation.stochasticity.png"),width = 18, height = 10, units = "cm")

# -------------------------------------
# SECTION 3: Load iButton Data
# -------------------------------------

fish <- read_csv("data/modif.data/ibutton/all.ib.interp.depth.csv") %>%
  mutate(date.time = parse_date_time(date.time, orders = "mdy HM")) %>%
  filter(case == 1,
         date.time >= as.POSIXct("2021-08-01", tz = "America/Los_Angeles"),
         date.time <  as.POSIXct("2021-08-08", tz = "America/Los_Angeles")) %>%
  mutate(
    hour = hour(date.time),
    time = format(date.time, format = "%H:%M:%S"),
    fake.date = "2021-01-01",
    time2 = ymd_hms(paste(fake.date, time)),
    hour_numeric = hour(time2) + minute(time2) / 60
  )

norwood_fish <- fish %>% filter(site == "norwood")
blueruin_fish <- fish %>% filter(site == "blue.ruin")

# Norwood plot
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

# ggsave(p_norwood, filename = "results/figures/ibutton.simulation/smoothed.7day.depth.norwood.png", width = 13, height = 9, units = "cm")
# ggsave(p_blueruin, filename = "results/figures/ibutton.simulation/smoothed.7day.depth.blueruin.png", width = 13, height = 9, units = "cm")

# -------------------------------------
# Heat maps
# -------------------------------------
# Define shared color scale limits
temp_limits <- c(12, 22)
do_limits <- c(1, 10)

# Use Norwood or Blue Ruin fish depending on site
fish_overlay <- if (site_name == "norwood") norwood_fish else blueruin_fish

# Temperature heatmap + smoothed fish depth overlay
p_overlay <- ggplot() +
  geom_tile(data = day, aes(x = hour, y = depth, fill = temperature)) +
  scale_fill_gradientn(
    name = "Temp (°C)",
    colors = c("#4575B4", "#91BFDB", "#D9F0F3", "#FFFFBF", "#FEE08B", "#FC8D59", "#D73027"),
    limits = temp_limits,
    oob = scales::squish,
    breaks = seq(temp_limits[1], temp_limits[2], 2)
  ) +
  geom_smooth(data = fish_overlay, aes(x = hour_numeric, y = depth),
              method = "loess", span = 0.5, se = FALSE,
              color = "black", size = 1.2) +
  scale_y_reverse(limits = if (site_name == "norwood") c(1.5, NA) else c(1.6, NA))+
  scale_x_continuous(breaks = seq(0, 24, 4), limits = c(0, 24), expand = c(0, 0)) +
  labs(x = "Hour of Day", y = "Depth (m)") +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid = element_blank(),
    text = element_text(family = "serif"),
    axis.text.x = element_text(angle = 0)
  )

# DO heatmap + smoothed fish depth overlay
q_overlay <- ggplot() +
  geom_tile(data = day, aes(x = hour, y = depth, fill = dissolved.oxygen)) +
  scale_fill_viridis_c(
    name = "DO (mg/L)",
    option = "inferno",
    direction = -1,
    limits = c(1, 10),
    breaks = seq(2.5, 10, by = 2.5),
    oob = scales::squish,
    na.value = "white"
  )+
  geom_smooth(data = fish_overlay, aes(x = hour_numeric, y = depth),
              method = "loess", span = 0.5, se = FALSE,
              color = "black", size = 1.2) +
  scale_y_reverse(limits = if (site_name == "norwood") c(1.5, NA) else c(1.6, NA))+
  scale_x_continuous(breaks = seq(0, 24, 2), limits = c(0, 24), expand = c(0, 0)) +
  labs(x = "Hour of Day", y = "Depth (m)") +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid = element_blank(),
    text = element_text(family = "serif")
  )

# Combine plots
r <- ggarrange(p_overlay, q_overlay, ncol = 2)

ggsave(r, filename = paste0("results/figures/ibutton.simulation/", site_label, ".heat.map.movement.overlay.png"),width = 25, height = 10, units = "cm")


# ---------------------------------------------------
# SECTION 5: Temperature Range Exposure iButton Fish
# ---------------------------------------------------

# Filter fish data to match the site of the simulation
fish_site <- fish %>% filter(site == site_name)

# Plot observed temperature tracking across 7-day period

p_temp_ibutton <- ggplot(fish_site, aes(x = date.time, y = temperature)) +
  geom_line(aes(group = ibutton.id), alpha = 0.2, color = "gray") +
  geom_smooth(color = "#7C5467", fill = "#7C5467", alpha = 0.3, method = "loess", span = 0.2) +
  labs(
    title = paste("Observed Temperature Tracking by Fish -", site_name),
    x = "Date",
    y = "Temperature (°C)"
  ) +
  theme_minimal(base_size = 14) +
  theme(panel.grid = element_blank(), text = element_text(family = "serif"))

print(p_temp_ibutton)

# Summary statistics of temperature exposure: mean of average daily min/max per fish
ibutton_temp_stats <- fish_site %>%
  mutate(date = as.Date(date.time)) %>%
  group_by(ibutton.id, date) %>%
  summarise(
    daily_min_temp = min(temperature, na.rm = TRUE),
    daily_max_temp = max(temperature, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(ibutton.id) %>%
  summarise(
    avg_daily_min = mean(daily_min_temp, na.rm = TRUE),
    avg_daily_max = mean(daily_max_temp, na.rm = TRUE),
    .groups = "drop"
  )

ibutton_temp_summary <- ibutton_temp_stats %>%
  summarise(
    mean_avg_min_temp = mean(avg_daily_min, na.rm = TRUE),
    mean_avg_max_temp = mean(avg_daily_max, na.rm = TRUE)
  )

print(ibutton_temp_summary)

# Boxplot of average daily min and max temperature across fish
ibutton_temp_long <- ibutton_temp_stats %>%
  pivot_longer(cols = c(avg_daily_min, avg_daily_max),
               names_to = "stat_type", values_to = "temp")

p_temp_box <- ggplot(ibutton_temp_long, aes(x = stat_type, y = temp)) +
  geom_boxplot(fill = "#7C5467", alpha = 0.5) +
  labs(
    title = paste("Distribution of Avg Daily Min/Max Temperatures -", site_name),
    x = NULL,
    y = "Temperature (°C)"
  ) +
  theme_minimal(base_size = 14) +
  theme(panel.grid = element_blank(), text = element_text(family = "serif"))

print(p_temp_box)

# -------------------------------------
# SECTION 6: Depth Range Comparison
# -------------------------------------

# Average min/max depth per fish
fish_depth_summary <- fish_site %>%
  group_by(ibutton.id) %>%
  summarise(
    fish_min = min(depth, na.rm = TRUE),
    fish_max = max(depth, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  summarise(
    avg_min_depth_obs = mean(fish_min, na.rm = TRUE),
    avg_max_depth_obs = mean(fish_max, na.rm = TRUE)
  )

# Overall simulated depth metrics (TDOopt only, pooled across hours)
overall_sim_depth <- depth_samples %>%
  filter(Strategy == "TDOopt") %>%
  summarise(
    min_depth_sim = min(Depth, na.rm = TRUE),
    max_depth_sim = max(Depth, na.rm = TRUE)
  )

# Combine into one table
depth_summary_compare <- bind_cols(fish_depth_summary, overall_sim_depth)

print(depth_summary_compare)

# Norwood fish stay deeper than simulation - average min depth of 0.692 versus
# simulated min depth of 0.2. For Blue Ruin average minimum depth was 0.475 compared 
# to simulation minimum depth of 0.2. Look at heat maps of Temp/DO - super low DO 
# Blue Ruin (3am-6am) may cause fish to move shallower than in Norwood?

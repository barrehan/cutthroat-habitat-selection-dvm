# ---------------------------------------
# Load packages
# ---------------------------------------
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(zoo)
library(ggpubr)

# ---------------------------------------
# USER SETTINGS 
# ---------------------------------------

site <- "norwood"   # "norwood" or "blueruin"

start_time <- "2021-07-29 00:00:00"
end_time   <- "2021-08-06 00:00:00"   # change for multi-day

plot_mode <- "multiday"  # "singleday" or "multiday"

k_val <- 7   # rolling window

# ---------------------------------------
# File selection 
# ---------------------------------------
if (site == "norwood") {
  file_in <- "data/modif.data/logger.array/nor.unif.do.temp.set.depth.simulation.csv"
} else if (site == "blueruin") {
  file_in <- "data/modif.data/logger.array/br.unif.do.temp.set.depth.simulation.csv"
} else {
  stop("Invalid site selection")
}

# ---------------------------------------
# Read and preprocess
# ---------------------------------------
array <- read.csv(file_in)
array <- array[, c(1:4)]

array$date.time <- ymd_hms(array$date.time, tz = "America/Los_Angeles")
array$datetime_hour <- floor_date(array$date.time, unit = "hour")
array$date <- as.Date(array$date.time)

# ---------------------------------------
# Filter time window 
# ---------------------------------------
week <- array %>%
  filter(
    date.time >= as.POSIXct(start_time, tz = "America/Los_Angeles"),
    date.time <  as.POSIXct(end_time,   tz = "America/Los_Angeles")
  ) %>%
  drop_na(depth)

# ---------------------------------------
# Aggregate (date × hour × depth)
# ---------------------------------------
dat <- week %>%
  group_by(date, datetime_hour, depth) %>%
  summarize(
    temperature = round(mean(temperature, na.rm = TRUE), 1),
    dissolved.oxygen = round(mean(dissolved.oxygen, na.rm = TRUE), 1),
    .groups = "drop"
  )

# ---------------------------------------
# GLOBAL scaling 
# ---------------------------------------
t_min <- min(dat$temperature, na.rm = TRUE)
t_max <- max(dat$temperature, na.rm = TRUE)
do_min <- min(dat$dissolved.oxygen, na.rm = TRUE)
do_max <- max(dat$dissolved.oxygen, na.rm = TRUE)


cat("\n--- Suitability scaling values ---\n")
cat("Temperature:\n")
cat("  1 (best)  =", round(t_min, 2), "°C\n")
cat("  0 (worst) =", round(t_max, 2), "°C\n")

cat("Dissolved Oxygen:\n")
cat("  1 (best)  =", round(do_max, 2), "mg/L\n")
cat("  0 (worst) =", round(do_min, 2), "mg/L\n")
cat("----------------------------------\n\n")


t.fit <- lm(c(1, 0) ~ c(t_min, t_max))
do.fit <- lm(c(1, 0) ~ c(do_max, do_min))

# ---------------------------------------
# Sampling function
# ---------------------------------------
sample_depth <- function(slice, varname, reps = 100, sharpness = 4) {
  scores <- slice[[varname]]
  scores <- ifelse(is.na(scores), 0, scores)
  scores <- scores^sharpness
  probs <- scores / sum(scores, na.rm = TRUE)
  sample(slice$depth, size = reps, replace = TRUE, prob = probs)
}

# ---------------------------------------
# Stochastic simulation
# ---------------------------------------
set.seed(123)
reps <- 100000
sharpness_value <- 20
resamp_list <- list()

for (d in unique(dat$date)) {
  
  day_data <- dat %>% filter(date == d)
  
  day_data <- day_data %>%
    mutate(
      temp.fact = round((t.fit$coef[2] * temperature) + t.fit$coef[1], 2),
      do.fact   = round((do.fit$coef[2] * dissolved.oxygen) + do.fit$coef[1], 2),
      WQI       = round(((temp.fact * do.fact)^0.5), 2)
    )
  
  for (t in unique(day_data$datetime_hour)) {
    
    slice <- day_data %>% filter(datetime_hour == t)
    if (nrow(slice) == 0) next
    
    t_samples   <- sample_depth(slice, "temp.fact", reps, sharpness_value)
    do_samples  <- sample_depth(slice, "do.fact", reps, sharpness_value)
    wqi_samples <- sample_depth(slice, "WQI", reps, sharpness_value)
    
    resamp_list[[as.character(t)]] <- data.frame(
      Time = t,
      Strategy = rep(c("Tmin", "DOmax", "TDObal"), each = reps),
      Depth = c(t_samples, do_samples, wqi_samples)
    )
  }
}

# ---------------------------------------
# Combine samples
# ---------------------------------------
depth_samples <- do.call(rbind, resamp_list)

# ---------------------------------------
# Summarize by time 
# ---------------------------------------
depth_summary <- depth_samples %>%
  group_by(Time, Strategy) %>%
  summarize(
    mean_depth = mean(Depth),
    lower = quantile(Depth, 0.25),
    upper = quantile(Depth, 0.75),
    .groups = "drop"
  ) %>%
  arrange(Strategy, Time)

# ---------------------------------------
# Rolling smoothing 
# ---------------------------------------
depth_summary <- depth_summary %>%
  group_by(Strategy) %>%
  mutate(
    smooth_mean  = rollmean(mean_depth,  k = k_val, fill = NA, align = "center"),
    smooth_lower = rollmean(lower,       k = k_val, fill = NA, align = "center"),
    smooth_upper = rollmean(upper,       k = k_val, fill = NA, align = "center")
  ) %>%
  ungroup()

depth_summary$Time <- as.POSIXct(depth_summary$Time)


# Apply a centered rolling mean (moving average) with a window size of k = 5.
# This means each value is replaced by the average of 5 consecutive time points:
# the current hour plus the two hours before and after (±2 hours).
# For example, the value at time t is calculated as the mean of:
# t-2, t-1, t, t+1, and t+2.
#
# This smooths short-term variability while preserving the timing and
# amplitude of diel vertical migration patterns.


# ---------------------------------------
# Plot adaptive axis
# ---------------------------------------

c <- ggplot(depth_summary, aes(x = Time, linetype = Strategy)) +
  geom_ribbon(
    aes(ymin = smooth_lower, ymax = smooth_upper, group = Strategy),
    fill = "#7C5467", alpha = 0.2
  ) +
  geom_line(aes(y = smooth_mean, color = Strategy), size = 1) +
  scale_y_reverse(
    limits = c(1.6, 0.2),
    breaks = seq(0, 1.6, by = 0.4)
  ) +
  scale_linetype_manual(
    values = c(
      "DOmax" = "solid",
      "TDObal" = "dotted",
      "Tmin" = "dashed"
    )
  ) +
  scale_color_manual(
    values = c(
      "DOmax" = "black",
      "TDObal" = "black",
      "Tmin" = "black"
    ),
    guide = "none"
  ) +
  labs(
    x = ifelse(plot_mode == "singleday", "Hour of Day", "Date"),
    y = "Depth (m)",
    linetype = "Strategy"
  ) +
  theme_minimal(base_size = 22) +
  theme(
    panel.grid = element_blank(),
    legend.key = element_blank(),
    text = element_text(family = "serif"),
    
    # add axes
    axis.line = element_line(colour = "black", linewidth = 0.8),
    axis.ticks = element_line(colour = "black", linewidth = 0.8),
    axis.ticks.length = unit(0.2, "cm")
  )

# # switch x-axis based on mode
# if (plot_mode == "singleday") {
#   p <- p + scale_x_datetime(date_breaks = "2 hours", date_labels = "%H")
# } else {
#   p <- p + scale_x_datetime(date_breaks = "1 day", date_labels = "%b %d")
# }


final.figac <- ggarrange(
  a,
  c,
  ncol = 1,
  nrow = 2,
  labels = c("A", "C"),
  font.label = list(
    family = "serif",
    face = "bold",
    size = 22
  ),
  align = "hv"
)

ggsave(final.figac, filename = paste("final figures/figure 3_multiday.simulation.png"), width = 20, height = 14, units = "cm")



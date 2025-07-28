# Load packages
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)

setwd("C:/Users/barrehan/Documents/GitHub/cwa.habitat.selection.dvm")

# Read and preprocess data
array <- read.csv("data/modif.data/logger.array/nor.unif.do.temp.set.depth.simulation.csv")
array <- array[, c(1:4)]
array$date.time <- ymd_hms(array$date.time, tz = "America/Los_Angeles")
array$datetime_hour <- floor_date(array$date.time, unit = "hour")
array$date <- as.Date(array$date.time)

# Filter for one week of data
week <- array %>%
  filter(date.time >= as.POSIXct("2021-08-01 00:00:00", tz = "America/Los_Angeles"),
         date.time <  as.POSIXct("2021-08-07 00:00:00", tz = "America/Los_Angeles")) %>%
  drop_na(depth)

# Aggregate by hour and depth
dat <- week %>%
  group_by(date, datetime_hour, depth) %>%
  summarize(
    temperature = round(mean(temperature, na.rm = TRUE), 1),
    dissolved.oxygen = round(mean(dissolved.oxygen, na.rm = TRUE), 1),
    .groups = "drop"
  )

# Function to stochastically sample depths
sample_depth <- function(slice, varname, reps = 100, sharpness = 4) {
  scores <- slice[[varname]]
  scores <- ifelse(is.na(scores), 0, scores)
  scores <- scores^sharpness
  probs <- scores / sum(scores, na.rm = TRUE)
  depths <- slice$depth
  sampled <- sample(depths, size = reps, replace = TRUE, prob = probs)
  return(sampled)
}

# Run stochastic simulation
set.seed(123)
reps <- 100000
sharpness_value <- 50
resamp_list <- list()

# Loop over each date
for (d in unique(dat$date)) {
  day_data <- dat %>% filter(date == d)
  
  # Fit suitability curves using that day's range
  t_min <- min(day_data$temperature, na.rm = TRUE)
  t_max <- max(day_data$temperature, na.rm = TRUE)
  do_min <- min(day_data$dissolved.oxygen, na.rm = TRUE)
  do_max <- max(day_data$dissolved.oxygen, na.rm = TRUE)
  
  # Skip if no variation or NAs
  if (t_min == t_max | do_min == do_max | any(is.infinite(c(t_min, t_max, do_min, do_max)))) next
  
  t.fit <- lm(c(1, 0) ~ c(t_min, t_max))
  do.fit <- lm(c(1, 0) ~ c(do_max, do_min))  # Higher DO = better
  
  # Apply suitability scoring
  day_data <- day_data %>%
    mutate(
      temp.fact = round((t.fit$coef[2] * temperature) + t.fit$coef[1], 2),
      do.fact   = round((do.fit$coef[2] * dissolved.oxygen) + do.fit$coef[1], 2),
      WQI       = round(((temp.fact * do.fact)^0.5), 2)
    )
  
  # Loop over each hour in the day
  for (t in unique(day_data$datetime_hour)) {
    slice <- day_data %>% filter(datetime_hour == t)
    if (nrow(slice) == 0) next
    
    t_samples <- sample_depth(slice, "temp.fact", reps, sharpness_value)
    do_samples <- sample_depth(slice, "do.fact", reps, sharpness_value)
    wqi_samples <- sample_depth(slice, "WQI", reps, sharpness_value)
    
    resamp_list[[as.character(t)]] <- data.frame(
      Time = t,
      Strategy = rep(c("Tmin", "DOmax", "TDOopt"), each = reps),
      Depth = c(t_samples, do_samples, wqi_samples)
    )
  }
}

# Combine sampled data
depth_samples <- do.call(rbind, resamp_list)

# Summarize sampled depths: mean + 10–90% CI
depth_summary <- depth_samples %>%
  group_by(Time, Strategy) %>%
  summarize(
    mean_depth = mean(Depth),
    lower = quantile(Depth, 0.10),
    upper = quantile(Depth, 0.90),
    .groups = "drop"
  )

# Smoothing function
smooth_ci_loess <- function(df, span = 0.3) {
  df %>%
    group_by(Strategy) %>%
    arrange(Time) %>%
    mutate(
      smooth_mean = predict(loess(mean_depth ~ as.numeric(Time), span = span)),
      smooth_lower = predict(loess(lower ~ as.numeric(Time), span = span)),
      smooth_upper = predict(loess(upper ~ as.numeric(Time), span = span))
    ) %>%
    ungroup()
}

depth_summary_smooth <- smooth_ci_loess(depth_summary, span = 0.3)

# Ensure Time is POSIXct for proper datetime plotting
depth_summary_smooth$Time <- as.POSIXct(depth_summary_smooth$Time)

# Plot results
p <- ggplot(depth_summary_smooth, aes(x = Time, linetype = Strategy)) +
  geom_ribbon(
    aes(ymin = smooth_lower, ymax = smooth_upper, group = Strategy),
    fill = "#7C5467", alpha = 0.2
  ) +
  geom_line(aes(y = smooth_mean, color = Strategy), size = 1) +
  scale_y_reverse() +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%b %d") +
  scale_linetype_manual(values = c("DOmax" = "solid", "TDOopt" = "dotted", "Tmin" = "dashed")) +
  scale_color_manual(values = c("DOmax" = "black", "TDOopt" = "black", "Tmin" = "black"),
                     guide = "none") +
  labs(
    x = "Date",
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

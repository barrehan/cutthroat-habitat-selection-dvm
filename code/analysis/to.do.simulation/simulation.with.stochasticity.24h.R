# Load packages
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)

# Read data and preprocess
# CHANGE FILE TYPE BETWEEN NORWOOD OR BLUE RUIN
array <- read.csv("data/modif.data/logger.array/nor.unif.do.temp.set.depth.simulation.csv")
# array <- read.csv("data/modif.data/logger.array/br.unif.do.temp.set.depth.simulation.csv")
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
# For a given site and time window
t_min <- min(dat$temperature, na.rm = TRUE)
t_max <- max(dat$temperature, na.rm = TRUE)
do_min <- min(dat$dissolved.oxygen, na.rm = TRUE)
do_max <- max(dat$dissolved.oxygen, na.rm = TRUE)

t.fit <- lm(c(1, 0) ~ c(t_min, t_max)) # Lower temp = better (1), higher = worse (0)
do.fit <- lm(c(1, 0) ~ c(do_max, do_min))  # Higher DO = better (1), lower = worse (0)

t.cept <- t.fit$coef[1]
t.slope <- t.fit$coef[2]
do.cept <- do.fit$coef[1]
do.slope <- do.fit$coef[2]

dat$temp.fact <- round((t.slope * dat$temperature) + t.cept, 2)
dat$do.fact <- round((do.slope * dat$dissolved.oxygen) + do.cept, 2)

# Combined score: temp and DO
dat$WQI <- round(((dat$temp.fact * dat$do.fact)^0.5), 2)

# ---------------------------------------
# Function to stochastically sample depths
# ---------------------------------------
# Applies a sharpness exponent to favor higher-scoring depths
sample_depth <- function(slice, varname, reps = 100, sharpness = 4) {
  scores <- slice[[varname]]
  scores <- ifelse(is.na(scores), 0, scores)
  
  # Sharpen the contrast to bias selection toward higher scores
  scores <- scores^sharpness
  
  # Normalize to get a probability distribution
  probs <- scores / sum(scores, na.rm = TRUE)
  
  depths <- slice$depth
  sampled <- sample(depths, size = reps, replace = TRUE, prob = probs)
  return(sampled)
}

# ---------------------------------------
# Run stochastic simulation
# ---------------------------------------

set.seed(123)            # For reproducibility
reps <- 100000              # Number of replicates per hour per strategy
sharpness_value <- 20     # Adjust sharpness: higher = more biased

resamp_list <- list()    # Storage for all hourly samples

# Loop over each hour in the dataset
for (h in unique(dat$hour)) {
  slice <- dat[dat$hour == h, ]
  
  # Stochastically sample depths for each strategy
  t_samples   <- sample_depth(slice, "temp.fact", reps, sharpness = sharpness_value)
  do_samples  <- sample_depth(slice, "do.fact", reps, sharpness = sharpness_value)
  wqi_samples <- sample_depth(slice, "WQI", reps, sharpness = sharpness_value)
  
  # Combine into long-format dataframe
  resamp_list[[as.character(h)]] <- data.frame(
    Hour = h,
    Strategy = rep(c("Tmin", "DOmax", "TDObal"), each = reps),
    Depth = c(t_samples, do_samples, wqi_samples)
  )
}

# Combine all samples into one dataframe
depth_samples <- do.call(rbind, resamp_list)

# ---------------------------------------
# Summarize sampled depths: mean + 95% CI
# ---------------------------------------
library(dplyr)

depth_summary <- depth_samples %>%
  group_by(Hour, Strategy) %>%
  summarize(
    mean_depth = mean(Depth),
    lower = quantile(Depth, 0.10),
    upper = quantile(Depth, 0.90),
    .groups = "drop"
  )


# Function to smooth all three variables for each strategy
smooth_ci_loess <- function(df, span = 0.5) { #change span value to smooth
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

# Apply smoothing
depth_summary_smooth <- smooth_ci_loess(depth_summary, span = 0.5) #change span value to smooth  

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
    values = c("DOmax" = "solid", "TDObal" = "dotted", "Tmin" = "dashed")
  ) +
  scale_color_manual(
    values = c("DOmax" = "black", "TDObal" = "black", "Tmin" = "black"),
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

# CHANGE SAVE NAME BETWEEN NORWOOD AND BLUE RUIN
ggsave(p, filename = paste("results/figures/ibutton.simulation/nor.depth.selection.simulation.stochasticity.png"), width = 18, height = 10, units = "cm")


# Heatmap of DO by depth and hour
ggplot(day, aes(x = hour, y = depth, fill = dissolved.oxygen)) +
  geom_tile() +
  scale_y_reverse() +
  scale_fill_viridis_c() +
  labs(title = "DO (mg/L) by depth and hour")

ggplot(day, aes(x = hour, y = depth, fill = temperature)) +
  geom_tile() +
  scale_y_reverse() +
  scale_fill_viridis_c() +
  labs(title = "Temperature by depth and hour")

rm(list=ls())

library(ggplot2)
library(dplyr)
library(grid)
library(gridExtra)
library(survival)
library(lubridate)
library(mgcv)
library(hms)

# -------------------------
# 1) Load Blue Ruin dataset
# -------------------------
br.ib <- read.csv("data/modif.data/hab.select.mod/br.ibutton.data/br.ibutton.pooled.csv")

# Parse datetime, set tz, pick same broader window as Norwood (then select by clock windows)
br.ib$date.time <- ymd_hms(br.ib$date.time)
br.ib <- br.ib %>% force_tz(br.ib$date.time, tzone = "America/Los_Angeles")
br.ib <- br.ib[br.ib$date.time >= "2021-07-30 00:00:00" & br.ib$date.time < "2021-08-06 00:00:00",]

# If file has legacy high/low cols you dropped before, remove them (safe-guard)
if (ncol(br.ib) >= 8) {
  br.ib <- br.ib[, -7:-8]
}

# Ensure 'site' exists
if (!"site" %in% names(br.ib)) {
  br.ib$site <- "Blue Ruin"
}

# Helpful derived columns
br.ib <- br.ib %>%
  mutate(
    date = as_date(date.time),
    time = hms::as_hms(format(date.time, "%H:%M:%S")),
    period = case_when(
      time >= hms::as_hms("09:00:00") & time < hms::as_hms("21:00:00") ~ "day",
      TRUE ~ "night"
    ),
    hour_of_day = hour(date.time) * 3600 + minute(date.time) * 60 + second(date.time)
  )

# -------------------------
# 2) Deepest-by-day & shallowest-by-night (per fish x date)
# -------------------------
deepest_day <- br.ib %>%
  filter(period == "day") %>%
  group_by(site, date, ibutton.id) %>%
  slice_max(order_by = depth, n = 1, with_ties = FALSE) %>%
  transmute(site, date, ibutton.id,
            deepest_time = time,
            deepest_depth = depth)

shallowest_night <- br.ib %>%
  filter(period == "night") %>%
  group_by(site, date, ibutton.id) %>%
  slice_min(order_by = depth, n = 1, with_ties = FALSE) %>%
  transmute(site, date, ibutton.id,
            shallowest_time = time,
            shallowest_depth = depth)

# -------------------------
# 3) Per-fish means, then site means (+ SD)
# -------------------------
deep_per_fish <- deepest_day %>%
  group_by(site, ibutton.id) %>%
  summarise(
    mean_deep_time  = mean(as.numeric(deepest_time), na.rm = TRUE),
    mean_deep_depth = mean(deepest_depth, na.rm = TRUE),
    n_days          = n(),
    .groups = "drop"
  ) %>%
  mutate(mean_deep_time = hms::as_hms(mean_deep_time))

shallow_per_fish <- shallowest_night %>%
  group_by(site, ibutton.id) %>%
  summarise(
    mean_shallow_time  = mean(as.numeric(shallowest_time), na.rm = TRUE),
    mean_shallow_depth = mean(shallowest_depth, na.rm = TRUE),
    n_days             = n(),
    .groups = "drop"
  ) %>%
  mutate(mean_shallow_time = hms::as_hms(mean_shallow_time))

site_time_summary <- deep_per_fish %>%
  group_by(site) %>%
  summarise(
    n_fish                = n(),
    site_mean_deep_time   = hms::as_hms(mean(as.numeric(mean_deep_time), na.rm = TRUE)),
    site_sd_deep_time     = sd(as.numeric(mean_deep_time), na.rm = TRUE),
    site_mean_deep_depth  = mean(mean_deep_depth, na.rm = TRUE),
    site_sd_deep_depth    = sd(mean_deep_depth, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    shallow_per_fish %>%
      group_by(site) %>%
      summarise(
        n_fish                    = n(),
        site_mean_shallow_time    = hms::as_hms(mean(as.numeric(mean_shallow_time), na.rm = TRUE)),
        site_sd_shallow_time      = sd(as.numeric(mean_shallow_time), na.rm = TRUE),
        site_mean_shallow_depth   = mean(mean_shallow_depth, na.rm = TRUE),
        site_sd_shallow_depth     = sd(mean_shallow_depth, na.rm = TRUE),
        .groups = "drop"
      ),
    by = "site",
    suffix = c("_deep", "_shallow")
  ) %>%
  mutate(
    site_sd_deep_time    = hms::as_hms(site_sd_deep_time),
    site_sd_shallow_time = hms::as_hms(site_sd_shallow_time)
  )

print(site_time_summary)

# Extract site-average centers (seconds since midnight)
deep_center    <- as.numeric(site_time_summary$site_mean_deep_time[site_time_summary$site == "Blue Ruin"])
shallow_center <- as.numeric(site_time_summary$site_mean_shallow_time[site_time_summary$site == "Blue Ruin"])

# -------------------------
# 4) Build ±1-hour windows around those center times (circular on 24h)
# -------------------------
within_window <- function(hod_seconds, center_seconds, half_width = 3600) {
  d <- abs(hod_seconds - center_seconds)
  d <- pmin(d, 86400 - d)
  d <= half_width
}

high.int <- br.ib %>% filter(within_window(hour_of_day, deep_center,    half_width = 3600))  # DO-maxima/daytime proxy
low.int  <- br.ib %>% filter(within_window(hour_of_day, shallow_center, half_width = 3600))  # DO-minima/nighttime proxy

# -------------------------
# 5) Standardize covariates on full Blue Ruin dataset, apply to windows
# -------------------------
mean.t  <- mean(br.ib$temperature, na.rm = TRUE)
sd.t    <- sd(br.ib$temperature, na.rm = TRUE)
mean.do <- mean(br.ib$dissolved.oxygen, na.rm = TRUE)
sd.do   <- sd(br.ib$dissolved.oxygen, na.rm = TRUE)

high.int <- high.int %>%
  mutate(
    standardized.do   = as.numeric(scale(dissolved.oxygen, center = mean.do, scale = sd.do)),
    standardized.temp = as.numeric(scale(temperature,       center = mean.t,  scale = sd.t))
  )

low.int <- low.int %>%
  mutate(
    standardized.do   = as.numeric(scale(dissolved.oxygen, center = mean.do, scale = sd.do)),
    standardized.temp = as.numeric(scale(temperature,       center = mean.t,  scale = sd.t))
  )

# case/available flag
high.int$dumt <- 1L
low.int$dumt  <- 1L

# -------------------------
# 6) Fit GAMs (same spec as yours)
# -------------------------
gam.high <- gam(
  cbind(dumt, stratID) ~ s(standardized.do, standardized.temp, bs = "ts"),
  data = high.int,
  family = cox.ph, weights = case,
  method = "REML", gamma = 1.6, select = TRUE
)
summary(gam.high)

gam.low <- gam(
  cbind(dumt, stratID) ~ s(standardized.do, standardized.temp, bs = "ts"),
  data = low.int,
  family = cox.ph, weights = case,
  method = "REML", gamma = 1.6, select = TRUE
)
summary(gam.low)

# -------------------------
# 7) Average-effect helper (unchanged)
# -------------------------
calc_avg_effect <- function(model, variable, mean_x, sd_x, bandwidth = NULL) {
  model_data <- model$model
  available_indices <- model_data$`(weights)` == 0
  used_indices      <- model_data$`(weights)` == 1
  
  available_data <- model_data |> filter(available_indices)
  available_vals <- available_data[[variable]]
  
  used_data <- model_data |> filter(used_indices)
  used_vals <- used_data[[variable]]
  used_vals <- data.frame(used_vals = (used_vals * sd_x) + mean_x)
  
  fitted_values <- predict(model, newdata = available_data, type = "link")
  w_x <- exp(fitted_values)
  
  if (is.null(bandwidth)) {
    smooth_fit <- ksmooth(available_vals, w_x, kernel = "normal")
  } else {
    smooth_fit <- ksmooth(available_vals, w_x, kernel = "normal", bandwidth = bandwidth)
  }
  
  n_boot <- 200
  n_points <- length(smooth_fit$x)
  boot_fits <- matrix(NA, nrow = n_boot, ncol = n_points)
  
  for (i in 1:n_boot) {
    boot_idx <- sample(length(available_vals), replace = TRUE)
    boot_covar <- available_vals[boot_idx]
    boot_w <- w_x[boot_idx]
    if (is.null(bandwidth)) {
      boot_smooth <- ksmooth(boot_covar, boot_w, kernel = "normal", x.points = smooth_fit$x)
    } else {
      boot_smooth <- ksmooth(boot_covar, boot_w, kernel = "normal", bandwidth = bandwidth, x.points = smooth_fit$x)
    }
    boot_fits[i, ] <- boot_smooth$y
  }
  
  ci_lower <- apply(boot_fits, 2, quantile, probs = 0.025, na.rm = TRUE)
  ci_upper <- apply(boot_fits, 2, quantile, probs = 0.975, na.rm = TRUE)
  
  plot_data <- data.frame(
    fit   = smooth_fit$y,
    x_std = smooth_fit$x,
    x     = (smooth_fit$x * sd_x) + mean_x,
    upper = ci_upper,
    lower = ci_lower
  )
  return(list(plot_data = plot_data, used_vals = used_vals))
}

# -------------------------
# 8) Average effect function and plots with standardized axes
# -------------------------
# HIGH / DO
do_br_high <- calc_avg_effect(gam.high, "standardized.do",  mean_x = mean.do, sd_x = sd.do)

# HIGH / TEMP
temp_br_high <- calc_avg_effect(gam.high, "standardized.temp", mean_x = mean.t, sd_x = sd.t)

# LOW / DO
do_br_low <- calc_avg_effect(gam.low, "standardized.do", mean_x = mean.do, sd_x = sd.do)

# LOW / TEMP
temp_br_low <- calc_avg_effect(gam.low, "standardized.temp", mean_x = mean.t, sd_x = sd.t)


# HIGH / DO
br_do_high <- ggplot(do_br_high$plot_data, aes(x = x, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "steelblue4") +
  geom_line(color = "steelblue4", size = 1) +
  geom_rug(aes(x = x), color = "gray60", alpha = .1, size = 0.1, sides = "b") +
  geom_rug(data = do_br_high$used_vals, aes(x = used_vals),
           inherit.aes = FALSE, color = "steelblue4", alpha = .7, size = 0.1, sides = "b") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 14, family = "serif")) +
  labs(x = "Dissolved Oxygen (mg/L)", y = NULL) +
  ylim(c(0, max(temp_br_high$plot_data$fit))) +
  xlim(c(min(do_br_high$plot_data$x, na.rm = TRUE), max(do_br_high$plot_data$x, na.rm = TRUE)))


# HIGH / TEMP
br_temp_high <- ggplot(temp_br_high$plot_data, aes(x = x, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "aquamarine4") +
  geom_line(color = "aquamarine4", size = 1) +
  geom_rug(aes(x = x), color = "gray60", alpha = .1, size = 0.1, sides = "b") +
  geom_rug(data = temp_br_high$used_vals, aes(x = used_vals),
           inherit.aes = FALSE, color = "aquamarine4", alpha = .7, size = 0.1, sides = "b") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 14, family = "serif")) +
  labs(x = "Temperature (\u00B0C)", y = NULL) +
  ylim(c(0, max(temp_br_high$plot_data$fit))) +
  xlim(c(min(temp_br_high$plot_data$x, na.rm = TRUE), max(temp_br_high$plot_data$x, na.rm = TRUE)))


# LOW / DO
br_do_low <- ggplot(do_br_low$plot_data, aes(x = x, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "steelblue4") +
  geom_line(color = "steelblue4", size = 1) +
  geom_rug(aes(x = x), color = "gray60", alpha = .1, size = 0.1, sides = "b") +
  geom_rug(data = do_br_low$used_vals, aes(x = used_vals),
           inherit.aes = FALSE, color = "steelblue4", alpha = .7, size = 0.1, sides = "b") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 14, family = "serif")) +
  labs(x = "Dissolved Oxygen (mg/L)", y = NULL) +
  ylim(c(0, max(temp_br_low$plot_data$fit))) +
  xlim(c(min(do_br_low$plot_data$x, na.rm = TRUE), max(do_br_low$plot_data$x, na.rm = TRUE)))


# LOW / TEMP
br_temp_low <- ggplot(temp_br_low$plot_data, aes(x = x, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "aquamarine4") +
  geom_line(color = "aquamarine4", size = 1) +
  geom_rug(aes(x = x), color = "gray60", alpha = .1, size = 0.1, sides = "b") +
  geom_rug(data = temp_br_low$used_vals, aes(x = used_vals),
           inherit.aes = FALSE, color = "aquamarine4", alpha = .7, size = 0.1, sides = "b") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 14, family = "serif")) +
  labs(x = "Temperature (\u00B0C)", y = NULL) +
  ylim(c(0, max(temp_br_low$plot_data$fit))) +
  xlim(c(min(temp_br_low$plot_data$x, na.rm = TRUE), max(temp_br_low$plot_data$x, na.rm = TRUE)))

# Combine panels
br_high <- grid.arrange(
  br_do_high, br_temp_high,
  nrow = 1,
  top = textGrob("DO-maxima", gp = gpar(fontsize = 16, fontfamily = "serif"))
)
br_low <- grid.arrange(
  br_do_low, br_temp_low,
  nrow = 1,
  top = textGrob("DO-minima", gp = gpar(fontsize = 16, fontfamily = "serif"))
)
all <- grid.arrange(
  br_high, br_low,
  nrow = 2,
  left = textGrob("Relative Probability of Use",
                  rot = 90,
                  gp = gpar(fontsize = 16, fontfamily = "serif"))
)

ggsave("results/figures/blue-ruin-average-effect-plot-deepest-shallowest.png", plot = all, width = 8, height = 9, dpi = 300)                    


# -------------------------
# 9) AIC comparisons (temp-only, DO-only, both)
# -------------------------
fit_aic_set <- function(dat, label) {
  if (!"dumt" %in% names(dat)) dat$dumt <- 1L
  
  m_temp <- gam(
    cbind(dumt, stratID) ~ s(standardized.temp, bs = "ts"),
    data = dat, family = cox.ph, weights = case,
    method = "REML", gamma = 1.6, select = TRUE
  )
  m_do <- gam(
    cbind(dumt, stratID) ~ s(standardized.do, bs = "ts"),
    data = dat, family = cox.ph, weights = case,
    method = "REML", gamma = 1.6, select = TRUE
  )
  m_both <- gam(
    cbind(dumt, stratID) ~ s(standardized.do, standardized.temp, bs = "ts"),
    data = dat, family = cox.ph, weights = case,
    method = "REML", gamma = 1.6, select = TRUE
  )
  tab <- data.frame(
    Window = label,
    Model = c("Temperature only", "DO only", "Both (bivariate)"),
    AIC   = c(AIC(m_temp), AIC(m_do), AIC(m_both)),
    edf   = c(sum(m_temp$edf), sum(m_do$edf), sum(m_both$edf)),
    row.names = NULL
  )
  tab$deltaAIC <- tab$AIC - min(tab$AIC)
  tab[order(tab$AIC), ]
}

aic_high <- fit_aic_set(high.int, "DO-maxima (high)")
aic_low  <- fit_aic_set(low.int,  "DO-minima (low)")
aic_table <- rbind(aic_high, aic_low)
print(aic_table)

# Optional: save to CSV
dir.create("results/tables", showWarnings = FALSE, recursive = TRUE)
write.csv(aic_table, "results/tables/blue-ruin-gam-aic-comparison-deepest-shallowest.csv", row.names = FALSE)


    
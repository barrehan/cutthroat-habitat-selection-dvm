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
# 1) Load Norwood dataset
# -------------------------
nor.ib <- read.csv("data/modif.data/hab.select.mod/norwood.ibutton.data/norwood.ibutton.pooled.csv")

# Parse datetime, force tz, keep same date window as before
nor.ib$date.time <- ymd_hms(nor.ib$date.time)
nor.ib <- nor.ib %>% force_tz(nor.ib$date.time, tzone = "America/Los_Angeles")
nor.ib <- nor.ib[nor.ib$date.time >= "2021-07-30 00:00:00" & nor.ib$date.time < "2021-08-06 00:00:00",]

# If your file had extra columns like the old high/low labels, drop them (as before)
if (ncol(nor.ib) >= 8) {
  nor.ib <- nor.ib[, -7:-8]
}

# Exclude problematic fish (as before)
nor.ib <- subset(nor.ib, ibutton.id != 18)

# Ensure 'site' exists (some Norwood-only files may omit it)
if (!"site" %in% names(nor.ib)) {
  nor.ib$site <- "Norwood"
}

# Helpful derived columns
nor.ib <- nor.ib %>%
  mutate(
    date = as_date(date.time),
    time = hms::as_hms(format(date.time, "%H:%M:%S")),
    # day/night ID per your rule
    period = case_when(
      time >= hms::as_hms("09:00:00") & time < hms::as_hms("21:00:00") ~ "day",
      TRUE ~ "night"
    ),
    # seconds since midnight for windowing
    hour_of_day = hour(date.time) * 3600 + minute(date.time) * 60 + second(date.time)
  )

# -------------------------
# 2) Deepest-by-day & shallowest-by-night
# -------------------------
deepest_day <- nor.ib %>%
  filter(period == "day") %>%
  group_by(site, date, ibutton.id) %>%
  slice_max(order_by = depth, n = 1, with_ties = FALSE) %>%
  transmute(site, date, ibutton.id,
            deepest_time = time,
            deepest_depth = depth)

shallowest_night <- nor.ib %>%
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
    mean_deep_time   = mean(as.numeric(deepest_time), na.rm = TRUE),
    mean_deep_depth  = mean(deepest_depth, na.rm = TRUE),
    n_days           = n(),
    .groups = "drop"
  ) %>%
  mutate(mean_deep_time = hms::as_hms(mean_deep_time))

shallow_per_fish <- shallowest_night %>%
  group_by(site, ibutton.id) %>%
  summarise(
    mean_shallow_time   = mean(as.numeric(shallowest_time), na.rm = TRUE),
    mean_shallow_depth  = mean(shallowest_depth, na.rm = TRUE),
    n_days              = n(),
    .groups = "drop"
  ) %>%
  mutate(mean_shallow_time = hms::as_hms(mean_shallow_time))

site_time_summary <- deep_per_fish %>%
  group_by(site) %>%
  summarise(
    n_fish = n(),
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
        n_fish = n(),
        site_mean_shallow_time   = hms::as_hms(mean(as.numeric(mean_shallow_time), na.rm = TRUE)),
        site_sd_shallow_time     = sd(as.numeric(mean_shallow_time), na.rm = TRUE),
        site_mean_shallow_depth  = mean(mean_shallow_depth, na.rm = TRUE),
        site_sd_shallow_depth    = sd(mean_shallow_depth, na.rm = TRUE),
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

# Extract the Norwood site-average center times (in seconds)
deep_center    <- as.numeric(site_time_summary$site_mean_deep_time[site_time_summary$site == "Norwood"])
shallow_center <- as.numeric(site_time_summary$site_mean_shallow_time[site_time_summary$site == "Norwood"])

# -------------------------
# 4) Build ±1-hour windows around those center times
#    (robust to midnight wrap)
# -------------------------
within_window <- function(hod_seconds, center_seconds, half_width = 3600) {
  # circular distance on 24h clock (86400 sec)
  d <- abs(hod_seconds - center_seconds)
  d <- pmin(d, 86400 - d)
  d <= half_width
}

deep_window <- nor.ib %>%
  filter(within_window(hour_of_day, deep_center, half_width = 3600))

shallow_window <- nor.ib %>%
  filter(within_window(hour_of_day, shallow_center, half_width = 3600))

# -------------------------
# 5) Standardize covariates (as in your original)
#    (do this on the full Norwood dataset so scaling matches both windows)
# -------------------------
nor.ib$standardized.do   <- as.numeric(scale(nor.ib$dissolved.oxygen))
nor.ib$standardized.temp <- as.numeric(scale(nor.ib$temperature))

mean.t  <- mean(nor.ib$temperature, na.rm = TRUE)
sd.t    <- sd(nor.ib$temperature, na.rm = TRUE)
mean.do <- mean(nor.ib$dissolved.oxygen, na.rm = TRUE)
sd.do   <- sd(nor.ib$dissolved.oxygen, na.rm = TRUE)

# Attach standardized columns back to the windowed data
deep_window <- deep_window %>%
  mutate(
    standardized.do   = as.numeric(scale(dissolved.oxygen, center = mean.do, scale = sd.do)),
    standardized.temp = as.numeric(scale(temperature,       center = mean.t,  scale = sd.t))
  )

shallow_window <- shallow_window %>%
  mutate(
    standardized.do   = as.numeric(scale(dissolved.oxygen, center = mean.do, scale = sd.do)),
    standardized.temp = as.numeric(scale(temperature,       center = mean.t,  scale = sd.t))
  )

# -------------------------
# 6) Prepare GAM inputs (replace old high/low 2hr bins)
# -------------------------
# Keep your original additional filters:
# (e.g., remove weird stratID for low window analyses)
high.int <- deep_window
low.int  <- shallow_window %>% filter(!(stratID == 9990))

# Create case/available "dumt" as before
high.int$dumt <- rep(1, nrow(high.int))
low.int$dumt  <- rep(1, nrow(low.int))

# -------------------------
# 7) Fit GAMs (same as your original spec)
# -------------------------
gam.high <- gam(
  cbind(dumt, stratID) ~ s(standardized.do, standardized.temp, bs = "ts"),
  data = high.int,
  family = cox.ph,
  weights = case,
  method = "REML",
  gamma  = 1.6,
  select = TRUE
)
summary(gam.high)

gam.low <- gam(
  cbind(dumt, stratID) ~ s(standardized.do, standardized.temp, bs = "ts"),
  data = low.int,
  family = cox.ph,
  weights = case,
  method = "REML",
  gamma  = 1.6,
  select = TRUE
)
summary(gam.low)

# -------------------------
# 8) Average effect helper (unchanged from your script)
# -------------------------
calc_avg_effect <- function(model, variable, mean_x, sd_x, bandwidth = NULL) {
  model_data <- model$model
  available_indices <- model_data$`(weights)` == 0
  used_indices <- model_data$`(weights)` == 1
  
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
# 9) Plots (same styling/limits as yours)
# -------------------------

# HIGH / DO
do_nor_high <- calc_avg_effect(gam.high, "standardized.do", mean_x = mean.do, sd_x = sd.do)

# HIGH / TEMP
temp_nor_high <- calc_avg_effect(gam.high, "standardized.temp", mean_x = mean.t, sd_x = sd.t)

# LOW / DO
do_nor_low <- calc_avg_effect(gam.low, "standardized.do", mean_x = mean.do, sd_x = sd.do)

# LOW / TEMP
temp_nor_low <- calc_avg_effect(gam.low, "standardized.temp", mean_x = mean.t, sd_x = sd.t)

# HIGH / DO
nor_do_high <- ggplot(do_nor_high$plot_data, aes(x = x, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "steelblue4") +
  geom_line(color = "steelblue4", size = 1) +
  geom_rug(aes(x = x), color = "gray60", alpha = .1, size = 0.1, sides = "b") +
  geom_rug(data = do_nor_high$used_vals, aes(x = used_vals),
           inherit.aes = FALSE, color = "steelblue4", alpha = .7, size = 0.1, sides = "b") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 14, family = "serif")) +
  labs(x = "Dissolved Oxygen (mg/L)", y = NULL) +
  ylim(c(0, max(temp_nor_high$plot_data$fit))) +
  xlim(c(min(do_nor_low$plot_data$x), max(do_nor_high$plot_data$x)))


# HIGH / TEMP
nor_temp_high <- ggplot(temp_nor_high$plot_data, aes(x = x, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "aquamarine4") +
  geom_line(color = "aquamarine4", size = 1) +
  geom_rug(aes(x = x), color = "gray60", alpha = .1, size = 0.1, sides = "b") +
  geom_rug(data = temp_nor_high$used_vals, aes(x = used_vals),
           inherit.aes = FALSE, color = "aquamarine4", alpha = .7, size = 0.1, sides = "b") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 14, family = "serif")) +
  labs(x = "Temperature (\u00B0C)", y = NULL) +
  ylim(c(0, max(temp_nor_high$plot_data$fit))) +
  xlim(c(min(temp_nor_low$plot_data$x), max(temp_nor_high$plot_data$x)))


# LOW / DO
nor_do_low <- ggplot(do_nor_low$plot_data, aes(x = x, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "steelblue4") +
  geom_line(color = "steelblue4", size = 1) +
  geom_rug(aes(x = x), color = "gray60", alpha = .1, size = 0.1, sides = "b") +
  geom_rug(data = do_nor_low$used_vals, aes(x = used_vals),
           inherit.aes = FALSE, color = "steelblue4", alpha = .7, size = 0.1, sides = "b") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 14, family = "serif")) +
  labs(x = "Dissolved Oxygen (mg/L)", y = NULL) +
  ylim(c(0, max(temp_nor_low$plot_data$fit))) +
  xlim(c(min(do_nor_low$plot_data$x), max(do_nor_high$plot_data$x)))


# LOW / TEMP
nor_temp_low <- ggplot(temp_nor_low$plot_data, aes(x = x, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "aquamarine4") +
  geom_line(color = "aquamarine4", size = 1) +
  geom_rug(aes(x = x), color = "gray60", alpha = .1, size = 0.1, sides = "b") +
  geom_rug(data = temp_nor_low$used_vals, aes(x = used_vals),
           inherit.aes = FALSE, color = "aquamarine4", alpha = .7, size = 0.1, sides = "b") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 14, family = "serif")) +
  labs(x = "Temperature (\u00B0C)", y = NULL) +
  ylim(c(0, max(temp_nor_low$plot_data$fit))) +
  xlim(c(min(temp_nor_low$plot_data$x), max(temp_nor_high$plot_data$x)))

# Arrange as before
norwood_high <- grid.arrange(
  nor_do_high, nor_temp_high,
  nrow = 1,
  top = textGrob("DO-maxima", gp = gpar(fontsize = 16, fontfamily = "serif"))
)

norwood_low <- grid.arrange(
  nor_do_low, nor_temp_low,
  nrow = 1,
  top = textGrob("DO-minima", gp = gpar(fontsize = 16, fontfamily = "serif"))
)

all <- grid.arrange(
  norwood_high, norwood_low,
  nrow = 2,
  left = textGrob("Relative Probability of Use",
                  rot = 90,
                  gp = gpar(fontsize = 16, fontfamily = "serif"))
)

ggsave("results/figures/norwood-average-effect-plot-deepest-shallowest.png", plot = all, width = 8, height = 9, dpi = 300)                    



# ---- AIC comparisons: temp-only, DO-only, both (bivariate) ----

fit_aic_set <- function(dat, label) {
  # ensure dumt exists
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
    AIC = c(AIC(m_temp), AIC(m_do), AIC(m_both)),
    edf = c(sum(m_temp$edf), sum(m_do$edf), sum(m_both$edf)),
    row.names = NULL
  )
  tab$deltaAIC <- tab$AIC - min(tab$AIC)
  tab <- tab[order(tab$AIC), ]
  return(tab)
}

aic_high <- fit_aic_set(high.int, "DO-maxima (high)")
aic_low  <- fit_aic_set(low.int,  "DO-minima (low)")

aic_table <- rbind(aic_high, aic_low)
print(aic_table)

# Optional: save to CSV
dir.create("results/tables", showWarnings = FALSE, recursive = TRUE)
write.csv(aic_table, "results/tables/norwood-gam-aic-comparison-deepest-shallowest.csv", row.names = FALSE)


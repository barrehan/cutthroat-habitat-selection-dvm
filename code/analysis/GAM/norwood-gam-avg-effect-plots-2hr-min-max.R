rm(list=ls())

library(ggplot2)
library(dplyr)
library(grid)
library(gridExtra)
library(survival)
library(lubridate)
library(mgcv)

# setwd("C:/Users/ruprechj/Downloads")
nor.ib<- read.csv("data/modif.data/hab.select.mod/norwood.ibutton.data/norwood.ibutton.pooled.csv")
nor.ib$date.time <- ymd_hms(nor.ib$date.time)
nor.ib <- nor.ib %>% force_tz(nor.ib$date.time, tzone = "America/Los_Angeles")
nor.ib<-nor.ib[nor.ib$date.time >="2021-08-01 00:00:00" & nor.ib$date.time < "2021-08-05 00:00:00",]
range(nor.ib$temperature)
nor.ib<-nor.ib[,-7:-8]

nor.ib <- subset(nor.ib, ibutton.id !=18)

nor.ib$standardized.do <- as.numeric(scale(nor.ib$dissolved.oxygen))
nor.ib$standardized.temp <-as.numeric(scale(nor.ib$temperature))

#variables for back transformation from standardized scale

mean.t<-mean(nor.ib$temperature)
sd.t <-sd(nor.ib$temperature)
mean.do<-mean(nor.ib$dissolved.oxygen)
sd.do<-sd(nor.ib$dissolved.oxygen)

# Data frames by high/low interval ----------------------------------------

high.int <- nor.ib[nor.ib$highlowDO.2hours == "high",]
high.int <- high.int[!is.na(high.int$highlowDO.2hours),]
range(high.int$temperature)
range(high.int$dissolved.oxygen)

low.int <- nor.ib[nor.ib$highlowDO.2hours == "low",]
low.int <- low.int[!is.na(low.int$highlowDO.2hours),]
range(low.int$temperature)
range(low.int$dissolved.oxygen)

######HIGH DO ANALYSIS######
# GAM Norwood high DO 2hr window ------------------------------------------

high.int$dumt <-rep(1,nrow(high.int))

gam.high <- gam(
  cbind(dumt, stratID) ~ s(standardized.do, standardized.temp, bs = "ts"),
  data = high.int,
  family = cox.ph, weights = case,
  method = "REML",
  gamma  = 1.6, select = TRUE
)
summary(gam.high)


######LOW DO ANALYSIS#####
# GAM Norwood low DO 2hr window ------------------------------------------
#StratID 9990 has weird predicted temp for the DO, remove for analysis

low.int <-low.int[!(low.int$stratID == 9990),]

low.int$dumt <-rep(1,nrow(low.int))
gam.low <-gam(
  cbind(dumt, stratID) ~ s(standardized.do, standardized.temp, bs = "ts"),
  data = low.int,
  family = cox.ph, weights = case,
  method = "REML",
  gamma  = 1.6, select = TRUE
)
summary(gam.low)

calc_avg_effect <- function(model,
                            variable,  
                            mean_x, # mean of original covariate values to backtransform
                            sd_x, # mean of original covariate values to backtransform
                            bandwidth = NULL) { # bandwidth adjusts wiggliness of smoother, higher vals = smoother
  
  model_data <- model$model
  
  available_indices <- model_data$`(weights)` == 0
  used_indices <- model_data$`(weights)` == 1
  
  # Extract your covariate data
  available_data <- model_data |>
    filter(available_indices)
  available_vals <- available_data[[variable]]
  
  used_data <- model_data |>
    filter(used_indices)
  used_vals <- used_data[[variable]]
  used_vals <- data.frame(used_vals = (used_vals * sd_x) + mean_x) # backtransform

  # Compute fitted RSF values at available locations
  fitted_values <- predict(model, newdata = available_data, type = "link")
  w_x <- exp(fitted_values)  # Exponential RSF values w(x)
  
  # Use ksmooth for nonparametric regression
  if (is.null(bandwidth)) {
    smooth_fit <- ksmooth(available_vals, w_x, kernel = "normal")
  } else {
    smooth_fit <- ksmooth(available_vals, w_x, kernel = "normal", bandwidth = bandwidth)
  }
  
    # Bootstrap confidence intervals for ksmooth
    # This resamples the data and refits ksmooth to get CI bounds
    n_boot <- 200
    n_points <- length(smooth_fit$x)
    boot_fits <- matrix(NA, nrow = n_boot, ncol = n_points)
    
    for (i in 1:n_boot) {
      boot_idx <- sample(length(available_vals), replace = TRUE)
      boot_covar <- available_vals[boot_idx]
      boot_w <- w_x[boot_idx]
      
      # Fit ksmooth to bootstrap sample
      if (is.null(bandwidth)) {
        boot_smooth <- ksmooth(boot_covar, boot_w, kernel = "normal", x.points = smooth_fit$x)
      } else {
        boot_smooth <- ksmooth(boot_covar, boot_w, kernel = "normal", bandwidth = bandwidth, x.points = smooth_fit$x)
      }
      boot_fits[i, ] <- boot_smooth$y
    }
    
    # Calculate percentile-based confidence intervals
    ci_lower <- apply(boot_fits, 2, quantile, probs = 0.025, na.rm = TRUE)
    ci_upper <- apply(boot_fits, 2, quantile, probs = 0.975, na.rm = TRUE)
    
    plot_data <- data.frame(
      fit = smooth_fit$y,
      x_std = smooth_fit$x,
      x = (smooth_fit$x * sd_x) + mean_x, # to backtransform x given sd (first val) and mean (second val) of original data
      upper = ci_upper,
      lower = ci_lower
    )
    
    return(list(plot_data = plot_data, used_vals = used_vals))
    
}

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
                  gp = gpar(fontsize = 16, fontfamily = "serif")))
  
# ggsave("results/figures/norwood-average-effect-plot-0801-0804.png", plot = all, width = 8, height = 9, dpi = 300)                    


# --- Peak + 95% CI from bootstrap (location and height) ---

calc_avg_effect <- function(model, variable, mean_x, sd_x, bandwidth = NULL, n_boot = 200) {
  md <- model$model
  avail <- md$`(weights)` == 0
  used  <- md$`(weights)` == 1
  
  dat_avail <- md[avail, , drop = FALSE]
  x_avail   <- dat_avail[[variable]]
  
  used_vals <- md[used, variable, drop = TRUE]
  used_vals <- data.frame(used_vals = used_vals * sd_x + mean_x)
  
  # RSF weights
  w <- exp(predict(model, newdata = dat_avail, type = "link"))
  
  # main ksmooth on standardized x; keep its grid
  ks_main <- if (is.null(bandwidth)) {
    ksmooth(x_avail, w, kernel = "normal")
  } else {
    ksmooth(x_avail, w, kernel = "normal", bandwidth = bandwidth)
  }
  xg_std <- ks_main$x
  yg     <- ks_main$y
  
  # bootstrap curves on same grid
  B <- n_boot
  boot <- matrix(NA_real_, B, length(xg_std))
  for (b in seq_len(B)) {
    idx <- sample(seq_along(x_avail), replace = TRUE)
    xb  <- x_avail[idx]; wb <- w[idx]
    ksb <- if (is.null(bandwidth)) {
      ksmooth(xb, wb, kernel = "normal", x.points = xg_std)
    } else {
      ksmooth(xb, wb, kernel = "normal", bandwidth = bandwidth, x.points = xg_std)
    }
    boot[b, ] <- ksb$y
  }
  
  # pointwise 95% band (for plotting)
  band_lower <- apply(boot, 2, quantile, 0.025, na.rm = TRUE)
  band_upper <- apply(boot, 2, quantile, 0.975, na.rm = TRUE)
  
  # peak (argmax) for main and for each bootstrap
  i_hat   <- which.max(yg)
  x_peak  <- xg_std[i_hat] * sd_x + mean_x
  y_peak  <- yg[i_hat]
  
  i_star  <- apply(boot, 1, which.max)
  x_star  <- xg_std[i_star] * sd_x + mean_x
  y_star  <- boot[cbind(seq_len(B), i_star)]
  
  peak <- list(
    x     = x_peak,
    x_lwr = quantile(x_star, 0.025, na.rm = TRUE),
    x_upr = quantile(x_star, 0.975, na.rm = TRUE),
    y     = y_peak,
    y_lwr = quantile(y_star, 0.025, na.rm = TRUE),
    y_upr = quantile(y_star, 0.975, na.rm = TRUE)
  )
  
  plot_data <- data.frame(
    x_std = xg_std,
    x     = xg_std * sd_x + mean_x,
    fit   = yg,
    lower = band_lower,
    upper = band_upper
  )
  
  list(plot_data = plot_data, used_vals = used_vals, peak = peak)
}

# High-DO window
do_nor_high   <- calc_avg_effect(gam.high, "standardized.do", mean.do, sd.do)
temp_nor_high <- calc_avg_effect(gam.high, "standardized.temp", mean.t, sd.t)

# Low-DO window
do_nor_low    <- calc_avg_effect(gam.low, "standardized.do", mean.do, sd.do)
temp_nor_low  <- calc_avg_effect(gam.low, "standardized.temp", mean.t, sd.t)

# Visualize data

pk_tbl <- dplyr::bind_rows(
  dplyr::mutate(as.data.frame(do_nor_high$peak),  Panel = "DO-maxima: DO"),
  dplyr::mutate(as.data.frame(temp_nor_high$peak),Panel = "DO-maxima: Temp"),
  dplyr::mutate(as.data.frame(do_nor_low$peak),   Panel = "DO-minima: DO"),
  dplyr::mutate(as.data.frame(temp_nor_low$peak), Panel = "DO-minima: Temp")
) |>
  dplyr::select(Panel, x, x_lwr, x_upr, y, y_lwr, y_upr)

print(pk_tbl, row.names = FALSE)

# Optional: save to CSV
dir.create("results/tables", showWarnings = FALSE, recursive = TRUE)
write.csv(pk_tbl, "results/tables/norwood-gam-peak-selection-95CI-0801-0804.csv", row.names = FALSE)


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
write.csv(aic_table, "results/tables/norwood-gam-aic-comparison-0801-0804.csv", row.names = FALSE)




# --- Partial-likelihood AIC for mgcv::gam(..., family = cox.ph()) ---

cox_aic <- function(m) {
  if (is.null(m)) return(c(AICp = NA_real_, edf = NA_real_))
  # logLik.gam returns partial log-likelihood for cox.ph(); may be NA on failure
  ll <- tryCatch(as.numeric(logLik(m)), error = function(e) NA_real_)
  edf <- tryCatch(sum(m$edf), error = function(e) NA_real_)
  if (!is.finite(ll) || !is.finite(edf)) return(c(AICp = NA_real_, edf = NA_real_))
  c(AICp = -2 * ll + 2 * edf, edf = edf)
}

fit_aic_set <- function(dat, label) {
  # ensure dumt exists
  if (!"dumt" %in% names(dat)) dat$dumt <- 1L
  if (!all(c("stratID","standardized.temp","standardized.do","case") %in% names(dat))) {
    stop("Missing one of required columns: stratID, standardized.temp, standardized.do, case")
  }
  
  # Keep finite rows; keep zero-weight rows (controls)
  keep <- is.finite(dat$dumt) & is.finite(dat$stratID) &
    is.finite(dat$standardized.temp) & is.finite(dat$standardized.do) &
    is.finite(dat$case)
  dat2 <- dat[keep, , drop = FALSE]
  
  # Cox requires positive "time" column; shift if needed
  if (any(dat2$stratID <= 0, na.rm = TRUE)) {
    dat2$stratID <- dat2$stratID - min(dat2$stratID, na.rm = TRUE) + 1
  }
  
  # Epsilonize weights so zeros stay but don't drop rows
  w <- dat2$case
  w[!is.finite(w)] <- 0
  w <- pmax(w, 1e-8)
  
  fam <- cox.ph()
  
  # Choose modest k based on unique counts to avoid rank issues
  uniq_k <- function(x, k_max = 10L) {
    nux <- length(unique(x[is.finite(x)]))
    max(3L, min(k_max, nux - 1L))
  }
  k_t  <- uniq_k(dat2$standardized.temp, 12L)
  k_do <- uniq_k(dat2$standardized.do,  12L)
  k_bi <- max(8L, min(24L, k_t + k_do))
  
  f_temp <- as.formula(sprintf(
    "cbind(dumt, stratID) ~ s(standardized.temp, bs='ts', k=%d)", k_t))
  f_do   <- as.formula(sprintf(
    "cbind(dumt, stratID) ~ s(standardized.do, bs='ts', k=%d)", k_do))
  f_bi_iso <- as.formula(sprintf(
    "cbind(dumt, stratID) ~ s(standardized.do, standardized.temp, bs='ts', k=%d)", k_bi))
  
  fit_safe <- function(fml) {
    tryCatch(
      gam(fml, data = dat2, family = fam, weights = w,
          method = "REML", gamma = 1.6, select = TRUE),
      error = function(e) NULL
    )
  }
  
  m_temp <- fit_safe(f_temp)
  m_do   <- fit_safe(f_do)
  m_both <- fit_safe(f_bi_iso)
  if (is.null(m_both)) {
    # fallback: tensor product, then additive if needed
    f_bi_te  <- as.formula(sprintf(
      "cbind(dumt, stratID) ~ te(standardized.do, standardized.temp, bs=c('ts','ts'), k=c(%d,%d))",
      k_do, k_t))
    m_both <- fit_safe(f_bi_te)
    if (is.null(m_both)) {
      f_bi_add <- as.formula(sprintf(
        "cbind(dumt, stratID) ~ s(standardized.do, bs='ts', k=%d) + s(standardized.temp, bs='ts', k=%d)",
        k_do, k_t))
      m_both <- fit_safe(f_bi_add)
    }
  }
  
  # Build table using partial-likelihood AIC
  sc_temp <- cox_aic(m_temp)
  sc_do   <- cox_aic(m_do)
  sc_bi   <- cox_aic(m_both)
  
  tab <- data.frame(
    Window = label,
    Model  = c("Temperature only", "DO only", "Both (bivariate)"),
    AICp   = c(sc_temp["AICp"], sc_do["AICp"], sc_bi["AICp"]),
    edf    = c(sc_temp["edf"],  sc_do["edf"],  sc_bi["edf"]),
    row.names = NULL
  )
  
  if (all(is.na(tab$AICp))) {
    tab$deltaAICp <- NA_real_
  } else {
    tab$deltaAICp <- tab$AICp - min(tab$AICp, na.rm = TRUE)
  }
  
  message(sprintf("[%s] rows=%d; kept=%d; zeros in case=%d; k_t=%d, k_do=%d, k_bi=%d",
                  label, nrow(dat), nrow(dat2), sum(dat2$case == 0, na.rm = TRUE),
                  k_t, k_do, k_bi))
  tab[order(tab$AICp), ]
}



# ---- Run as before ----
aic_high <- fit_aic_set(high.int, "DO-maxima (high)")
aic_low  <- fit_aic_set(low.int,  "DO-minima (low)")
aic_table <- rbind(aic_high, aic_low)
print(aic_table)

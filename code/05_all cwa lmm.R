rm(list = ls())

# --------------------------------------------------
# Libraries
# --------------------------------------------------

library(tidyverse)
library(lubridate)
library(lme4)
library(sjPlot)
library(ggpubr)
library(effects)

# --------------------------------------------------
# Figure colors
# --------------------------------------------------

do_min_col <- "#289A84"
do_max_col <- "#8FF7BD"

# --------------------------------------------------
# Import and prepare data
# --------------------------------------------------

all.array.dat <- read_csv(
  "data/modif.data/logger.array/epa.osu.logger.array.cleaned.csv",
  show_col_types = FALSE
)

all.array.dat <- all.array.dat %>%
  mutate(
    date.time = mdy_hm(date.time),
    hour = hour(date.time),
    location = factor(location)
  )

osu <- all.array.dat %>%
  filter(collected.by == "osu")

# --------------------------------------------------
# Exploratory mixed-effects models
# --------------------------------------------------
#
# We used linear mixed-effects models because
# observations were repeatedly collected within
# locations and therefore were not independent.
#
# Temperature was included as a fixed effect and
# location was included as a random intercept to
# account for variation among sites.
#
# Three exploratory models were evaluated:
#
#   1. OSU sites only
#   2. All logger arrays
#   3. All arrays with the NHB bottom logger removed
#
# Results were qualitatively similar among models.
# The NHB bottom logger appeared influential, so
# subsequent analyses used that dataset for the
# final diel comparisons.
#
# --------------------------------------------------

mod.osu <- lmer(
  dissolved.oxygen ~ temperature +
    (1 | location),
  data = osu
)

summary(mod.osu)

tab_model(
  mod.osu,
  show.re.var = TRUE,
  pred.labels = c("(Intercept)", "Temperature °C"),
  dv.labels = "OSU sites only"
)

mod.all <- lmer(
  dissolved.oxygen ~ temperature +
    (1 | location),
  data = all.array.dat
)

summary(mod.all)

tab_model(
  mod.all,
  show.re.var = TRUE,
  pred.labels = c("(Intercept)", "Temperature °C"),
  dv.labels = "All logger arrays"
)

# --------------------------------------------------
# Remove influential NHB bottom logger
# --------------------------------------------------

data.mod <- all.array.dat %>%
  filter(
    !(logger.site == "north.harrisburg.site.b" &
        sensor.depth == "2.4")
  )

mod.nhb.removed <- lmer(
  dissolved.oxygen ~ temperature +
    (1 | location),
  data = data.mod
)

summary(mod.nhb.removed)

tab_model(
  mod.nhb.removed,
  show.re.var = TRUE,
  pred.labels = c("(Intercept)", "Temperature °C"),
  dv.labels = "All arrays, NHB bottom logger removed"
)

# --------------------------------------------------
# Final diel analysis
# --------------------------------------------------
#
# Diel patterns indicated that minimum dissolved
# oxygen generally occurred during early morning
# hours and maximum dissolved oxygen occurred
# during evening hours.
#
# Therefore, temperature–DO relationships were
# evaluated separately for:
#
#   DO-min : 0600–0800 h
#   DO-max : 1800–2000 h
#
# --------------------------------------------------

array.lowdo <- data.mod %>%
  filter(hour >= 6, hour < 8)

array.highdo <- data.mod %>%
  filter(hour >= 18, hour < 20)

# --------------------------------------------------
# DO minimum model
# --------------------------------------------------

mod.lowdo <- lmer(
  dissolved.oxygen ~ temperature +
    (1 | location),
  data = array.lowdo
)

summary(mod.lowdo)

tab_model(
  mod.lowdo,
  show.re.var = TRUE,
  pred.labels = c("(Intercept)", "Temperature °C"),
  dv.labels = "DO-min period (0600–0800 h)"
)

low.do.fit <- effects::effect(
  term = "temperature",
  mod = mod.lowdo
)

low.do.fit <- as.data.frame(low.do.fit)

# --------------------------------------------------
# DO maximum model
# --------------------------------------------------

mod.highdo <- lmer(
  dissolved.oxygen ~ temperature +
    (1 | location),
  data = array.highdo
)

summary(mod.highdo)

tab_model(
  mod.highdo,
  show.re.var = TRUE,
  pred.labels = c("(Intercept)", "Temperature °C"),
  dv.labels = "DO-max period (1800–2000 h)"
)

high.do.fit <- effects::effect(
  term = "temperature",
  mod = mod.highdo
)

high.do.fit <- as.data.frame(high.do.fit)

# --------------------------------------------------
# Blue Ruin net pen
# --------------------------------------------------

br.netpen <- osu %>%
  filter(logger.site == "site.4.netpen")

br.lowdo <- br.netpen %>%
  filter(hour >= 5, hour < 7)

br.highdo <- br.netpen %>%
  filter(hour >= 17, hour < 19)

br.mod.low <- lm(
  dissolved.oxygen ~ temperature,
  data = br.lowdo
)

br.mod.high <- lm(
  dissolved.oxygen ~ temperature,
  data = br.highdo
)

br.low.fit <- effects::effect(
  term = "temperature",
  mod = br.mod.low
)

br.low.fit <- as.data.frame(br.low.fit)

br.high.fit <- effects::effect(
  term = "temperature",
  mod = br.mod.high
)

br.high.fit <- as.data.frame(br.high.fit)

# --------------------------------------------------
# Norwood net pen
# --------------------------------------------------

nor.netpen <- osu %>%
  filter(location == "norwood")

nor.lowdo <- nor.netpen %>%
  filter(hour >= 6, hour < 8)

nor.highdo <- nor.netpen %>%
  filter(hour >= 18, hour < 20)

nor.mod.low <- lm(
  dissolved.oxygen ~ temperature,
  data = nor.lowdo
)

nor.mod.high <- lm(
  dissolved.oxygen ~ temperature,
  data = nor.highdo
)

nor.low.fit <- effects::effect(
  term = "temperature",
  mod = nor.mod.low
)

nor.low.fit <- as.data.frame(nor.low.fit)

nor.high.fit <- effects::effect(
  term = "temperature",
  mod = nor.mod.high
)

nor.high.fit <- as.data.frame(nor.high.fit)

# --------------------------------------------------
# Common theme
# --------------------------------------------------

fig_theme <- theme_classic(
  base_size = 13,
  base_family = "serif"
) +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 13)
  )

# --------------------------------------------------
# Panel A - Blue Ruin
# --------------------------------------------------

a <- ggplot() +
  
  geom_line(
    data = br.low.fit,
    aes(temperature, fit),
    colour = do_min_col,
    linewidth = 1.2
  ) +
  
  geom_ribbon(
    data = br.low.fit,
    aes(
      x = temperature,
      ymin = lower,
      ymax = upper
    ),
    fill = do_min_col,
    alpha = 0.30
  ) +
  
  geom_line(
    data = br.high.fit,
    aes(temperature, fit),
    colour = do_max_col,
    linewidth = 1.2
  ) +
  
  geom_ribbon(
    data = br.high.fit,
    aes(
      x = temperature,
      ymin = lower,
      ymax = upper
    ),
    fill = do_max_col,
    alpha = 0.30
  ) +
  labs(
    title = "Blue Ruin",
    x = expression(
      paste("Temperature (", degree, "C)")
    ),
    y = expression(
      paste(
        "Dissolved Oxygen (mg L"^-1, ")"
      )
    )
  ) +
  
  coord_cartesian(
    xlim = c(10, 25),
    ylim = c(0, 15)
  ) +
  
  fig_theme

# --------------------------------------------------
# Panel B - Norwood
# --------------------------------------------------

b <- ggplot() +
  
  geom_line(
    data = nor.low.fit,
    aes(temperature, fit),
    colour = do_min_col,
    linewidth = 1.2
  ) +
  
  geom_ribbon(
    data = nor.low.fit,
    aes(
      x = temperature,
      ymin = lower,
      ymax = upper
    ),
    fill = do_min_col,
    alpha = 0.30
  ) +
  
  geom_line(
    data = nor.high.fit,
    aes(temperature, fit),
    colour = do_max_col,
    linewidth = 1.2
  ) +
  
  geom_ribbon(
    data = nor.high.fit,
    aes(
      x = temperature,
      ymin = lower,
      ymax = upper
    ),
    fill = do_max_col,
    alpha = 0.30
  ) +
  
  labs(
    title = "Norwood",
    x = expression(
      paste("Temperature (", degree, "C)")
    ),
    y = expression(
      paste(
        "Dissolved Oxygen (mg L"^-1, ")"
      )
    )
  ) +
  
  coord_cartesian(
    xlim = c(10, 25),
    ylim = c(0, 15)
  ) +
  
  fig_theme

# --------------------------------------------------
# Panel C - All arrays
# --------------------------------------------------

j <- ggplot() +
  
  geom_line(
    data = low.do.fit,
    aes(temperature, fit),
    colour = do_min_col,
    linewidth = 1.2
  ) +
  
  geom_ribbon(
    data = low.do.fit,
    aes(
      x = temperature,
      ymin = lower,
      ymax = upper
    ),
    fill = do_min_col,
    alpha = 0.30
  ) +
  
  geom_line(
    data = high.do.fit,
    aes(temperature, fit),
    colour = do_max_col,
    linewidth = 1.2
  ) +
  
  geom_ribbon(
    data = high.do.fit,
    aes(
      x = temperature,
      ymin = lower,
      ymax = upper
    ),
    fill = do_max_col,
    alpha = 0.30
  ) +
  
  labs(
    title = "All CWA",
    x = expression(
      paste("Temperature (", degree, "C)")
    ),
    y = expression(
      paste(
        "Dissolved Oxygen (mg L"^-1, ")"
      )
    )
  ) +
  coord_cartesian(
    xlim = c(10, 25),
    ylim = c(0, 15)
  ) +
  
  fig_theme

# --------------------------------------------------
# Final figure
# --------------------------------------------------

final.fig <- ggarrange(
  a,
  b,
  j,
  ncol = 2,
  nrow = 2,
  labels = c("A", "B", "C", "D"),
  font.label = list(
    family = "serif",
    face = "bold",
    size = 16
  ),
  align = "hv"
)

final.fig

# ggsave(
#   filename = "final.figures/temp_do_high_low_2hr_lmm.png",
#   plot = final.fig,
#   width = 8,
#   height = 7,
#   dpi = 300
# )

# --------------------------------------------------
# Supplementary Figure S1
# Residuals vs Temperature
# --------------------------------------------------

resid.df <- bind_rows(
  
  data.frame(
    temperature = array.lowdo$temperature,
    residuals = resid(mod.lowdo),
    model = "All arrays\nDO-min"
  ),
  
  data.frame(
    temperature = array.highdo$temperature,
    residuals = resid(mod.highdo),
    model = "All arrays\nDO-max"
  ),
  
  data.frame(
    temperature = br.lowdo$temperature,
    residuals = resid(br.mod.low),
    model = "Blue Ruin\nDO-min"
  ),
  
  data.frame(
    temperature = br.highdo$temperature,
    residuals = resid(br.mod.high),
    model = "Blue Ruin\nDO-max"
  ),
  
  data.frame(
    temperature = nor.lowdo$temperature,
    residuals = resid(nor.mod.low),
    model = "Norwood\nDO-min"
  ),
  
  data.frame(
    temperature = nor.highdo$temperature,
    residuals = resid(nor.mod.high),
    model = "Norwood\nDO-max"
  )
)

supp.fig.s1 <- ggplot(
  resid.df,
  aes(
    x = temperature,
    y = residuals
  )
) +
  
  geom_point(
    alpha = 0.35,
    size = 1.3
  ) +
  
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 0.5
  ) +
  
  
  facet_wrap(
    ~ model,
    ncol = 2
  ) +
  
  labs(
    x = expression(
      paste("Temperature (", degree, "C)")
    ),
    y = "Residuals"
  ) +
  
  fig_theme +
  
  theme(
    strip.text = element_text(
      size = 13,
      face = "bold"
    )
  )

supp.fig.s1

ggsave(
  filename = "final figures/supplemental figures/lmm_residual_temperature_plots.png",
  plot = supp.fig.s1,
  width = 8,
  height = 9,
  dpi = 300
)

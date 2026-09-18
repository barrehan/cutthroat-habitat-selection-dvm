############################################################
## DO-based vertical behavior 
## Uses minimum DO in the water column and rolling 4-hr windows
############################################################

library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(lme4)
library(lmerTest)
library(slider)


############################
#Read and preprocess fish depth data
############################

fish <- read_csv("data/modif.data/ibutton/all.ibuttons.depth.do.interpolation.resolvable.only.csv") %>%
rename(
  depth = fish_depth,
  temperature = ibutton.temp
) %>%
  mutate(
    date.time = ymd_hms(date.time),
    date = as.Date(date.time),
  ) 
unique(fish$ibutton)

############################
## 4. Read and preprocess DO data (both sites)
############################

nor_do <- read_csv("data/modif.data/logger.array/nor.unif.do.temp.set.depth.simulation.csv") %>%
  mutate(site = "norwood")

br_do <- read_csv("data/modif.data/logger.array/br.unif.do.temp.set.depth.simulation.csv") %>%
  mutate(site = "blue.ruin")

do_all <- bind_rows(nor_do, br_do) %>%
  mutate(
    date.time = ymd_hms(date.time, truncated = 3),
    date = as.Date(date.time)
  )

####################################################################
## 5. Collapse DO profiles to minimum DO in the water column
####################################################################
## This represents the worst oxygen available to fish at each moment
####################################################################

do_all <- do_all %>%
  filter(!is.na(date.time))

do_wc_min_ts <- do_all %>%
  group_by(site, date, date.time) %>%
  summarise(
    do_min_wc = min(dissolved.oxygen, na.rm = TRUE),
    .groups = "drop"
  )

####################################################################
## 6. Calculate rolling 4-hour mean of water-column minimum DO
####################################################################
## Assumes 5-minute resolution: 48 observations = 4 hours
## This identifies sustained hypoxic / oxic periods, not point extremes
####################################################################

window_size <- 48

do_windows <- do_wc_min_ts %>%
  arrange(site, date, date.time) %>%
  group_by(site, date) %>%
  mutate(
    do_4hr = slide_dbl(
      do_min_wc,
      mean,
      .before = window_size - 1,
      .complete = TRUE
    )
  ) %>%
  filter(!is.na(do_4hr))

####################################################################
## 7. Identify hypoxic (min DO) and oxic (max DO) 4-hr windows
####################################################################
## The selected time represents the center of the most extreme
## sustained 4-hr DO period for each day
####################################################################

do_extremes <- do_windows %>%
  group_by(site, date) %>%
  summarise(
    min_do_time = date.time[which.min(do_4hr)],
    max_do_time = date.time[which.max(do_4hr)],
    .groups = "drop"
  ) %>%
  mutate(
    min_start = min_do_time - hours(2),
    min_end   = min_do_time + hours(2),
    max_start = max_do_time - hours(2),
    max_end   = max_do_time + hours(2)
  )

############################
## 8. Join DO windows to fish data
############################

fish_do <- fish %>%
  left_join(do_extremes, by = c("site", "date"))

####################################################################
## 9. Calculate median fish depth during hypoxic vs oxic windows
####################################################################
## Median depth reflects typical depth use while avoiding brief excursions
####################################################################

fish_depth_summary <- fish_do %>%
  group_by(site, date, ibutton) %>%
  summarise(
    median_depth_minDO = median(
      depth[date.time >= min_start & date.time <= min_end],
      na.rm = TRUE
    ),
    median_depth_maxDO = median(
      depth[date.time >= max_start & date.time <= max_end],
      na.rm = TRUE
    ),
    .groups = "drop"
  )

############################
## 10. Restrict to experimental date window
############################

fish_depth_summary <- fish_depth_summary %>%
  filter(
    date >= as.Date("2021-07-29") &
      date <= as.Date("2021-08-05")
  )

####################################################################
## 11. Prepare data for plotting (long format)
####################################################################
## Each fish/day now contributes two paired depth observations:
##   - Minimum DO (hypoxic)
##   - Maximum DO (oxic)
####################################################################

depth_long <- fish_depth_summary %>%
  select(
    site,
    date,
    ibutton,
    median_depth_minDO,
    median_depth_maxDO
  ) %>%
  pivot_longer(
    cols = c(median_depth_minDO, median_depth_maxDO),
    names_to = "period",
    values_to = "depth"
  ) %>%
  mutate(
    period = recode(
      period,
      "median_depth_minDO" = "Minimum DO",
      "median_depth_maxDO" = "Maximum DO"
    )
  )

####################################################################
## 12. Plot: paired fish depth during DO min vs DO max
####################################################################

############################
## 2. Your unique fish IDs
############################

fish_ids <- c(
  5, 7, 8, 10, 15, 16, 17, 23, 30,  
  6, 11, 14, 22, 31, 13, 21, 18, 4, 2
)

# Force character (IMPORTANT for matching)
fish_ids <- as.character(fish_ids)

############################################################
## DO-based paired depth plot
############################################################

p <- ggplot(
  depth_long,
  aes(
    x = period,
    y = depth,
    group = interaction(date, ibutton)
  )
) +
  
  # Paired lines for each fish-day
  geom_line(alpha = 0.4, color = "gray60") +
  
  # Points colored only by DO period
  geom_point(aes(color = period), size = 2.2) +
  
  # Two-color palette (same as your original DVM plot)
  scale_color_manual(
    values = c(
      "Maximum DO" = "#FF5050",  # oxic
      "Minimum DO" = "#A5236E"   # hypoxic
    ),
    name = NULL
  ) +
  
  # Facet by site
  facet_wrap(
    ~ site,
    labeller = as_labeller(
      c(
        "blue.ruin" = "Blue Ruin",
        "norwood"   = "Norwood"
      )
    )
  ) +
  
  # Depth increases downward
  scale_y_reverse() +
  
  # Labels
  labs(
    x = NULL,
    y = "Depth (m)",
    title = NULL
  ) +
  
  # Match original theme exactly
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    strip.background = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.key = element_blank(),
    text = element_text(size = 20, family = "serif")
  )

p

############################################################
## Histogram of depth shift (Max DO - Min DO)
############################################################


fish_depth_summary <- fish_depth_summary %>%
  mutate(
    do_depth_shift = median_depth_maxDO - median_depth_minDO
  )
    
fish_depth_summary <- fish_depth_summary %>%
      filter(!is.na(do_depth_shift))


summary_stats <- fish_depth_summary %>%
  group_by(site) %>%
  summarise(
    mean_shift = mean(do_depth_shift, na.rm = TRUE),
    se_shift   = sd(do_depth_shift, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

  

ggplot(fish_depth_summary, aes(x = do_depth_shift)) +
  
  geom_histogram(binwidth = 0.04, fill = "gray70", color = "black") +
  
  geom_vline(xintercept = 0, linetype = "dashed", size = 1) +
  
  # mean point (IMPORTANT: override aes completely)
  geom_point(
    data = summary_stats,
    aes(x = mean_shift, y =8 ),
    color = "red",
    size = 3,
    inherit.aes = FALSE
  ) +
  
  # SE bar (IMPORTANT: override aes completely)
  geom_errorbarh(
    data = summary_stats,
    aes(
      xmin = mean_shift - se_shift,
      xmax = mean_shift + se_shift,
      y = 8
    ),
    height = 0.2,
    color = "red",
    inherit.aes = FALSE
  ) +
  
  facet_wrap(~ site,
             labeller = as_labeller(c(
               "blue.ruin" = "Blue Ruin",
               "norwood"   = "Norwood"
             ))) +
  
  labs(
    x = "Depth shift (m)\n(Max DO - Min DO)",
    y = "Frequency"
  ) +
  
  theme(
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    text = element_text(size = 20, family = "serif")
  )



############################################################
## Density plot of depth shift
############################################################

ggplot(
  fish_depth_summary,
  aes(
    x = do_depth_shift,
    fill = site
  )
) +
  
  geom_density(
    alpha = 0.4
  ) +
  
  geom_vline(
    xintercept = 0,
    linetype = "dashed"
  ) +
  
  facet_wrap(~site)

############################################################
## Mixed effects model
############################################################

m1<- lmer(
  depth ~ period * site +
    (1 | ibutton) +
    (1 | date),
  data = depth_long
)

summary(m1)


m2 <- lmer(
  depth ~ period * site +
    (1 + period | ibutton) +
    (1 | date),
  data = depth_long
)

summary(m2)

# Methods:
# We used a linear mixed-effects modeling framework to evaluate whether fish depth 
# varied between periods of minimum and maximum DO. Fish depth (m) was modeled as a
# function of DO period (minimum vs. maximum), site (Blue Ruin vs. Norwood), and
# their interaction. We included an interaction to test whether the effect of DO on 
# fish depth differed between sites. We also  included fish ID as a random effect 
# with a random intercept and slope for DO period, and date as a random intercept. 
# Models were fit using restricted maximum likelihood (REML) with the lme4 package 
# in R.
# Results:
# Cutthroat trout monitored in situ did not remain at constant depth but instead exhibited diel vertical movements (Figure 4, Figures S5–S7). Median fish depth varied significantly between dissolved oxygen (DO) periods, with individuals occupying deeper depths during periods of maximum DO. At Blue Ruin, fish were on average 0.18 ± 0.05 m deeper during the DO-maximum period than during the DO-minimum period (linear mixed-effects model: β = 0.184, SE = 0.046, t = 4.00, p = 0.001; n = 18 fish, 288 observations). The response was stronger at Norwood, where fish were estimated to occupy depths approximately 0.35 m deeper during DO-maximum periods. The effect of DO period differed significantly between sites (period × site interaction: β = -0.170, SE = 0.074, t = 2.31, p = 0.035), indicating a larger depth shift at Norwood. Random effects indicated substantial among-individual variation in baseline depth (SD = 0.22 m) and in the magnitude of depth responses to DO conditions (SD = 0.13 m).
#Cutthroat trout monitored in situ did not remain at constant depth but exhibited diel vertical movements. Fish occupied significantly deeper depths during periods of maximum dissolved oxygen than during periods of minimum dissolved oxygen (linear mixed-effects model: β = -0.153 ± 0.047 SE for minimum DO relative to maximum DO, t = -3.23, p = 0.005). Average fish depth at Blue Ruin was 1.10 m during maximum DO periods and approximately 0.15 m shallower during minimum DO periods. A similar pattern was observed at Norwood, where fish were estimated to be approximately 0.22 m deeper during maximum DO periods; however, the site × period interaction was not significant (β = -0.069 ± 0.080 SE, t = -0.86, p = 0.40), indicating no evidence that the magnitude of depth shifts differed between sites. Random effects indicated substantial among-individual variation in both baseline depth (SD = 0.16 m) and responses to dissolved oxygen conditions (SD = 0.14 m).


####################################################################
## 9. Fish-level median depth across all days
####################################################################
## Each fish contributes one median depth during
## minimum DO periods and one median depth during
## maximum DO periods.
####################################################################

fish_depth_by_fish <- fish_do %>%
  group_by(site, ibutton) %>%
  summarise(
    
    median_depth_minDO = median(
      depth[
        date.time >= min_start &
          date.time <= min_end
      ],
      na.rm = TRUE
    ),
    
    median_depth_maxDO = median(
      depth[
        date.time >= max_start &
          date.time <= max_end
      ],
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )

####################################################################
## 10. Long format for plotting
####################################################################

depth_long <- fish_depth_by_fish %>%
  pivot_longer(
    cols = c(
      median_depth_minDO,
      median_depth_maxDO
    ),
    names_to = "period",
    values_to = "depth"
  ) %>%
  mutate(
    period = recode(
      period,
      "median_depth_minDO" = "DO-minimum",
      "median_depth_maxDO" = "DO-maximum"
    )
  )

####################################################################
## 11. Paired fish plot
####################################################################
## Each line now represents one fish rather than
## one fish-day.
####################################################################

############################################################
# Add fish-specific offsets
############################################################

set.seed(123)

depth_long <- depth_long %>%
  group_by(site, ibutton) %>%
  mutate(
    
    # small offset so overlapping fish can be distinguished
    fish_offset = runif(1, -0.01, 0.01),
    
    # bring categories closer together
    x_position = case_when(
      period == "DO-maximum" ~ 1.00 + fish_offset,
      period == "DO-minimum" ~ 1.25 + fish_offset
    )
    
  ) %>%
  ungroup()

############################################################
# Paired fish plot
############################################################

p <- ggplot(
  depth_long,
  aes(
    x = x_position,
    y = depth,
    group = ibutton
  )
) +
  
  geom_line(
    colour = "grey70",
    alpha = 0.7,
    linewidth = 0.7
  ) +
  
  geom_point(
    aes(colour = period),
    size = 3, alpha = 0.75
  ) +
  
  facet_wrap(
    ~site,
    labeller = as_labeller(
      c(
        "blue.ruin" = "Blue Ruin",
        "norwood"   = "Norwood"
      )
    )
  ) +
  
  scale_x_continuous(
    breaks = c(1.00, 1.25),
    labels = c(
      "DO-max",
      "DO-min"
    ),
    limits = c(0.95, 1.30),
    expand = c(0, 0)
  ) +
  
  scale_y_reverse() +
  
  scale_color_manual(
    values = c(
      "DO-maximum" = "#8FF7BD",
      "DO-minimum" = "#289A84"
    )
  ) +
  
  labs(
    x = NULL,
    y = "Median Depth (m)"
  ) +
  
  theme_classic(
    base_size = 16,
    base_family = "serif"
  ) +
  
  theme(
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_text(
      size = 16,
      face = "bold"
    ),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14)
  )

p


ggsave(
  filename = "final figures/overall median depth min max DO.png",
  plot = p,
  width = 5.75,
  height = 6,
  dpi = 300
)

#############################################
#proportion of fish-days showing expected DVM direction
############################################

fish_depth_summary %>%
  mutate(
    depth_shift = median_depth_maxDO - median_depth_minDO
  ) %>%
  group_by(site) %>%
  summarize(
    n_fish_days = n(),
    n_positive = sum(depth_shift > 0, na.rm = TRUE),
    pct_positive = 100 * n_positive / n_fish_days
  )


#############################################
#average temp/do occupied per min/max window
############################################

fish_occ <- fish %>%
  left_join(
    do_extremes,
    by = c("site", "date")
  )

fish_occ_summary <- fish_occ %>%
  group_by(site, date, ibutton) %>%
  summarise(
    
    DO_min_window = median(
      dissolved.oxygen[
        date.time >= min_start &
          date.time <= min_end
      ],
      na.rm = TRUE
    ),
    
    DO_max_window = median(
      dissolved.oxygen[
        date.time >= max_start &
          date.time <= max_end
      ],
      na.rm = TRUE
    ),
    
    Temp_min_window = median(
      temperature[
        date.time >= min_start &
          date.time <= min_end
      ],
      na.rm = TRUE
    ),
    
    Temp_max_window = median(
      temperature[
        date.time >= max_start &
          date.time <= max_end
      ],
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )

occupied_conditions <- fish_occ_summary %>%
  group_by(site) %>%
  summarise(
    
    DO_min_mean = mean(DO_min_window, na.rm = TRUE),
    DO_min_sd   = sd(DO_min_window, na.rm = TRUE),
    
    DO_max_mean = mean(DO_max_window, na.rm = TRUE),
    DO_max_sd   = sd(DO_max_window, na.rm = TRUE),
    
    Temp_min_mean = mean(Temp_min_window, na.rm = TRUE),
    Temp_min_sd   = sd(Temp_min_window, na.rm = TRUE),
    
    Temp_max_mean = mean(Temp_max_window, na.rm = TRUE),
    Temp_max_sd   = sd(Temp_max_window, na.rm = TRUE),
    
    n = n(),
    
    .groups = "drop"
  )

occupied_conditions

################################
# New figure depth shift
###############################

q <- fish_depth_summary %>%
  mutate(
    depth_shift = median_depth_maxDO - median_depth_minDO
  ) %>%
  ggplot(aes(site, depth_shift)) +
  
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  
  geom_jitter(
    width = 0.1,
    height = 0,
    alpha = 0.5,
    colour = "#AEB2B7"
  ) +
  
  stat_summary(
    fun = mean,
    geom = "point",
    size = 4,
    colour = "#532A34"
  ) +
  
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.15,
    colour = "#532A34"
  ) +
  
  scale_x_discrete(
    labels = c(
      "blue.ruin" = "Blue Ruin",
      "norwood" = "Norwood"
    )
  ) +
  
  labs(
    x = NULL,
    y = "Depth shift (DO-max − DO-min; m)"
  ) +
  
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.key = element_rect(fill = "white"),
    text = element_text(size = 18, family = "serif"),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 18),  # same size as y-axis title
    legend.title = element_text(family = "serif"),
    legend.text = element_text(family = "serif")
  )


# ggsave(
#   filename = "final figures/depth shift DO min DO max periods.png",
#   plot = q,
#   width = 5.75,
#   height = 6,
#   dpi = 300
# )


fish_shift <- fish_depth_summary %>%
  mutate(
    depth_shift = median_depth_maxDO - median_depth_minDO
  ) %>%
  group_by(site, ibutton) %>%
  summarise(
    mean_shift = mean(depth_shift, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(fish_shift, aes(site, mean_shift)) +
  
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  
  geom_boxplot(
    width = 0.5,
    fill = "grey90",
    outlier.shape = NA
  ) +
  
  geom_jitter(
    width = 0.08,
    size = 2.5,
    alpha = 0.8
  )

fish_depth_summary %>%
  mutate(
    depth_shift = median_depth_maxDO - median_depth_minDO
  ) %>%
  ggplot(aes(site, depth_shift)) +
  
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  
  geom_boxplot(
    width = 0.5,
    fill = "grey90"
  )

fish_depth_summary %>%
  group_by(site) %>%
  summarise(
    n_fish = n_distinct(ibutton),
    n_fish_days = n(),
    .groups = "drop"
  )


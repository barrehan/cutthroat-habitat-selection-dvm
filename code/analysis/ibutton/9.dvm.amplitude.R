library(readr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(lme4)
library(lmerTest)

setwd("C:/Users/barrehan/Documents/GitHub/cwa.habitat.selection.dvm")
fish <-read.csv("data/modif.data/ibutton/all.ib.interp.depth.csv")
fish<-fish%>%
  mutate(date.time = parse_date_time(date.time, orders = "mdy HM"))
fish <-fish[fish$case ==1,]
fish<-fish %>% select(-case)

df <- fish %>%
  mutate(time = hms::as_hms(time),  # Convert to time format
    period = case_when(
      time >= hms::as_hms("09:00:00") & time < hms::as_hms("21:00:00") ~ "day",
      TRUE ~ "night"
    ))

# Get deepest depth during the day

deepest_day <- df %>%
  filter(period == "day") %>%
  group_by(site, date, ibutton.id) %>%
  slice_max(order_by = depth, n = 1, with_ties = FALSE) %>%
  transmute(
    site,
    date,
    ibutton.id,
    deepest_time = time,
    deepest_depth = depth
  )

# Get shallowest depth during the night
shallowest_night <- df %>%
  filter(period == "night") %>%
  group_by(site, date, ibutton.id) %>%
  slice_min(order_by = depth, n = 1, with_ties = FALSE) %>%
  transmute(
    site,
    date,
    ibutton.id,
    shallowest_time = time,
    shallowest_depth = depth
  )

# Combine summaries by full join
summary_df <- full_join(deepest_day, shallowest_night,
                        by = c("site", "date", "ibutton.id")) %>%
  arrange(site, date, ibutton.id)

# Calculate amplitude
summary_df <- summary_df %>%
  mutate(amplitude = deepest_depth - shallowest_depth)

# Limit dataframe to 7 day period that we tested for simulation

summary_df$date <- as.Date(summary_df$date, format = "%m/%d/%Y")
summary_df <- summary_df %>%
  filter(date >= as.Date("2021-07-30") & date <= as.Date("2021-08-06"))

# Confirm that DVM is happening in both alcoves
# First test for normality of depth differences (amplitude)

shapiro.test(filter(summary_df, site == "norwood")$amplitude)
shapiro.test(filter(summary_df, site == "blue.ruin")$amplitude)

# amplitude values not normally distributed, use wicoxin rank sum test
# putting "greater" means specifically testing DVM in the expected 
# direction (deeper during the day)

wilcox.test(
  x = filter(summary_df, site == "blue.ruin")$amplitude,
  mu = 0,
  alternative = "greater"
)

wilcox.test(
  x = filter(summary_df, site == "norwood")$amplitude,
  mu = 0,
  alternative = "greater"
)

# A Wilcoxon signed-rank test indicated that fish at both sites 
# exhibited significant DVM with daytime depths consistently deeper 
# than nighttime depths

# Gather day/night depths for plotting
depth_long <- summary_df %>%
  select(site, date, ibutton.id, deepest_depth, shallowest_depth) %>%
  pivot_longer(cols = c(deepest_depth, shallowest_depth),
               names_to = "period", values_to = "depth") %>%
  mutate(period = recode(period,
                         "deepest_depth" = "Day",
                         "shallowest_depth" = "Night"))

# Plot
p <- ggplot(depth_long, aes(x = period, y = depth, group = interaction(date, ibutton.id))) +
  geom_line(alpha = 0.4, color = "gray60") +
  geom_point(aes(color = period), size = 2) +
  scale_color_manual(
    values = c("Night" = "#A5236E", "Day" = "#FF5050"),
    name = NULL  # Remove "period" from legend
  ) +
  facet_wrap(~ site, labeller = as_labeller(c("blue.ruin" = "Blue Ruin", "norwood" = "Norwood"))) +
  scale_y_reverse() +
  labs(x = NULL, y = "Depth (m)", title = NULL) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    strip.background = element_blank(),         # Removes gray strip background
    axis.line = element_line(colour = "black"),
    legend.key = element_blank(),
    text = element_text(size = 20, family = "serif")
  )

# ggsave(p, filename = "results/figures/ibutton/day.night.depths.point.plot.png",
#        width = 25, height = 20, units = "cm")

# Boxplot of amplitudes per habitat
ggplot(summary_df, aes(x = site, y = amplitude)) +
  geom_boxplot(outlier.shape = NA) +
  scale_x_discrete(labels = c("blue.ruin" = "Blue Ruin", "norwood" = "Norwood")) +
  labs(x = NULL,  # Remove x-axis label
       y = "Vertical Amplitude (m)",
       title = "Fish Vertical Amplitude by Habitat") +
  theme_minimal() +
  theme(legend.position = "none")

# Is there a difference in amplitude between habitats? - fit LMM
model <- lmer(amplitude ~ site + (1 | ibutton.id) + (1 | date), data = summary_df)
summary(model)
anova(model)

# Difference in vertical amplitude between habitats was 
# not statistically significant, fish at 
# Blue Ruin exhibited greater variability and slightly 
# lower average amplitude compared to Norwood

# Is there a difference in DVM amplitude proportional to 
# total water column depth? orwood = 1.5m, Blue Ruin = 1.7m

summary_df <- summary_df %>%
  mutate(
    column_depth = if_else(site == "norwood", 1.6, 1.7),
    rel_amplitude = amplitude / column_depth
  )


lmer_rel <- lmer(rel_amplitude ~ site + (1 | ibutton.id) + (1 | date), data = summary_df)
anova(lmer_rel)

# No significant difference in relative amplitude - DVM ot influenced by habitat
# type


# Boxplot of mean amplitude
summary_df %>%
  group_by(ibutton.id, site) %>%
  summarize(mean_amplitude = mean(amplitude), .groups = "drop") %>%
  ggplot(aes(x = site, y = mean_amplitude)) +
  geom_boxplot(outlier.shape = NA, fill = "gray90") +
  geom_jitter(width = 0.2, aes(color = ibutton.id), size = 2) +
  theme_minimal()


# Could one fish have disproportionate affect on model output?
library(broom.mixed)

# Get full model estimate
full_model <- lmer(amplitude ~ site + (1 | ibutton.id), data = summary_df)
full_result <- fixef(full_model)["sitenorwood"]

# Loop over individuals
influence_test <- unique(summary_df$ibutton.id) %>%
  purrr::map_df(function(id) {
    model <- lmer(amplitude ~ site + (1 | ibutton.id),
                  data = filter(summary_df, ibutton.id != id))
    tibble(ibutton.id = id,
           site_effect = fixef(model)["sitenorwood"])
  })

ggplot(influence_test, aes(x = ibutton.id, y = site_effect)) +
  geom_point() +
  geom_hline(yintercept = full_result, linetype = "dashed") +
  labs(title = "Leave-One-Out Influence on Site Effect",
       y = "Estimated Site Effect (Norwood)", x = "Fish ID") +
  theme_minimal()

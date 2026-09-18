############################################################
# 1. LOAD PACKAGES
############################################################
library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)

############################################################
# 2. LOAD ARRAY DATA (BOTH SITES)
############################################################

# Blue Ruin
br_array <- read.csv("data/modif.data/logger.array/blue.ruin.netpen.array.do.temp.csv") %>%
  mutate(
    date.time = mdy_hm(date.time),
    site = "blue.ruin",
    date = as.Date(date.time)
  )

# Norwood
nor_array <- read.csv("data/raw.data/logger.array/norwood.mouth.temp.do.csv") %>%
  mutate(
    date.time = mdy_hm(date.time),
    site = "norwood",
    date = as.Date(date.time)
  )

############################################################
# 3. COMBINE + FILTER TO WEEK OF INTEREST
############################################################

array_df <- bind_rows(br_array, nor_array) %>%
  filter(
    date >= as.Date("2021-07-29"),
    date <= as.Date("2021-08-06")
  )

############################################################
# 4. ENSURE COLUMN NAMES + ALIGN TIME
############################################################

array_df <- array_df %>%
  rename(depth = sensor.depth) %>%
  mutate(
    date.time = floor_date(date.time, "5 minutes")
  )

############################################################
# 5. EFFECTIVE TEMPERATURE UNCERTAINTY (RELAXED)
############################################################

sigma_T_fish  <- 0.5
sigma_T_array <- 0.2

# original combined uncertainty
sigma_T_eff <- sqrt(sigma_T_fish^2 + sigma_T_array^2)

# ✅ RELAXED threshold (LESS stringent)
sigma_T_relaxed <- 0.5 * sigma_T_eff

print(paste("Original sigma_T_eff =", round(sigma_T_eff, 2), "°C"))
print(paste("Relaxed threshold =", round(sigma_T_relaxed, 2), "°C"))

############################################################
# 6. CALCULATE ΔT BETWEEN ADJACENT LOGGERS (CORRECTED)
############################################################

deltaT_df <- array_df %>%
  
  # ✅ KEEP ALL temperature values (DO + temp loggers)
  filter(!is.na(temperature)) %>%
  
  arrange(site, date.time, depth) %>%
  
  group_by(site, date.time) %>%
  
  # ✅ CRITICAL: enforce ONE value per depth level
  # prevents incorrect within-layer comparisons
  distinct(depth, .keep_all = TRUE) %>%
  
  mutate(
    dT = lead(temperature) - temperature,
    dz = lead(depth) - depth,
    mid_depth = (depth + lead(depth)) / 2
  ) %>%
  
  filter(
    !is.na(dT),
    dz > 0
  ) %>%
  
  ungroup()

############################################################
# 7. COMPUTE MAGNITUDE + RESOLVABILITY (UPDATED)
############################################################

deltaT_df <- deltaT_df %>%
  mutate(
    abs_dT = abs(dT),
    dTdz   = dT / dz,
    
    # ✅ USE RELAXED THRESHOLD
    resolvable = abs_dT >= sigma_T_relaxed
  )

############################################################
# 8. SUMMARY OF THERMAL STRUCTURE BY SITE
############################################################

summary_df <- deltaT_df %>%
  group_by(site) %>%
  summarize(
    prop_resolvable = mean(resolvable, na.rm = TRUE),
    mean_abs_dT     = mean(abs_dT, na.rm = TRUE),
    .groups = "drop"
  )

print(summary_df)

############################################################
# 9. HEATMAP: ΔT
############################################################

p1 <- ggplot(deltaT_df, aes(x = date.time, y = mid_depth, fill = abs_dT)) +
  geom_tile() +
  scale_fill_viridis_c(name = "ΔT (°C)") +
  scale_y_reverse() +
  facet_wrap(~site, ncol = 1) +
  labs(
    x = "Time",
    y = "Depth (m)",
    title = "Thermal structure (ΔT between adjacent loggers)"
  ) +
  theme_bw()

print(p1)

############################################################
# 10. RESOLVABLE VS HOMOGENEOUS
############################################################

p2 <- ggplot(deltaT_df, aes(x = date.time, y = mid_depth, fill = resolvable)) +
  geom_tile() +
  scale_fill_manual(
    values = c("grey80", "red"),
    labels = c("Homogeneous", "Resolvable"),
    name = paste0("ΔT ≥ ", round(sigma_T_relaxed, 2), " °C")  # ✅ updated label
  ) +
  scale_y_reverse() +
  facet_wrap(~site, ncol = 1) +
  labs(
    x = "Time",
    y = "Depth (m)",
    title = "Thermally resolvable vs homogeneous conditions"
  ) +
  theme_bw()

print(p2)

############################################################
# 11. LOAD FISH DATA
############################################################

br.fish <- read_csv(
  "data/modif.data/ibutton/do.depth.interpolation/br.all.ibuttons.depth.do.interpolation.csv",
  show_col_types = FALSE
) %>%
  mutate(
    date.time = ymd_hms(date.time),
    date = as.Date(date.time), 
    site  = "blue.ruin"
  ) %>%
  filter(
    date >= as.Date("2021-07-29"),
    date <= as.Date("2021-08-06")
  )

norwood.fish <- read_csv(
  "data/modif.data/ibutton/do.depth.interpolation/norwood.all.ibuttons.depth.do.interpolation.csv",
  show_col_types = FALSE
) %>%
  mutate(
    date.time = ymd_hms(date.time),
    date = as.Date(date.time),
    site = "norwood"
  ) %>%
  filter(
    date >= as.Date("2021-07-29"),
    date <= as.Date("2021-08-06")
  )

fish <- bind_rows(
  br.fish,
  norwood.fish
) 

############################################################
# 12. MATCH FISH TO NEAREST ΔT DEPTH
############################################################

fish <- fish %>%
  rename(fish_depth = fish.depth)

fish_match <- fish %>%
  left_join(deltaT_df,
            by = c("date.time", "site"),
            relationship = "many-to-many") %>%
  mutate(
    depth_diff = abs(fish_depth - mid_depth)
  ) %>%
  group_by(date.time, ibutton) %>%
  slice_min(depth_diff, n = 1, with_ties = FALSE) %>%
  ungroup()

############################################################
# 13. PROPORTION OF TIME FISH ARE RESOLVABLE
############################################################

fish_summary <- fish_match %>%
  summarize(
    prop_resolvable = mean(resolvable, na.rm = TRUE),
    prop_unresolvable = 1 - prop_resolvable,
    n_obs = n()
  )

print(fish_summary)

############################################################
# 14. BY SITE
############################################################

fish_summary_site <- fish_match %>%
  group_by(site) %>%
  summarize(
    prop_resolvable = mean(resolvable, na.rm = TRUE),
    prop_unresolvable = 1 - prop_resolvable,
    n_obs = n(),
    .groups = "drop"
  )

print(fish_summary_site)

############################################################
# 15. ΔT DISTRIBUTION WHERE FISH OCCUR
############################################################

p3 <- ggplot(fish_match, aes(x = abs_dT)) +
  geom_histogram(bins = 30, fill = "grey50") +
  geom_vline(xintercept = sigma_T_relaxed, color = "red", linewidth = 1) +
  facet_wrap(~site) +
  labs(
    x = "ΔT at fish location (°C)",
    y = "Frequency",
    title = "Thermal structure experienced by fish"
  ) +
  theme_bw()

print(p3)

############################################################
# 16. FISH OVERLAY ON THERMAL STRUCTURE
############################################################

p4 <- ggplot() +
  geom_tile(data = deltaT_df,
            aes(x = date.time, y = mid_depth, fill = abs_dT)) +
  geom_point(data = fish_match,
             aes(x = date.time, y = fish_depth),
             color = "white", size = 0.6, alpha = 0.5) +
  scale_fill_viridis_c(name = "ΔT (°C)") +
  scale_y_reverse() +
  facet_wrap(~site, ncol = 1) +
  labs(
    x = "Time",
    y = "Depth (m)",
    title = "Fish positions relative to thermal structure"
  ) +
  theme_bw()

print(p4)

############################################################
# 17. FISH OVERLAY ON RESOLVABLE/HOMOGENEOUS
############################################################

p5 <- ggplot(deltaT_df, aes(x = date.time, y = mid_depth, fill = resolvable)) +
  geom_tile() +
  geom_point(data = fish_match,
             aes(x = date.time, y = fish_depth),
             color = "black", size = 0.6, alpha = 0.5) +
  scale_fill_manual(
    values = c("grey80", "red"),
    labels = c("Homogeneous", "Resolvable"),
    name = paste0("ΔT ≥ ", round(sigma_T_relaxed, 2), " °C")
  ) +
  scale_y_reverse() +
  facet_wrap(~site, ncol = 1) +
  labs(
    x = "Time",
    y = "Depth (m)",
    title = "Thermally resolvable vs homogeneous conditions"
  ) +
  theme_bw()

print(p5)

############################################################
# 18. CREATE FILTERED FISH DATASETS
############################################################

# Original fish records (all observations)

fish_all <- fish_match

# Only fish occurring in thermally resolvable water

fish_resolvable <- fish_match %>%
  filter(resolvable) %>%
  rename(
    date = date.x,
    dissolved.oxygen = dissolved.oxygen.x
  ) %>%
  select(names(fish))

# Summary

cat(
  "\nOriginal observations:", nrow(fish_all),
  "\nResolvable observations:", nrow(fish_resolvable),
  "\nRemoved observations:", nrow(fish_all) - nrow(fish_resolvable),
  "\nPercent retained:", round(
    100 * nrow(fish_resolvable) / nrow(fish_all),
    1
  ),
  "%\n"
)


write_csv(
  fish_all,
  "data/modif.data/ibutton/all.ibuttons.depth.do.interpolation.original.csv"
)

write_csv(
  fish_resolvable,
  "data/modif.data/ibutton/all.ibuttons.depth.do.interpolation.resolvable.only.csv"
)

library(tidyverse)
library(lubridate)

#==============================================================================
# SENSOR ASSIGNMENTS
#==============================================================================

# NORWOOD
nor_temp_epi <- 0.00
nor_temp_hyp <- 1.45

nor_do_epi <- 0.25
nor_do_hyp <- 1.45

# BLUE RUIN
br_temp_epi <- 0.00
br_temp_hyp <- 1.55

br_do_epi <- 0.25
br_do_hyp <- 1.35

######################################
# Read in data files
#####################################

nordat <- read_csv(
  "data/raw.data/logger.array/norwood.mouth.temp.do.csv",
  show_col_types = FALSE
) %>%
  mutate(
    date.time = round_date(mdy_hm(date.time), unit = "5 minutes"),
    date = as.Date(date.time),
    hour = hour(date.time) + minute(date.time) / 60
  )

brdat <- read_csv(
  "data/modif.data/logger.array/blue.ruin.netpen.array.do.temp.csv",
  show_col_types = FALSE
) %>%
  mutate(
    date.time = round_date(mdy_hm(date.time), unit = "5 minutes"),
    date = as.Date(date.time),
    hour = hour(date.time) + minute(date.time) / 60
  )

#==============================================================================
# DATE RANGE
#==============================================================================

start_date <- as.Date("2021-07-31")
end_date   <- as.Date("2021-08-07")

nordat_sub <- nordat %>%
  filter(date >= start_date,
         date <= end_date)

brdat_sub <- brdat %>%
  filter(date >= start_date,
         date <= end_date)

#==============================================================================
# FUNCTION
#==============================================================================

calc_summary <- function(temp_dat, do_dat, layer_name) {
  
  temp_daily <- temp_dat %>%
    group_by(date) %>%
    summarise(
      daily_min_temp = min(temperature, na.rm = TRUE),
      daily_max_temp = max(temperature, na.rm = TRUE),
      .groups = "drop"
    )
  
  do_daily <- do_dat %>%
    group_by(date) %>%
    summarise(
      daily_min_do = min(dissolved.oxygen, na.rm = TRUE),
      daily_max_do = max(dissolved.oxygen, na.rm = TRUE),
      .groups = "drop"
    )
  
  tibble(
    Layer = layer_name,
    Temp_Min = mean(temp_daily$daily_min_temp, na.rm = TRUE),
    Temp_Max = mean(temp_daily$daily_max_temp, na.rm = TRUE),
    DO_Min = mean(do_daily$daily_min_do, na.rm = TRUE),
    DO_Max = mean(do_daily$daily_max_do, na.rm = TRUE)
  )
}

#####################################
# Norwood summary
#####################################

norwood_table <- bind_rows(
  
  calc_summary(
    temp_dat = nordat_sub %>%
      filter(sensor.depth == nor_temp_hyp),
    
    do_dat = nordat_sub %>%
      filter(sensor.depth == nor_do_hyp),
    
    layer_name = "Hypolimnion"
  ),
  
  calc_summary(
    temp_dat = nordat_sub %>%
      filter(sensor.depth == nor_temp_epi),
    
    do_dat = nordat_sub %>%
      filter(sensor.depth == nor_do_epi),
    
    layer_name = "Epilimnion"
  )
  
) %>%
  mutate(Site = "Norwood")

#####################################
# Blueruin summary
#####################################
blue_ruin_table <- bind_rows(
  
  calc_summary(
    temp_dat = brdat_sub %>%
      filter(sensor.depth == br_temp_hyp),
    
    do_dat = brdat_sub %>%
      filter(sensor.depth == br_do_hyp),
    
    layer_name = "Hypolimnion"
  ),
  
  calc_summary(
    temp_dat = brdat_sub %>%
      filter(sensor.depth == br_temp_epi),
    
    do_dat = brdat_sub %>%
      filter(sensor.depth == br_do_epi),
    
    layer_name = "Epilimnion"
  )
  
) %>%
  mutate(Site = "Blue Ruin")

######################################
# FORMAT NORWOOD TABLE
#####################################

norwood_out <- tibble(
  Site = "Norwood",
  Metric = c(
    "Average minimum hypolimnion",
    "Average maximum hypolimnion",
    "Average minimum epilimnetic",
    "Average maximum epilimnetic"
  ),
  Temp_C = c(
    norwood_table$Temp_Min[norwood_table$Layer == "Hypolimnion"],
    norwood_table$Temp_Max[norwood_table$Layer == "Hypolimnion"],
    norwood_table$Temp_Min[norwood_table$Layer == "Epilimnion"],
    norwood_table$Temp_Max[norwood_table$Layer == "Epilimnion"]
  ),
  DO_mg_L = c(
    norwood_table$DO_Min[norwood_table$Layer == "Hypolimnion"],
    norwood_table$DO_Max[norwood_table$Layer == "Hypolimnion"],
    norwood_table$DO_Min[norwood_table$Layer == "Epilimnion"],
    norwood_table$DO_Max[norwood_table$Layer == "Epilimnion"]
  )
)

#####################################
# FORMAT BLUE RUIN TABLE
#####################################

blue_ruin_out <- tibble(
  Site = "Blue Ruin",
  Metric = c(
    "Average minimum hypolimnion",
    "Average maximum hypolimnion",
    "Average minimum epilimnetic",
    "Average maximum epilimnetic"
  ),
  Temp_C = c(
    blue_ruin_table$Temp_Min[blue_ruin_table$Layer == "Hypolimnion"],
    blue_ruin_table$Temp_Max[blue_ruin_table$Layer == "Hypolimnion"],
    blue_ruin_table$Temp_Min[blue_ruin_table$Layer == "Epilimnion"],
    blue_ruin_table$Temp_Max[blue_ruin_table$Layer == "Epilimnion"]
  ),
  DO_mg_L = c(
    blue_ruin_table$DO_Min[blue_ruin_table$Layer == "Hypolimnion"],
    blue_ruin_table$DO_Max[blue_ruin_table$Layer == "Hypolimnion"],
    blue_ruin_table$DO_Min[blue_ruin_table$Layer == "Epilimnion"],
    blue_ruin_table$DO_Max[blue_ruin_table$Layer == "Epilimnion"]
  )
)

#####################################
# COMBINE AND ROUND
#####################################

table_s1 <- bind_rows(
  norwood_out,
  blue_ruin_out
) %>%
  mutate(
    Temp_C = round(Temp_C, 1),
    DO_mg_L = round(DO_mg_L, 1)
  )

table_s1

write_csv(
  table_s1,
  "final.figures/supplemental figures/table_s1_temperature_do_summary.csv"
)

library(tidyverse)
library(lubridate)
library(ggpubr)

br.logger.array <- read.csv(
  "data/modif.data/logger.array/blue.ruin.netpen.array.do.temp.csv"
) %>%
  mutate(
    date.time = mdy_hm(date.time),
    date.time = force_tz(date.time, "America/Los_Angeles"),
    date.time = round_date(date.time, "5 minutes"),
    sensor.depth = factor(sensor.depth)
  ) %>%
  filter(
    date.time >= ymd_hms("2021-07-29 00:00:00",
                         tz = "America/Los_Angeles"),
    date.time <= ymd_hms("2021-08-05 00:00:00",
                         tz = "America/Los_Angeles")
  )

nor.logger.array <- read.csv(
  "data/raw.data/logger.array/norwood.mouth.temp.do.csv"
) %>%
  mutate(
    date.time = mdy_hm(date.time),
    date.time = force_tz(date.time, "America/Los_Angeles"),
    date.time = round_date(date.time, "5 minutes"),
    sensor.depth = factor(sensor.depth)
  ) %>%
  filter(
    date.time >= ymd_hms("2021-07-29 00:00:00",
                         tz = "America/Los_Angeles"),
    date.time <= ymd_hms("2021-08-05 00:00:00",
                         tz = "America/Los_Angeles")
  )

# Filter to only sensors that record temperature and DO

br.do.temp <- br.logger.array %>%
  filter(
    !is.na(dissolved.oxygen))

nor.do.temp <- nor.logger.array %>%
  filter(
    !is.na(dissolved.oxygen)
  )

# Function to calculate DO % saturation

do_sat <- function(temp_c) {
  14.652 -
    0.41022 * temp_c +
    0.007991 * temp_c^2 -
    0.000077774 * temp_c^3
}

nor.logger.array <- nor.logger.array %>%
  mutate(
    do.sat.conc = do_sat(temperature),
    do.pct.sat = dissolved.oxygen / do.sat.conc * 100
  )

br.logger.array <- br.logger.array %>%
  mutate(
    do.sat.conc = do_sat(temperature),
    do.pct.sat = dissolved.oxygen / do.sat.conc * 100
  )

# Daily maximum DO % saturation by sensor

nor.daily.max <- nor.logger.array %>%
  filter(!is.na(do.pct.sat)) %>%
  mutate(date = as.Date(date.time)) %>%
  group_by(sensor.depth, date) %>%
  summarize(
    daily.max.sat = max(do.pct.sat),
    .groups = "drop"
  )

nor.avg.daily.max <- nor.logger.array %>%
  filter(!is.na(do.pct.sat)) %>%
  mutate(date = as.Date(date.time)) %>%
  group_by(date) %>%
  summarize(
    daily.max.sat = max(do.pct.sat),
    .groups = "drop"
  ) %>%
  summarize(
    avg.daily.max.sat = mean(daily.max.sat)
  )

nor.avg.daily.max

br.daily.max <- br.logger.array %>%
  filter(!is.na(do.pct.sat)) %>%
  mutate(date = as.Date(date.time)) %>%
  group_by(sensor.depth, date) %>%
  summarize(
    daily.max.sat = max(do.pct.sat, na.rm = TRUE),
    .groups = "drop"
  )

br.avg.daily.max <- br.logger.array %>%
  filter(!is.na(do.pct.sat)) %>%
  mutate(date = as.Date(date.time)) %>%
  group_by(date) %>%
  summarize(
    daily.max.sat = max(do.pct.sat),
    .groups = "drop"
  ) %>%
  pull(daily.max.sat) %>%
  mean()

br.avg.daily.max

    
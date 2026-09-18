# ---------------------------------------
# Setup
# ---------------------------------------
rm(list = ls())

library(readr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)

# ---------------------------------------
# USER INPUT
# ---------------------------------------
site <- "norwood"   # "norwood" or "blueruin"

start_time <- "2021-07-26 00:00:00"
end_time   <- "2021-08-14 00:00:00"

# ---------------------------------------
# Site-specific settings
# ---------------------------------------
if (site == "norwood") {
  
  file_in  <- "data/raw.data/logger.array/norwood.mouth.temp.do.csv"
  file_out <- "data/modif.data/logger.array/nor.unif.do.temp.set.depth.simulation.csv"
  max_depth <- 1.5
  
} else if (site == "blueruin") {
  
  file_in  <- "data/modif.data/logger.array/blue.ruin.netpen.array.do.temp.csv"
  file_out <- "data/modif.data/logger.array/br.unif.do.temp.set.depth.simulation.csv"
  max_depth <- 1.6
  
} else {
  stop("Invalid site name")
}

# ---------------------------------------
# Load data
# ---------------------------------------
array <- read.csv(file_in)

array$date.time <- mdy_hm(array$date.time)
array <- array %>% force_tz(tzone = "America/Los_Angeles")

# OPTIONAL but safe: ensure consistent 5-min bins
array$date.time <- round_date(array$date.time, "5 minutes")

# ---------------------------------------
# Filter date range
# ---------------------------------------
array <- array %>%
  filter(
    date.time >= as.POSIXct(start_time, tz = "America/Los_Angeles"),
    date.time <  as.POSIXct(end_time,   tz = "America/Los_Angeles")
  )

# ---------------------------------------
# Split temp and DO
# ---------------------------------------
temp.array <- array %>% drop_na(temperature)
do.array   <- array %>% drop_na(dissolved.oxygen)

# ---------------------------------------
# Create grid
# ---------------------------------------
dates <- data.frame(date.time = unique(array$date.time))
depth_seq <- seq(0.2, max_depth, by = 0.1)

row.ct <- nrow(dates) * length(depth_seq)

# ---------------------------------------
# Setup
# ---------------------------------------
rm(list = ls())

library(readr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)

# ---------------------------------------
# USER INPUT
# ---------------------------------------
site <- "blueruin"   # "norwood" or "blueruin"

start_time <- "2021-07-26 00:00:00"
end_time   <- "2021-08-14 00:00:00"

# ---------------------------------------
# Site-specific settings
# ---------------------------------------
if (site == "norwood") {
  
  file_in  <- "data/raw.data/logger.array/norwood.mouth.temp.do.csv"
  file_out <- "data/modif.data/logger.array/nor.unif.do.temp.set.depth.simulation.csv"
  max_depth <- 1.5
  
} else if (site == "blueruin") {
  
  file_in  <- "data/modif.data/logger.array/blue.ruin.netpen.array.do.temp.csv"
  file_out <- "data/modif.data/logger.array/br.unif.do.temp.set.depth.simulation.csv"
  max_depth <- 1.6
  
} else {
  stop("Invalid site name")
}

# ---------------------------------------
# Load data
# ---------------------------------------
array <- read.csv(file_in)

array$date.time <- mdy_hm(array$date.time)
array <- array %>% force_tz(tzone = "America/Los_Angeles")

# OPTIONAL but safe: ensure consistent 5-min bins
array$date.time <- round_date(array$date.time, "5 minutes")

# ---------------------------------------
# Filter date range
# ---------------------------------------
array <- array %>%
  filter(
    date.time >= as.POSIXct(start_time, tz = "America/Los_Angeles"),
    date.time <  as.POSIXct(end_time,   tz = "America/Los_Angeles")
  )

# ---------------------------------------
# Split temp and DO
# ---------------------------------------
temp.array <- array %>% drop_na(temperature)
do.array   <- array %>% drop_na(dissolved.oxygen)

# ---------------------------------------
# Create grid
# ---------------------------------------
dates <- data.frame(date.time = unique(array$date.time))
depth_seq <- seq(0.2, max_depth, by = 0.1)

row.ct <- nrow(dates) * length(depth_seq)

new.dat <- data.frame(
  date.time = as.POSIXct(rep(NA, row.ct), origin = "1970-01-01", tz = "America/Los_Angeles"),
  depth = rep(NA, row.ct),
  temperature = rep(NA, row.ct)
)


cntr <- 0

# ---------------------------------------
# TEMPERATURE INTERPOLATION
# ---------------------------------------
for (k in 1:nrow(dates)) {
  
  dt <- dates$date.time[k]
  match <- temp.array[temp.array$date.time == dt, ]
  
  if (nrow(match) == 0) next
  
  min_d <- min(match$sensor.depth)
  max_d <- max(match$sensor.depth)
  
  for (j in 1:length(depth_seq)) {
    
    d.unif <- depth_seq[j]
    
    if (d.unif %in% match$sensor.depth) {
      
      t <- match$temperature[match$sensor.depth == d.unif]
      
    } else if (d.unif > max_d) {
      
      t <- match$temperature[match$sensor.depth == max_d]
      
    } else if (d.unif < min_d) {
      
      t <- match$temperature[match$sensor.depth == min_d]
      
    } else {
      
      d1 <- max(match$sensor.depth[match$sensor.depth < d.unif])
      d2 <- min(match$sensor.depth[match$sensor.depth > d.unif])
      
      t1 <- match$temperature[match$sensor.depth == d1]
      t2 <- match$temperature[match$sensor.depth == d2]
      
      t <- (((d.unif - d1) * (t2 - t1)) / (d2 - d1)) + t1
    }
    
    cntr <- cntr + 1
    new.dat[cntr, ] <- list(dt, d.unif, t)
  }
}

colnames(new.dat) <- c("date.time", "depth", "temperature")

# ---------------------------------------
# DO INTERPOLATION
# ---------------------------------------
dater <- new.dat
dater$dissolved.oxygen <- NA
dater$case <- 0

for (i in 1:nrow(dater)) {
  
  row <- dater[i, ]
  match <- do.array[do.array$date.time == row$date.time, ]
  
  if (nrow(match) == 0) next
  
  depth.est <- row$depth
  
  min_d <- min(match$sensor.depth)
  max_d <- max(match$sensor.depth)
  
  closest <- match[which.min(abs(depth.est - match$sensor.depth)), ]
  closest.do <- closest$dissolved.oxygen
  
  if (depth.est %in% match$sensor.depth) {
    
    do <- match$dissolved.oxygen[match$sensor.depth == depth.est]
    
  } else if (depth.est > max_d) {
    
    do <- match$dissolved.oxygen[match$sensor.depth == max_d]
    
  } else if (depth.est < min_d) {
    
    do <- match$dissolved.oxygen[match$sensor.depth == min_d]
    
  } else {
    
    x1 <- max(match$sensor.depth[match$sensor.depth < depth.est])
    x2 <- min(match$sensor.depth[match$sensor.depth > depth.est])
    
    d1 <- match$dissolved.oxygen[match$sensor.depth == x1]
    d2 <- match$dissolved.oxygen[match$sensor.depth == x2]
    
    do <- (d2 - d1) / (x2 - x1) * (depth.est - x1) + d1
  }
  
  if (length(do) == 0 || is.na(do)) {
    do <- closest.do
  }
  
  dater$dissolved.oxygen[i] <- do
}

# ---------------------------------------
# Save
# ---------------------------------------
write.csv(dater, file_out, row.names = FALSE)

cntr <- 0

# ---------------------------------------
# TEMPERATURE INTERPOLATION
# ---------------------------------------
for (k in 1:nrow(dates)) {
  
  dt <- dates$date.time[k]
  match <- temp.array[temp.array$date.time == dt, ]
  
  if (nrow(match) == 0) next
  
  min_d <- min(match$sensor.depth)
  max_d <- max(match$sensor.depth)
  
  for (j in 1:length(depth_seq)) {
    
    d.unif <- depth_seq[j]
    
    if (d.unif %in% match$sensor.depth) {
      
      t <- match$temperature[match$sensor.depth == d.unif]
      
    } else if (d.unif > max_d) {
      
      t <- match$temperature[match$sensor.depth == max_d]
      
    } else if (d.unif < min_d) {
      
      t <- match$temperature[match$sensor.depth == min_d]
      
    } else {
      
      d1 <- max(match$sensor.depth[match$sensor.depth < d.unif])
      d2 <- min(match$sensor.depth[match$sensor.depth > d.unif])
      
      t1 <- match$temperature[match$sensor.depth == d1]
      t2 <- match$temperature[match$sensor.depth == d2]
      
      t <- (((d.unif - d1) * (t2 - t1)) / (d2 - d1)) + t1
    }
    
    cntr <- cntr + 1
    new.dat[cntr, ] <- list(dt, d.unif, t)
  }
}

colnames(new.dat) <- c("date.time", "depth", "temperature")

# ---------------------------------------
# DO INTERPOLATION
# ---------------------------------------
dater <- new.dat
dater$dissolved.oxygen <- NA
dater$case <- 0

for (i in 1:nrow(dater)) {
  
  row <- dater[i, ]
  match <- do.array[do.array$date.time == row$date.time, ]
  
  if (nrow(match) == 0) next
  
  depth.est <- row$depth
  
  min_d <- min(match$sensor.depth)
  max_d <- max(match$sensor.depth)
  
  closest <- match[which.min(abs(depth.est - match$sensor.depth)), ]
  closest.do <- closest$dissolved.oxygen
  
  if (depth.est %in% match$sensor.depth) {
    
    do <- match$dissolved.oxygen[match$sensor.depth == depth.est]
    
  } else if (depth.est > max_d) {
    
    do <- match$dissolved.oxygen[match$sensor.depth == max_d]
    
  } else if (depth.est < min_d) {
    
    do <- match$dissolved.oxygen[match$sensor.depth == min_d]
    
  } else {
    
    x1 <- max(match$sensor.depth[match$sensor.depth < depth.est])
    x2 <- min(match$sensor.depth[match$sensor.depth > depth.est])
    
    d1 <- match$dissolved.oxygen[match$sensor.depth == x1]
    d2 <- match$dissolved.oxygen[match$sensor.depth == x2]
    
    do <- (d2 - d1) / (x2 - x1) * (depth.est - x1) + d1
  }
  
  if (length(do) == 0 || is.na(do)) {
    do <- closest.do
  }
  
  dater$dissolved.oxygen[i] <- do
}

# ---------------------------------------
# Save
# ---------------------------------------
write.csv(dater, file_out, row.names = FALSE)
# ------------------------------------------------------------------------------
# Blue Ruin iButton Depth and DO Interpolation
# Original interpolation logic preserved
# ------------------------------------------------------------------------------

rm(list = ls())

library(readr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)

# ------------------------------------------------------------------------------
# Load logger array once
# ------------------------------------------------------------------------------

logger.array <- read.csv(
  "data/modif.data/logger.array/blue.ruin.netpen.array.do.temp.csv"
)

logger.array$date.time <- mdy_hm(
  logger.array$date.time
)

logger.array <- logger.array %>%
  force_tz(
    logger.array$date.time,
    tzone = "America/Los_Angeles"
  )

logger.array <- logger.array[
  logger.array$date.time < "2021-08-13 00:00:00",
]

temp.array <- logger.array %>%
  drop_na(temperature)

do.array <- logger.array %>%
  drop_na(dissolved.oxygen)

# ------------------------------------------------------------------------------
# Function
# ------------------------------------------------------------------------------

process_ibutton <- function(file){
  
  message("Processing: ", basename(file))
  
  # ---------------------------------------------------------------------------
  # Determine pen/site from filename
  # ---------------------------------------------------------------------------
  
  if(grepl("pen1", basename(file))){
    
    site.name <- "blue.ruin.1"
    
  } else if(grepl("pen2", basename(file))){
    
    site.name <- "blue.ruin.2"
    
  } else{
    
    stop(
      "Could not determine pen from filename: ",
      basename(file)
    )
    
  }
  
  # ---------------------------------------------------------------------------
  # Read iButton
  # ---------------------------------------------------------------------------
  
  ib <- read.csv(file)
  
  ib$date.time <- mdy_hms(
    ib$date.time
  )
  
  ib <- ib %>%
    force_tz(
      ib$date.time,
      tzone = "America/Los_Angeles"
    )
  
  # ---------------------------------------------------------------------------
  # Deployment period
  # ---------------------------------------------------------------------------
  
  ib <- ib[
    ib$date.time > "2021-07-26 00:00:00" &
      ib$date.time < "2021-08-10 00:00:00",
  ]
  
  # ---------------------------------------------------------------------------
  # Match iButton timestamps to logger array
  # ---------------------------------------------------------------------------
  
  ans <- vapply(
    ib$date.time,
    function(x) x - logger.array$date.time,
    numeric(nrow(logger.array))
  )
  
  indx <- apply(
    abs(ans),
    2,
    which.min
  )
  
  time.match <- cbind(
    ib,
    logger.array[indx, ]
  )
  
  # ---------------------------------------------------------------------------
  # Clean dataframe exactly as original script
  # ---------------------------------------------------------------------------
  
  time.match <- time.match[, -c(2:4, 8:16)]
  
  time.match <- time.match %>%
    rename(
      ibutton.temp = temperature.c
    )
  
  # ---------------------------------------------------------------------------
  # Create output dataframe exactly as original script
  # ---------------------------------------------------------------------------
  
  row.ct <- nrow(time.match)
  
  new.dat <- as.data.frame(
    matrix(
      ncol = 5,
      nrow = row.ct
    )
  )
  
  new.dat$V2 <- mdy_hms(new.dat$V2)
  
  new.dat$V2 <- force_tz(
    new.dat$V2,
    tzone = "America/Los_Angeles"
  )
  
  cntr <- 0
  
  # ---------------------------------------------------------------------------
  # Temperature -> Depth interpolation
  # ---------------------------------------------------------------------------
  
  for(i in 1:nrow(time.match)){
    
    row <- time.match[i, ]
    
    match <- temp.array[
      temp.array$date.time == row$date.time,
    ]
    
    t1 <- row$ibutton.temp
    
    x1 <- max(
      match$temperature[
        which(match$temperature < t1)
      ]
    )
    
    x2 <- min(
      match$temperature[
        which(match$temperature > t1)
      ]
    )
    
    d1 <- match$sensor.depth[
      match$temperature == x1
    ]
    
    d2 <- match$sensor.depth[
      match$temperature == x2
    ]
    
    d <- (d2 - d1) /
      (x2 - x1) *
      (t1 - x1) +
      d1
    
    depth <- ifelse(
      length(d > 1),
      mean(d),
      d
    )
    
    depth <- ifelse(
      is.na(depth),
      1.60,
      depth
    )
    
    cntr <- cntr + 1
    
    new.dat[cntr, 1] <- row$ibutton
    new.dat[cntr, 2] <- row$date.time
    new.dat[cntr, 3] <- row$ibutton.temp
    new.dat[cntr, 4] <- site.name
    new.dat[cntr, 5] <- depth
  }
  
  colnames(new.dat) <- c(
    "ibutton",
    "date.time",
    "ibutton.temp",
    "site",
    "fish.depth"
  )
  
  # ---------------------------------------------------------------------------
  # Depth -> DO interpolation
  # ---------------------------------------------------------------------------
  
  for(i in 1:nrow(new.dat)){
    
    row <- new.dat[i, ]
    
    match <- do.array[
      do.array$date.time == row$date.time,
    ]
    
    depth.est <- row$fish.depth
    
    closest <- match[
      which.min(
        abs(depth.est - match$sensor.depth)
      ),
    ]
    
    closest.do <- closest$dissolved.oxygen
    
    x1 <- max(
      match$sensor.depth[
        which(match$sensor.depth < depth.est)
      ]
    )
    
    x2 <- min(
      match$sensor.depth[
        which(match$sensor.depth > depth.est)
      ]
    )
    
    d1 <- match$dissolved.oxygen[
      match$sensor.depth == x1
    ]
    
    d2 <- match$dissolved.oxygen[
      match$sensor.depth == x2
    ]
    
    do <- (d2 - d1) /
      (x2 - x1) *
      (depth.est - x1) +
      d1
    
    do <- ifelse(
      is_empty(do),
      closest.do,
      do
    )
    
    new.dat[i, 6] <- do
  }
  
  colnames(new.dat)[6] <- "dissolved.oxygen"
  
  return(new.dat)
}

## ------------------------------------------------------------------------------
# Find all Blue Ruin iButton files
# ------------------------------------------------------------------------------

ibutton.files <- list.files(
  path = "data/raw.data/ibutton/blue.ruin.netpens",
  pattern = "^ibutton.*\\.csv$",
  full.names = TRUE
)

# ------------------------------------------------------------------------------
# Skip tags 1 and 3 (fell off)
# ------------------------------------------------------------------------------

ibutton.files <- ibutton.files[
  !grepl(
    "ibutton\\.(01|03)\\.",
    basename(ibutton.files)
  )
]

# ------------------------------------------------------------------------------
# Process tags
# ------------------------------------------------------------------------------

all.tags <- list()

for(file in ibutton.files){
  
  tag.dat <- process_ibutton(file)
  
  tag.id <- str_extract(
    basename(file),
    "(?<=ibutton\\.)\\d+"
  )
  
  write.csv(
    tag.dat,
    paste0(
      "data/modif.data/ibutton/do.depth.interpolation/",
      "br.ibutton.",
      tag.id,
      ".depth.do.interpolation.csv"
    ),
    row.names = FALSE
  )
  
  all.tags[[tag.id]] <- tag.dat
}

# ------------------------------------------------------------------------------
# Combine all tags
# ------------------------------------------------------------------------------

all.ibuttons <- bind_rows(all.tags)

write.csv(
  all.ibuttons,
  "data/modif.data/ibutton/do.depth.interpolation/br.all.ibuttons.depth.do.interpolation.csv",
  row.names = FALSE
)

save(
  all.ibuttons,
  all.tags,
  file = "data/modif.data/ibutton/do.depth.interpolation/br.all.ibuttons.depth.do.interpolation.RData"
)

message(
  "Finished processing ",
  length(all.tags),
  " Blue Ruin tags."
)

##################Detection efficiency#############################
#' create column that counts detection patterns to determine detection efficiency
#' 1 means zero misses 0 means missed  (skipped a receiver)
#' good (sequential antenna) reads == 11, 12, 21, 22, 23, 32, 33, 34, 43, 44
#' all others will be given 0

all.ib <- read.csv("data/modif.data/radio.tag/tag.reads.10.min.interval.csv")
all.ib$date.time <- mdy_hm(all.ib$date.time)
all.ib <- all.ib %>% force_tz(all.ib$date.time, tzone = "America/Los_Angeles")
all.ib<-all.ib[order(all.ib$tag.id, all.ib$date.time),]
all.ib<-all.ib[all.ib$date.time >="2021-07-26 00:00:00",]

all.ib$detection.id<-ifelse(all.ib$receiver.site == 1 & shift(all.ib$receiver.site == 1, n = 1L, type = "lag"), 1, 
                            ifelse(all.ib$receiver.site ==1 & shift(all.ib$receiver.site ==2, n = 1L, type = "lag"), 1,
                                   ifelse(all.ib$receiver.site == 2 & shift(all.ib$receiver.site == 1, n = 1L, type = "lag"), 1,
                                          ifelse(all.ib$receiver.site == 2 & shift(all.ib$receiver.site ==2, n = 1L, type = "lag"), 1,
                                                 ifelse(all.ib$receiver.site == 2 & shift(all.ib$receiver.site == 3, n = 1L, type = "lag"), 1,
                                                        ifelse(all.ib$receiver.site == 3 & shift(all.ib$receiver.site == 2, n = 1L, type = "lag"), 1,
                                                               ifelse(all.ib$receiver.site == 3 & shift(all.ib$receiver.site == 3, n = 1L, type = "lag"), 1,
                                                                      ifelse(all.ib$receiver.site == 3 & shift(all.ib$receiver.site == 4, n = 1L, type = "lag"), 1,
                                                                             ifelse(all.ib$receiver.site == 4 & shift(all.ib$receiver.site == 3, n = 1L, type = "lag"), 1,
                                                                                    ifelse(all.ib$receiver.site == 4 & shift(all.ib$receiver.site == 4, n = 1L, type = "lag"), 1, 0))))))))))
all.ib<-rowid_to_column(all.ib, "unique.id")


first.read<-all.ib %>% 
  group_by(tag.id) %>%
  filter(date.time == min(date.time))
v<- first.read$unique.id

#'if unique id value matches vector of unique ids from first tag read replace detection.id with NA

all.ib$detection.id[all.ib$unique.id %in% v] <- NA

#'detection efficiency as a group and per individual

detects<-na.exclude(count(all.ib$detection.id[all.ib$detection.id ==1],))
miss<- na.exclude(count(all.ib$detection.id[all.ib$detection.id == 0],))

efficiency <-detects$freq/(detects$freq+miss$freq)

#' detection efficiency count sum of all detect codes (unique) vs non-detect
#' 14 stands for detected previously site 1 next site 4 this becomes a missed detection
#' 12 for detected previously site 1 next site 2, this is a good detection
#' code so only manual at last step
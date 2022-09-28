library(readr)
library(tidyverse)
library(lubridate)
library(dplyr)
library(data.table)

setwd("C:/Users/barrehan/Box/projects/2021.alcove.DO.project")

all_tags <- read.csv("data/radio.tag.data/2021.all.tag.reads.raw.csv")


##remove temperatures below 10 and above 25, tags below 11 and above 40 (error reads),
##remove tag #28 since this was the test tag (we had in the car on accident a few times...)

tags <- all_tags %>%
  filter(temperature.c >= 10) %>%
  filter(temperature.c < 25) %>%
  filter(tag.id >=11) %>%
  filter(tag.id <40) 
tags <- tags[!(tags$tag.id == 28),]

tags$date_time = paste(tags$date, tags$time)

tags$date_time = mdy_hms(tags$date_time)

##create new column that bins time into 10 minute intervals

tags$binned.10.min <- floor_date(tags$date_time, "10 minutes")

#list of all unique tag IDs

all.tags<-unique(tags$tag.id)

# new data frame with length of unique 10 minute binned date.time, columns for wanted data

row.ct<-length(unique(tags$binned.10.min))
new.dat<-as.data.frame(matrix(ncol=9,nrow=row.ct))
names(new.dat)<-c("date.time","tag.id", "receiver.site", "antenna.number", "temp.sd","temp.strong","lat","long", "strength")


cntr = 0 # counter to assign output data to row in new.dat, reset first

for (i in 1:length(all.tags)){  # step through i's from 1 to however many tags there are
  tags.i<-na.omit(tags[tags$tag.id==all.tags[i],])  #filter the data to where the tag.id = the ith tag in our list
  
  for(j in 1:length(unique(tags.i$binned.10.min))){ #for each tag i, step through each 10 minute bin j
    
    tags.ij<-tags.i[tags.i$binned.10.min==unique(tags.i$binned.10.min)[j],]#subset for date = jth observation of date
    best.row<-which.max(tags.ij$strength) #which row has highest signal strength?
    if(length(best.row)>1) {best.row<-best.row[1]}
    lat.ij<-tags.ij$latitude[best.row] #grab lat from row with highest strength
    long.ij<-tags.ij$longitude[best.row] #grab long from row with highest strength
    temp.ij<-tags.ij$temperature.c[best.row]#grab temperature from row with highest strength
    antenna.ij <- tags.ij$antenna[best.row]
    site.ij <- tags.ij$site[best.row]
    strength.ij <- tags.ij$strength[best.row]
    sd.temp.ij<-sd(tags.ij$temperature.c) #st of temperature from this time bin
    
    #record output data for new dataframe
    cntr<-cntr+1 #start a new row
    new.dat[cntr,1]<-format(unique(tags.ij$binned.10.min))
    new.dat[cntr,2]<-unique(tags.ij$tag.id)
    new.dat[cntr,3]<-site.ij
    new.dat[cntr,4]<- antenna.ij
    new.dat[cntr,5]<-sd.temp.ij
    new.dat[cntr,6]<-temp.ij
    new.dat[cntr,7]<-lat.ij
    new.dat[cntr,8]<-long.ij
    new.dat[cntr,9]<-strength.ij
  }
  
}

unique(new.dat$tag.id)

write.csv(new.dat, "data/radio.tag.data/tag.reads.10.min.interval.csv")

tag.11 <- new.dat %>%
  filter(tag.id == 11)

ggplot(tag.11, aes(x=date.time, y = receiver.site))+
  geom_point()+
  

tag.12 <- new.dat %>%
  filter(tag.id == 12)

tag.13 <- new.dat %>%
  filter(tag.id == 13)

tag.14 <- new.dat %>%
  filter(tag.id == 14)

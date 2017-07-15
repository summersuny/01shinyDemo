install.packages('purrr')
library(shiny)
library(RColorBrewer)
library(scales)
library(lattice)
library(leaflet)
#library(geosphere)
library(tidycensus)
library(tidyverse)
#census_api_key('2e97f0e5539829feaf9c33107bb69781bb94edb5')
#library(jsonlite)
library(googleVis)
library(dplyr)
library(ggplot2)

#data loading/ 3 month data (Jan - Mar 2017)
setwd('~/R_files/ShinyCitiBike/')
temp = list.files(pattern="cb\\d+\\.csv")
myfiles = lapply(temp, function(x) {
  dum=read.csv(x,stringsAsFactors = FALSE)
})
bike <- do.call(rbind.data.frame, myfiles)

#station data
# url with some information about project in Andalussia
url <- 'https://feeds.citibikenyc.com/stations/stations.json'

# read url and convert to data.frame
stations <- as.data.frame(fromJSON(txt=url))
stations <- stations %>% select(stationBeanList.id,stationBeanList.availableDocks)  
colnames(stations) <- c('Station.ID','Total.Docks')
df<- left_join(bike,stations,by=c("Start.Station.ID"="Station.ID"))
colnames(df)[16] <- 'Start.Total.Docks'
df<- left_join(df,stations,by=c("End.Station.ID"="Station.ID"))
colnames(df)[17]='End.Total.Docks'


#convert time columns to date formart and seperate time
#hr and minutes to be input by user and filter the data
df <- df%>% separate(Start.Time, into=c('start.date','start.ts'),sep='\\s') %>% 
  separate(Stop.Time, into=c('stop.date','stop.ts'),sep='\\s') %>% 
  mutate(hr=substr(start.ts,1,2)) 
#df$start.time <-df$start.ts <- as.POSIXct(df$start.ts, format="%H:%M:%S")
#df$stop.time <-df$stop.ts <- strpftime(df$stop.ts, format="%H:%M:%S")
df$start.date=as.Date(df$start.date)
df$stop.date=as.Date(df$stop.date)
df <- df %>% mutate(weekday=ifelse(weekdays(start.date) %in% c("Sarturday","Sunday"),'Weekends','Weekday'))
save(df,file="~/Desktop/SHINY/data.Rda")

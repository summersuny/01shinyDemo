library(leaflet)
#install.packages("zipcode")
#install.packages("ggthemes")
library(ggthemes)
library(tidycensus)
library(tidyverse)
library(jsonlite)
library(googleVis)
library(dplyr)
library(ggplot2)
library(zipcode)
setwd('~/Desktop/SHINY')

load("data.Rda")

###########
library(dplyr)
library(zipcode)
library(RCurl)
library(zipcode)
library(ggthemes)


# temporarily remove colleges with missing data 
# cleantable <- na.omit(cleantable)
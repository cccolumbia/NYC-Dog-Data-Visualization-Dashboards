library(ggplot2)
library(ggmap)
library(tidyverse)
library(maps)
library(googleway)
dogbites=read.csv("dogbites2.csv")
dogbites=na.omit(dogbites)
dogbites=dogbites[dogbites$lon<(-70)&dogbites$lon>(-75)&dogbites$lat>39&dogbites$lat<42,]

#need to replace value of key
key <- "AIzaSyBbJbhg97t5oEGf9m39krg9Dz-8LkB9bkI"

google_map(data = dogruns,key=key) %>%
  add_heatmap(lat="lat", lon="lon", option_radius = 0.01) 

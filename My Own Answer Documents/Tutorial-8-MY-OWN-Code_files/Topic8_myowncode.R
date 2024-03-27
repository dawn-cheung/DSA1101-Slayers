library(tidyverse)

hdb = read.csv("~/Github/DSA1101 Slayers/datasets/hdbresale_cluster.csv")
glimpse(hdb)

dim(hdb) #774 observations, 4 variables
table(hdb$flat_type) #all are 3 room flats
attach(hdb)

plot(floor_area_sqm, amenities)
#we see that there are 2 subsets naturally; we call them clusters


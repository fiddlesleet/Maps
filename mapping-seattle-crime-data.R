library(ggmap)
library(dplyr)
library(ggplot2)
library(data.table)

#########################
# GET SEATTLE CRIME DATA
#########################

download.file("http://www.sharpsightlabs.com/wp-content/uploads/2015/01/seattle_crime_2010_to_2014_REDUCED.txt.zip", destfile="seattle_crime_2010_to_2014_REDUCED.txt.zip")

#------------------------------
# Unzip the SF crime data file
#------------------------------
unzip("seattle_crime_2010_to_2014_REDUCED.txt.zip")

#------------------------------------
# Read crime data into an R dataframe
#------------------------------------
df.seattle_crime <- fread("seattle_crime_2010_to_2014_REDUCED.txt")
str(df.seattle_crime)

################
# SEATTLE GGMAP
################

map.seattle <- qmap("seattle", zoom = 11, source = "stamen", maptype = "watercolor")
map.seattle

##########################
# CREATE BASIC MAP
#  - dot distribution map
##########################

# make points semi-transparent so variance in density shows
map.seattle <- map.seattle +
  geom_point(data = df.seattle_crime, aes(x=Longitude, y=Latitude, color = "dark green", alpha = .03, size = 1.1))

## this is just a scatterplot
ggplot() +
  geom_point(data=df.seattle_crime, aes(x=Longitude, y=Latitude))

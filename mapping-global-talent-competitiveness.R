#=================
# INSTALL PACKAGES
#=================
library(tidyverse)
library(rvest)
library(magrittr)
library(ggmap)
library(stringr)

#============
# SCRAPE DATA 
#============

html.global_talent <- read_html("https://www.insead.edu/news/2017-global-talent-competitiveness-index-davos")

#===========
# CLEAN DATA
#===========

clean_data <- function(tableN) {
  
  # global talent countries
  df.RAW <- html.global_talent %>%
    html_nodes("table") %>%
    .[[tableN]] %>%
    html_table()
  
  # split df in 2
  df1 <- df.RAW %>%
    select(X1, X2) %>%
    rename(rank = X1, 
           country = X2)
  
  df2 <- df.RAW %>%
    select(X3, X4) %>%
    rename(rank = X3, 
           country = X4)
  
  # combine splits
  df <- rbind(df1, df2)
  
  df
}

# get top countries
df.global_talent_countries <- clean_data(1)
# inspect
df.global_talent_countries

# get top cities
df.global_talent_cities <- clean_data(2)
# inspect
df.global_talent_cities

#==============
# GET WORLD MAP
#==============

map.world <- map_data("world")



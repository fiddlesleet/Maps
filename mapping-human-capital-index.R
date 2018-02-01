# mapping the World Economic Forum's Human Capital Index 
# human capital: cutting-edge knowledge, judgement (esp. in complex domains), & creativity 

library(dplyr)
library(stringr)
library(readr)
library(rvest)
library(viridis)
library(ggmap)

##########
# GET DATA
##########

html.human_capital <- read_html("http://reports.weforum.org/human-capital-report-2016/rankings/")
df.human_capital <- html.human_capital %>%
  html_node("table") %>%
  html_table()

# inspect
head(df.human_capital)

# rename variables
df.human_capital <- df.human_capital %>%
  rename(rank = `Overall Rank`,
         country = Economy,
         human_capital_score = `Overall Score`)

# inspect
head(df.human_capital)

###############
# GET WORLD MAP
###############

map.world <- map_data("world")

# make sure country names match in Human Capital DF and in the world map
human_capital_countries <- df.human_capital %>%
  select(country)
# inspect
human_capital_countries


# get world map countries
world_map_countries <-  as.data.frame(unique(map.world$region))
str(world_map_countries)
world_map_countries



# check match
df.match_check <- left_join(human_capital_countries, world_map_countries, by = )

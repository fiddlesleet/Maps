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
world_map_countries <- map.world %>%
  select(region) %>%
  distinct()
# inspect
world_map_countries

# check match
df.match_check <- left_join(human_capital_countries, world_map_countries, 
                            by = c('country' = 'region'))
# inspect
df.match_check

# get unmatched countries
unmatched <- df.match_check %>%
  filter(is.na(match))
# inspect
unmatched

# recode unmatched countries
df.human_capital$country <- recode(df.human_capital$country,
                                   "United Kingdom" = "UK",
                                   "United States" = "USA",
                                   "Russian Federation" = "Russia",
                                   "Korea, Rep." = "South Korea",
                                   "Kyrgyz Republic" = "Kyrgyzstan",
                                   "Slovak Republic" = "Slovakia",
                                   "Macedonia, FYR" = "Macedonia",
                                   "Iran, Islamic Rep." = "Iran",
                                   "Trinidad and Tobago" = "Trinidad",
                                   "Lao PDR" = "Laos",
                                   "CÙte d’Ivoire" = "Ivory Coast")

# join human capital data to world map
map.world <- left_join(map.world, df.human_capital, by = c('region' = 'country'))
# inspect
str(map.world)
head(map.world)

##########
# PLOT MAP
##########

# rough draft 
gg <- ggplot(data = map.world,
       aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = human_capital_score))

# plot final map
gg + 
  scale_fill_viridis(option = "inferno", name = "Human Capital\nIndex") +
  labs(title = "Human Capital Index, by country",
       subtitle = "Source: World Economic Forum, 2016") +
  theme(panel.background = element_rect(fill = "#3E3E3E"),
        plot.background = element_rect(fill = "#3E3E3E"),
        legend.background = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        text = element_text(family = "American Typewriter", color = "#DDDDDD"),
        plot.title = element_text(size = 32),
        legend.position = c(.18, .375))

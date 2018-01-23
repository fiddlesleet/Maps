# recreating the visualization at: https://www.cbsnews.com/news/amazon-hq2-cities-location-choices-new-second-headquarters/

#==============
# LOAD PACKAGES
#==============

library(rvest)
library(tidyverse)
library(stringr)
library(ggmap)

#=======
# SCRAPE
#=======

html.amzn_cities <- read_html("https://www.cbsnews.com/news/amazon-hq2-cities-location-choices-new-second-headquarters/")

df.amzn_cities <- html.amzn_cities %>%
  html_nodes("table") %>%
  .[[1]] %>%
  html_table()

# inspect
head(df.amzn_cities)

#====================
# CHANGE COLUMN NAMES
#====================

# inspect col names
names(df.amzn_cities)

# assign new col names
names(df.amzn_cities) <- c("metro_area", "state", "population", "bachelors_degree_pct")

# inspect
head(df.amzn_cities)

# delete unneeded first row
df.amzn_cities <- df.amzn_cities[-1, ]

# inspect
head(df.amzn_cities)

#====================
# CHANGE COLUMN TYPES
#====================

str(df.amzn_cities)

# coerce population from chr to int by parsing as int
df.amzn_cities <- mutate(df.amzn_cities, population = parse_number(population))
# inspect
head(df.amzn_cities)
typeof(df.amzn_cities$population)

# coerce bachelors_degree_pct to double
df.amzn_cities <- mutate(df.amzn_cities, bachelors_degree_pct = parse_number(bachelors_degree_pct))
# inspect
head(df.amzn_cities)
typeof(df.amzn_cities$bachelors_degree_pct)

#====================
# EXTRACT CITY NAMES
#====================

# Extract first city name from metro area, e.g. Los Angeles-Long Beach-Anaheim ==> Los Angeles
# -get the first city before a hyphen; add city column
df.amzn_cities <- df.amzn_cities %>% 
  mutate(city = str_extract(metro_area, "^[^-]*")) # get the first city before a hyphen
# inspect
head(df.amzn_cities)

#========
# GEOCODE
#========

geocodes <- geocode(df.amzn_cities$city)
# inspect
head(geocodes)

# merge geocodes into amazn df
df.amzn_cities <- cbind(df.amzn_cities, geocodes)
# inspect
head(df.amzn_cities)

#====================
# REORDER COL NAMES
#====================

df.amzn_cities <- select(df.amzn_cities, city, state, metro_area, population, bachelors_degree_pct, lon, lat)
# inspect
head(df.amzn_cities)

# rename lon to long
df.amzn_cities <- rename(df.amzn_cities, long = lon)
# inspect
head(df.amzn_cities)

#============
# GET USA MAP 
#============

map.states <- map_data("state")

# plot first iteration
ggplot() + 
  geom_polygon(data = map.states, aes(x = long, y = lat, group = group)) + # plots map of USA state outlines
  geom_point(data = df.amzn_cities, aes(x = long, y = lat, 
                                        size = population, 
                                        color = bachelors_degree_pct))
# plot final version
ggplot() + 
  geom_polygon(data = map.states, aes(x = long, y = lat, group = group)) + # plots map of USA state outlines
  geom_point(data = df.amzn_cities, aes(x = long, y = lat, 
                                        size = population, 
                                        color = bachelors_degree_pct * .01),
                                        alpha = .5) + 
  geom_point(data = df.amzn_cities, aes(x = long, y = lat, 
                                        size = population, 
                                        color = bachelors_degree_pct * .01),
                                        shape = 1) + 
  coord_map(projection = "albers", lat0 = 30, lat1 = 40, xlim = c(-121, -73), ylim = c(25, 51)) +
  scale_color_gradient2(low = "red", mid = "yellow", high = "green", midpoint = .41, 
                        labels = scales::percent_format()) +
  scale_size_continuous(range = c(.9, 11), breaks = c(2000000, 10000000, 20000000),
                        labels = scales::comma_format()) +
  guides(color = guide_legend(reverse = T,
                              override.aes = list(alpha = 1, size = 4))) +
  labs(color = "Bachelor's Degree \nPercent", 
       size = "Total Population \n(metro area)",
       title = "Possible cities for new Amazon Headquarters",
       subtitle = "Based on population % percent of people with college degrees") +
  theme(text = element_text(color = "#464646", family = "American Typewriter"),
        panel.background = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 11),
        legend.key = element_rect(fill = "white")
        )
 
  
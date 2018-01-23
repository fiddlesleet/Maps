#==============
# LOAD PACKAGES
#==============

library(rvest)
library(tidyverse)
library(ggmap)
library(stringr)

#==========================
# SCRAPE DATA ROM WIKIPEDIA
#==========================
html.population <- read_html('https://en.wikipedia.org/wiki/List_of_Asian_cities_by_population_within_city_limits')
df.asian_cities <- html.population %>%
  html_nodes("table") %>%
  .[[3]] %>%
  html_table(fill = TRUE)

# inspect
head(df.asian_cities)
names(df.asian_cities)
str(df.asian_cities)

#============================
# REMOVE EXTRANEOUS VARIABLES
#============================
df.asian_cities <- df.asian_cities[,-c(3,5,6)] 
names(df.asian_cities)
str(df.asian_cities)

# rename columns
colnames(df.asian_cities) <- c("City", "Country", "Population")
head(df.asian_cities)

# remove extra row at top
df.asian_cities <- df.asian_cities[-1,]
head(df.asian_cities)


# remove bracketed footnote markers from population markers (e.g. 23,500,000[4])
df.asian_cities <- df.asian_cities %>% 
  mutate(Population = str_replace_all(Population, "\\(.*\\)","") %>%
           parse_number())
# inspect
head(df.asian_cities)

#=============================================
# create "City, Country" column for geocoding
#=============================================
df.asian_cities <- df.asian_cities %>%
  mutate(Full_City_Name = str_c(df.asian_cities$City,
                                df.asian_cities$Country,
                                sep = ", "))
# inspect
head(df.asian_cities)

#=====================================================
# REORDER VARIABLES
# - with select(), simply list them in desired col order
#=====================================================

df.asian_cities <-df.asian_cities %>%
  select(City, Country, Full_City_Name, Population)
# inspect
head(df.asian_cities)


#========================================
# COERCE TO TIBBLE
# - just to make the data print better
#========================================

df.asian_cities <- df.asian_cities %>% as_tibble()

#===============================================
# GEOCODE
# - get longitude, latitude via ggmap::geocode()
#================================================

geocodes <- geocode(df.asian_cities$Full_City_Name)
print(geocodes)
class(geocodes)

# join geocodes to the df
df.asian_cities<- cbind(df.asian_cities, geocodes)
# inspect
head(df.asian_cities)

#=============
# GET ASIA MAP
#=============
map.asia <- get_map('Asia', zoom = 3, source = "stamen", maptype = "watercolor")

# map it ...
map.asia %>% ggmap()

#========================================
# PLOT CITIES ON MAP
#========================================
# just the points
ggmap(map.asia) +
  geom_point(data = df.asian_cities, 
             aes(x = lon, y = lat, size = Population), 
             color = "red", alpha = .3) +
  geom_point(data = df.asian_cities, 
             aes(x = lon, y = lat, size = Population),
             color = "red", shape = 1) 

# final map
ggmap(map.asia) +
  geom_point(data = df.asian_cities, 
             aes(x = lon, y = lat, size = Population), 
             color = "red", alpha = .3) +
  geom_point(data = df.asian_cities, 
             aes(x = lon, y = lat, size = Population),
             color = "red", shape = 1) +
  labs(x = NULL, y = NULL) +
  labs(size = 'Population (millions)') +
  labs(title = "Largest Cities in Asia", subtitle = "source: https://en.wikipedia.org/wiki/List_of_Asian_cities_by_population_within_city_limits") +
  scale_size_continuous(range = c(.6,18), labels = scales::comma_format(), breaks =  c(1500000, 10000000, 20000000)) +
  theme(text = element_text(color = "#464646", family = "American Typewriter")) +
  theme(axis.text = element_blank()) + # remove axis tick marks 
  theme(axis.ticks = element_blank()) +
  theme(plot.title = element_text(size = 32)) +
  theme(plot.subtitle = element_text(size = 10)) +
  theme(legend.key = element_rect(fill = "white")) # white background for key


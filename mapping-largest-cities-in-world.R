#==============
# LOAD PACKAGES
#==============

library(rvest)
library(tidyverse)
library(ggmap)
library(stringr)
library(RColorBrewer)


#==========================
# SCRAPE DATA ROM WIKIPEDIA
#==========================
html.population <- read_html('https://en.wikipedia.org/wiki/List_of_cities_proper_by_population')
df.cities <- html.population %>%
  html_nodes("table") %>%
  .[[4]] %>%
  html_table(fill = TRUE)

# inspect
head(df.cities)
names(df.cities)
str(df.cities)
summary(df.cities)

#============================
# REMOVE EXTRANEOUS VARIABLES
#============================
df.cities <- df.cities[,c(2,4,7,8)] 
names(df.cities)
str(df.cities)

# rename columns
colnames(df.cities) <- c("City", "Population", "Pop.Density", "Country")
head(df.cities)

# remove crud prefixes from population column
df.cities <- df.cities %>% 
  mutate(Population = str_sub(Population, 21)) 

# inspect
head(df.cities)

# remove bracketed footnote markers from population markers (e.g. 23,500,000[4])
df.cities <- df.cities %>% 
  mutate(Population = str_replace_all(Population, "\\(.*\\)","") %>%
         parse_number())

# inspect
head(df.cities)

# remove commas from population density
df.cities$Pop.Density <- as.numeric(gsub(",","",df.cities$Pop.Density))
# inspect
head(df.cities)


#=============================================
# create "City, Country" column for geocoding
#=============================================
df.cities <- df.cities %>%
  mutate(Full_City_Name = str_c(df.cities$City,
                                df.cities$Country,
                                sep = ", "))
# inspect
head(df.cities)

#=====================================================
# REORDER VARIABLES
# - with select(), simply list them in desired col order
#=====================================================

df.cities <- df.cities %>%
  select(City, Country, Full_City_Name, Population, Pop.Density)
# inspect
head(df.cities)
str(df.cities)

#========================================
# COERCE TO TIBBLE
# - just to make the data print better
#========================================

df.cities <- df.cities %>% as_tibble()

#===============================================
# GEOCODE
# - get longitude, latitude via ggmap::geocode()
#================================================

geocodes <- geocode(df.cities$Full_City_Name)
print(geocodes)
class(geocodes)
summary(geocodes) # 8 NAs

# join geocodes to the df
df.cities <- cbind(df.cities, geocodes)
# inspect
head(df.cities)

#=============
# GET WORLD MAP
#=============

map.world <- map_data('world')

#========================================
# PLOT CITIES ON MAP
#========================================
# just the points

ggplot() + 
  geom_polygon(data = map.world, aes(x = long, y = lat, group = group)) +
  geom_point(data = df.cities, 
             aes(x = lon, y = lat, size = Population), 
             color = "red", alpha = .3) +
  geom_point(data = df.cities, 
             aes(x = lon, y = lat, size = Population),
             color = "red", shape = 1) 

# final map
ggplot() + 
  geom_polygon(data = map.world, aes(x = long, y = lat, group = group)) +
  geom_point(data = df.cities, 
             aes(x = lon, y = lat, size = Population), 
             color = "#ff0066", alpha = .2) +
  geom_point(data = df.cities, 
             aes(x = lon, y = lat, size = Population),
             color = "#ff0066", shape = 1) +
  labs(x = NULL, y = NULL) +
  labs(size = 'Population (millions)') +
  labs(title = "World's Largest Cities", subtitle = "source: https://en.wikipedia.org/wiki/List_of_cities_proper_by_population") +
  scale_size_continuous(range = c(.6,18), labels = scales::comma_format(), breaks = c(4000000, 9000000,
                                                                                      14000000, 1900000, 
                                                                                      24000000)) +
  theme(text = element_text(color = "#464646", family = "American Typewriter")) +
  theme(axis.text = element_blank()) + # remove axis tick marks 
  theme(axis.ticks = element_blank()) +
  theme(plot.title = element_text(size = 32)) +
  theme(plot.subtitle = element_text(size = 10)) +
  theme(legend.key = element_rect(fill = "white"))  # white background for key
 
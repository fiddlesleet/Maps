library(tidyverse)
library(forcats)
library(scales)
library(openxlsx)
library(directlabels)
library(ggthemes)
library(RColorBrewer)
library(countrycode)
library(maps)
library(viridis)
library(leaflet)
library(rworldmap)
library(sf)
library(maptools)

#######################
# GET DATA IN TEMP FILE
#######################

# read data
tf <- tempfile()
download.file("https://www.sipri.org/sites/default/files/SIPRI-Milex-data-1949-2016.xlsx",
              destfile = tf, mode = "wb")
df <- tbl_df(read.xlsx(tf, sheet = "Share of GDP", startRow = 6))

# inspect
df

# reformat 
df <- df %>%
  gather(Year, Value, -Country, -Notes) %>%
  mutate(Value = as.numeric(Value), 
         Year = as.numeric(Year)) %>%
  filter(!is.na(Value))

# inspect
df

######
# PLOT
######

# get iso3c codes for each country using countrycodes package
df <- df %>%
  mutate(iso3c = countrycode(Country, "country.name", destination = "iso3c")) %>%
  mutate(iso3c = ifelse(Country == "Kosovo", "KOS", iso3c)) %>%
  mutate(iso3c = ifelse(Country == "Central African Rep.", "CAF", iso3c))

# get world map
map.world <- map_data("world") %>%
  mutate(iso3c = countrycode(region, "country.name", destination = "iso3c"))

# use per-country data from the latest year
data <- df %>%
  group_by(Country) %>%
  filter(Year == max(Year))

# merge datasets
plottable <- map.world %>%
  left_join(data, by = "iso3c")

# plot 
ggplot(plottable, aes(x = long, y = lat, group = group, fill = Value)) +
  geom_polygon() + 
  scale_fill_viridis("", label = percent, option = "magma", direction = -1) +
  theme_minimal(base_family = "American Typewriter") +
  labs(title = 'Military expenditure as a percentage of GDP',
       subtitle = 'Source: Stockholm International Peace Research Institute',
       x = NULL,
       y = NULL) +
    coord_map("orthographic", orientation = c(20, 20, 0)) # make rounded map 


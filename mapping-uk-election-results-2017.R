library(readr)
library(dplyr)
#devtools::install_github("tidyverse/ggplot2") # for geom_sf()
library(ggplot2)
library(parlitools)
library(sf)

###########
# GET DATA
###########
# csv at: https://www.r-bloggers.com/uk-2017-general-election-results-data/
df.results <- read_csv("EconomistUK2017.csv")
# inspect
df.results

#########
# GET MAP
#########
map.uk <- west_hex_map # Westminster parliamentary constituencies
# inspect
map.uk

#########
# COMBINE
#########

df.results <- inner_join(df.results, map.uk,
                         by = c("Constituency.ID" = "gss_code")) %>%
  filter(!is.na(win)) %>%
  st_as_sf()
# inspect
df.results

######
# PLOT
######

# get party colors
colors <- party_colour
# inspect
colors

# plot
ggplot(df.results) + 
  geom_sf(aes(fill = win), size = 0.2) +
  scale_fill_manual(values = c("#006BA4", "#800B05", "#349B3A", "#888888", "#DB434E",
                               "#E8B335", "#98B3D1", "#60B031", "#8DDC64","#FCDD02")) +
  labs(fill = "Party",
       title = "UK General Election Results 2017") + 
  theme_minimal()

# save last plot 
ggsave("mapping-uk-election-results-2017.png", width = 4.61, height = 7.5)


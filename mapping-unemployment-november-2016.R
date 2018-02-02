library(ggplot2)
library(viridis)

###########
# LOAD DATA
###########

url.unemployment_map <- url("http://sharpsightlabs.com/wp-content/datasets/unemployment_map_data_2016_nov.RData")
load(url.unemployment_map)

##########
# PLOT MAP
##########

ggplot() +
  geom_polygon(data = map.county_unemp,
               aes(x = long, y = lat, group = group, fill = unemployed_rate)) +
  geom_polygon(data = map.states, aes(x = long, y = lat, group = group), 
               color = "#EEEEEE", fill = NA, size = .3) +
  coord_map("albers", lat0 = 30, lat1 = 40) +
  labs(title = "United States unemployment rate, by county",
       subtitle = "November 2016",
       fill = "% unemployed") +
  scale_fill_viridis() + 
  theme(text = element_text(family = "American Typewriter", color = "#464646"),
        plot.title = element_text(size = 30),
        plot.subtitle = element_text(size = 20),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = c(.9, .4),
        legend.title = element_text(size = 16),
        legend.background = element_blank(),
        panel.background = element_blank())

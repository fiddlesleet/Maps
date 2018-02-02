library(ggalt)
library(ggplot2)
library(maps)
library(dplyr)
library(readr)

##########
# GET DATA
##########

df.vc <- read_csv("http://sharpsightlabs.com/wp-content/uploads/2016/09/vc_investment_totals.csv")
# inspect
df.vc

#########
# GET MAP
#########

map.world <- map_data("world")
str(map.world)

#############
# PLOT CITIES
#############

ggplot() + 
  geom_polygon(data = map.world, 
               aes(x = long, y = lat, group = group),
               fill = "#002035",
               color = "#114151",
               size = .25) + 
  geom_point(data = df.vc,
             aes(x = longitude, y = latitude, size = vc_investment_millions),
             color = "red", 
             alpha = .15) + 
  geom_point(data = df.vc,
             aes(x = longitude, y = latitude, size = vc_investment_millions),
             color = "red", 
             alpha = .7,
             shape = 1) +
  scale_size_continuous(range = c(1,20), 
                        breaks = c(500,2000,6000), 
                        name="Venture Capital Investment\n(USD, Millions)\n") + 
  theme(text = element_text(family = "American Typewriter")) +
  theme(panel.background = element_rect(fill = "#000727")) + 
  theme(panel.grid = element_blank()) +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.title = element_blank()) +
  theme(legend.position = c(.17,.3)) +
  theme(legend.background = element_blank()) + 
  theme(legend.key = element_blank())  +
  theme(legend.title = element_text(color = "#DDDDDD", size = 16)) +
  theme(legend.text = element_text(color = "#DDDDDD", size = 16)) +
  coord_proj("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")  # use robinson projection
  
###########
# BAR CHART
###########

df.vc %>% 
  ggplot(aes(x = reorder(metro, vc_investment_millions), y = vc_investment_millions)) + 
  geom_bar(stat = "identity", fill = "#000727") +
  geom_text(aes(label = vc_investment_millions), hjust = 1.1, color = "#ffffff") + 
  labs(y = "Millions of Dollars", 
       title = "Global Venture Capital Investment by City") + 
  coord_flip() + 
  theme(text = element_text(family = "American Typewriter")) + 
  theme(plot.title = element_text(size = 28, color = "#646464")) + 
  theme(axis.title.y = element_blank()) + 
  theme(panel.background = element_rect(fill = "#dddddd")) +
  theme(panel.grid.major = element_blank())
                                  

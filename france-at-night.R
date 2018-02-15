# using population density to map "France at night"
# uses: density = population / commune area data, across ~36,000 French commune tracts
# requires: micro-area, population vectors

devtools::install_github("hadley/ggplot2")
library(sf)
library(dplyr)
library(stringr)
library(scales)
library(ggplot2)

##########
# GET DATA
##########

url <- url("http://sharpsightlabs.com/wp-content/datasets/france_population_data_2016.RData")
load(url)

# inspect
str(df.france)
df.france <- df.france %>%
  as_tibble()
df.france

############
# CLEAN DATA
############

colnames(df.france)
# set colnames to lowercase
colnames(df.france) <- colnames(df.france) %>%
  str_to_lower()
# inspect
colnames(df.france)

# get population stats
df.france$population %>%
  summary()
# Create density variable
df.france <- df.france %>% 
  mutate(density = population / superficie * 100)
# inspect
colnames(df.france)

##########
# PLOT MAP
##########

## Set color scheme
# get color breaks by finding ventiles 
quantile(df.france$density, seq(from = 0, to = 1, by = .05))
# percentiles: 90-100
quantile(df.france$density, seq(from = .9, to = 1, by = .01))
# median
median(df.france$density)

# Plot 
light_grey <- "#E1E1E1"
deep_purple <- "#000223"
orange_creamsicle <- "#f9cf86"
light_yellow <- "#fceccf"
# deep purple, light orangesicle, light yellow
colors <- c(deep_purple, orange_creamsicle, light_yellow, "white")
values <- c(0, 500, 3000, 40000)
df.france %>%
  ggplot() + 
  geom_sf(aes(fill = density, color = density)) + 
  scale_fill_gradientn(colors = colors,
                       values = rescale(values),
                       breaks = c(0, 500, 1500),
                       guide = FALSE) + 
  scale_color_gradientn(colors = colors,
                        values = rescale(values),
                        guide = FALSE) + 
  labs(title = "Population density in France,\nvisualized by nightime satellite images",
       subtitle = "(by people/sq km)") + 
  theme(text = element_text(family = "Roboto",
                            color = light_grey),
        plot.title = element_text(size = 12, color = light_grey),
        plot.subtitle = element_text(size = 9),
        plot.background = element_rect(fill = deep_purple),
        panel.background = element_rect(fill = deep_purple),
        panel.grid.major = element_line(color = deep_purple),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.background = element_blank())

ggsave("france-at-night.png")
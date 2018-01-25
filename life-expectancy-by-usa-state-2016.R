# generates 6 maps of Overall, White, Black, Latino, Asian & Native American life 
# expectancies by state in 2016 using a single mapply at end of script;
# explores statistical relationship between the life expectancies 

#==============
# LOAD PACKAGES
#==============

library(tidyverse)
library(sf)
library(rvest)
library(stringr)
library(scales)
library(viridis)

#============
# SCRAPE DATA
#============

df.le <- read_html("https://en.wikipedia.org/wiki/List_of_U.S._states_and_territories_by_life_expectancy") %>%
  html_nodes("table") %>%
  .[[2]] %>%
  html_table(fill = T)

# inspect
glimpse(df.le)
str(df.le)

#============
# CLEAN DATA
#============

# delete superfluous first 3 rows
df.le <- df.le[-c(1:3),]
head(df.le)
str(df.le)

# delete rank columns
df.le <- df.le[, -c(1:2)]
head(df.le)
str(df.le)

# only keep cols that matter
df.le <- df.le[, c(1:7)]
head(df.le)
str(df.le)

# change col names
colnames(df.le) <- c("state", "le", "le_white", "le_black", "le_asian", "le_latino", "le_native")
head(df.le)
str(df.le)

# convert all cols except state name to numeric
df.le[, 2:7] <- sapply(df.le[, 2:7], as.numeric)
head(df.le)
str(df.le)

#============
# EXPLORE DATA
#============

cor.test(df.le$le_white, df.le$le_black) # 0.387
cor.test(df.le$le_white, df.le$le_latino) # -0.0664 
cor.test(df.le$le_white, df.le$le_asian) # 0.00836 
cor.test(df.le$le_white, df.le$le_native) # -0.296 
cor.test(df.le$le_white, df.le$le) # 0.755 # autocorrelation

cor.test(df.le$le_black, df.le$le_latino) # -0.184 

#========
# GET MAP
#========

map.states <- map_data('state')
str(map.states)

# transform all state names in df.le to lowercase to match statenames in map.states
df.le$region <- tolower(df.le$state)
head(df.le)
str(df.le)

# merge the datasets
states <- merge(map.states, df.le, by="region", all.x=T)
head(states)
str(states)

#=====
# PLOT
#=====

# plot multiple maps by race
le_by_race <- function(race, title) {
  ggplot(data = states, aes(x = long, y = lat, group = group, fill = race)) +
    geom_polygon(color = "white") +
    scale_fill_gradient(name = "years", 
                        low = "#cefaf2", 
                        high = "#095b4b", 
                        guide = "colorbar", 
                        na.value="#eeeeee") +
    labs(x = NULL, y = NULL) +
    labs(title = paste(title, "Life Expectancy, 2016", " "), subtitle = " source: https://en.wikipedia.org/wiki/\n\tList_of_U.S._states_and_territories_by_life_expectancy") +
    theme(panel.background = element_rect(fill = 'white')) + # white background for map
    theme(text = element_text(color = "#464646", family = "American Typewriter")) +
    theme(plot.background = element_blank()) + 
    theme(axis.text = element_blank()) + # remove axis tick marks 
    theme(axis.ticks = element_blank()) +
    theme(plot.title = element_text(size = 16)) +
    theme(plot.subtitle = element_text(size = 10))  # white background for key
}

# "" will represent the overall graph 
titles <- c("", "White", "Black", "Latino", "Asian", "Native American")
head(states)

# extract life expectancy columns
s <- states[, 8:13]

mapply(le_by_race, s, titles, SIMPLIFY = FALSE)

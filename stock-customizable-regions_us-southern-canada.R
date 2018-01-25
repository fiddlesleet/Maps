# NOTE: "A choropleth is any map that shows borders (such as states), 
# and expresses values for the regions with color"

# install and load the packages
library(choroplethr)
library(choroplethrAdmin1)
library(acs)
library(gridExtra)
library(microbenchmark)

install.packages('choroplethrAdmin1')
### read in data 

# map of continental us + southern canada
data("continental_us_states")
lower_canada <- c("british columbia", "alberta", "saskatchewan", "manitoba", 
                 "ontario", "quebec")
regions <- c(lower_canada, continental_us_states)
df <- data.frame(region=regions, value=sample(1:length(regions)))
map1 <- admin1_region_choropleth(df, title = "The U.S. & Southern Canada")
map2 <- admin1_region_choropleth(df, num_colors = 7, reference_map = TRUE)
grid.arrange(map1, map2, nrow = 1)


## state population data
# choroplethr data
data(df_pop_state)
tail(df_pop_state)
data(df_county_demographics)


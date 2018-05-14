library(choroplethr)
library(mapproj)
library(dplyr)
library(ggplot2)

data("df_county_demographics")
str(df_county_demographics)
head(df_county_demographics)

# plot percent hispanic in texas
df_county_demographics$value = df_county_demographics$percent_hispanic
county_choropleth(df_county_demographics, 
                  state_zoom = 'texas',
                  title = "Texas County Percent Hispanic\n2012 Estimates",
                  num_colors = 9) +
    coord_map() # preserves straight lines 

# plot median income in CA
df_county_demographics$value = df_county_demographics$per_capita_income
county_choropleth(df_county_demographics, 
                  state_zoom = 'california',
                  title = "CA County Per Capita Income\n2012 Estimates",
                  num_colors = 9) +
    coord_map() # preserves straight lines 

# plot median income in selected states
df_county_demographics$value = df_county_demographics$per_capita_income
county_choropleth(df_county_demographics, 
                  state_zoom = c('california', 'washington', 
                                 'pennsylvania', 'montana'),
                  title = "CA County Per Capita Income\n2012 Estimates",
                  num_colors = 9) +
    coord_map() # preserves straight lines 

# plot median income in west
df_county_demographics$value = df_county_demographics$per_capita_income
county_choropleth(df_county_demographics, 
                  state_zoom = c('california', 'washington', 'oregon'),
                  title = "CA County Per Capita Income\n2012 Estimates",
                  num_colors = 9) +
    coord_map() # preserves straight lines 

# plot median income across USA
df_county_demographics$value = df_county_demographics$per_capita_income
county_choropleth(df_county_demographics, 
                  title = "USA by County: Per Capita Income\n2012 Estimates",
                  num_colors = 9) + # continuous scale to show outliers
    coord_map() # preserves straight lines 


# plot poorest quintile median income across USA # mine
poorest_quintile <- df_county_demographics %>%
    filter(per_capita_income <= 19291)
poorest_quintile$value = poorest_quintile$per_capita_income
# plot
county_choropleth(poorest_quintile, 
                  title = "USA by County: BOTTOM 20% Per Capita Income\n2012 Estimates",
                  num_colors = 1)  
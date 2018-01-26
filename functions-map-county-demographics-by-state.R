##############################################
## WORKFLOW
# 1. LOAD DATA
# 2. PLOT MAP OF INDIVIDUAL STATE
# 2.5 SIDE-BY-SIDE COMPARISON OF 2 STATE MAPS
# 3. PLOT MAP OF NEIGHBORING STATES
# 4. PLOT MAPS OF MULTIPLE STATES AT ONCE
#  - best for states that do not share borders
#  - all maps within same demographic category
##############################################

library(choroplethr)
library(mapproj)
library(dplyr)
library(Hmisc) # for capitalization
library(microbenchmark)

##############
# 1. LOAD DATA
##############

data("df_county_demographics")
# inspect
str(df_county_demographics)
head(df_county_demographics)

###################################
# 2. PLOT MAPS OF INDIVIDUAL STATES
###################################

# generate demographic map
map_state_by_county <- function(v, state, title) {
  df_county_demographics$value <- v
  county_choropleth(df_county_demographics, 
                      state_zoom = state,
                      title = title,
                      num_colors = 5) +
      coord_map()
}

# process user input before calling map_states_by_county()
plot <- function (v, categoryName, states, noTitle = FALSE) {
  
  # format state vector for processing in county_choropleth() in map_state_by_country()
  states <- tolower(states) 
  
  # format title
  if (noTitle) {
    map_state_by_county(v, states, "")
  } else if (length(states) == 1) {
    # set title
    title <- paste0(categoryName, " by ", capitalize(states), " County,", "\n2012 Estimates")
    map_state_by_county(v, states, title)
  } else {
    title <- paste0(categoryName, " by County,", "\n2012 Estimates")
    map_state_by_county(v, states, title)
  }
}

# plot Texas Hispanic population
plot(v = df_county_demographics$percent_hispanic, 
     category = "Percent Hispanic",
     state = "Texas")

# plot per capita income in California 
plot(v = df_county_demographics$per_capita_income, 
     category = "Per Capita Income",
     state = "California")

# plot median rent in New York
plot(v = df_county_demographics$median_rent, 
     category = "Median Rent",
     state = "New York")

#######################################################
# 2.5 SIDE-BY-SIDE COMPARISON OF 2 STATE MAPS
# - use map_state_by_county() and plot() from Section 2
#######################################################

# percent hispanic, california v. texas
ca <- plot(v = df_county_demographics$percent_hispanic, 
           category = "Percent Hispanic",
           state = "California", noTitle = TRUE)
tx <- plot(v = df_county_demographics$percent_hispanic, 
           category = "Percent Hispanic",
           state = "Texas",
           noTitle = TRUE)
double_map(ca, tx, title = "Hispanic Population by County,\n    California vs. Texas")

########################################################
# 3. PLOT MAP OF NEIGHBORING STATES
# - use map_state_by_county() and plot() from Section 2
########################################################

# plot percent_hispanic in west
demographic_vector <- df_county_demographics$percent_hispanic
category_name <- "Percent Hispanic Population"
western_states <- c('california', 'washington', 'oregon')

plot(v = demographic_vector, 
     category = category_name,
     state = western_states)

##################################################
# 4. PLOT SEPARATE MAPS OF MULTIPLE STATES AT ONCE
# - best for states that do not share borders
##################################################

map_nstates_by_county <- function(state, title) {
  county_choropleth(df_county_demographics, 
                    state_zoom = state,
                    title = title, 
                    num_colors = 5) +
    coord_map()
}

plot_multiple_maps <- function(categoryName, states) {  
# process states for county_choropleth() function
  states <- tolower(states)
  
  # create vector of titles 
  states_sentencecase <- capitalize(states)
  titles <- paste(states_sentencecase, "County", categoryName, "\n2012 Estimates", " ")
  class(titles)
  length(titles)
  mapply(map_nstates_by_county, states, titles, SIMPLIFY = FALSE)
}

# process user input before calling map_nstates_by_county()

# -----------------------------------------------------------
# FIELDS USER MUST UPDATE
# 1. @df_county_demographics$value: desired demographic data
# 2. @categoryName:                 for plot title
# 3. @use_state_abbreviation:       whether or not to use state abbreviation in plot title
# 4. @states:                       list of states to analyze
# -----------------------------------------------------------

#  1. Set Desired demographic data: plot median income in selected states
df_county_demographics$value <- df_county_demographics$per_capita_income

# 2. Format Title 
# - The title will take the form "[State] County [category] "\n2012 Estimates" 
categoryName <- "Per Capita Income"

# 4. Select states to analyze
states <- c('california', 'washington', 'pennsylvania', 'montana', 'new york')

# generate plots
plot_multiple_maps(categoryName, states) 
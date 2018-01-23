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

df.oil <- read_html("https://en.wikipedia.org/wiki/List_of_countries_by_oil_production") %>%
  html_nodes("table") %>%
  .[[1]] %>%
  html_table()

# inspect
head(df.oil)

# change col names
colnames(df.oil) <- c("rank", "country", "bpd_2016")
# inspect
head(df.oil)
str(df.oil)

# coerce rank to int
df.oil <- df.oil %>% 
  mutate(rank = as.integer(rank))
# inspect
str(df.oil)

# coerce bpd into numeric
df.oil <- df.oil %>%
  mutate(bpd_2016 = bpd_2016 %>%
           str_replace_all(',', '') %>%
           as.integer())
# inspect
head(df.oil)

# create OPEC y/n variable
df.oil <- df.oil %>%
  mutate(is_opec = if_else(str_detect(country, 'OPEC'), 1, 0))
# inspect
head(df.oil)

# delete '(OPEC)' from country column
df.oil <- df.oil %>%
  mutate(country = country %>%
           str_replace(' \\(OPEC\\)', '') %>%
           str_replace('\\s{2,}', ' '))
# inspect
glimpse(df.oil)
# check all OPEC country names correct
df.oil %>% 
  filter(is_opec == 1) %>%
  select(country)

# reorder cols
df.oil <- df.oil %>%
  select(rank, country, is_opec, bpd_2016)
# inspect
head(df.oil)

#========
# GET MAP
#========

map.world <- map_data('world')
head(map.world)

# check for join mismatches
anti_join(df.oil, map.world, by = c('country' = 'region'))

# recode country names
map.world %>%
  group_by(region) %>%
  summarize() %>%
  print(n = Inf)

df.oil <- df.oil %>%
  mutate(country = recode(country, 'United States' = 'USA',
                          'United Kingtom' = 'UK',
                          'Congo, Democratic Republic of the' = 'Democratic Republic of the Congo',
                          'Trinidad and Tobago' = 'Trinidad',
                          'Sudan and South Sudan' = 'Sudan',
                          'Sudan and South Sudan' = 'South Sudan',
                          'Congo, Republic of the' = 'Republic of Congo'))

# join data sets
map.oil <- left_join(map.world, df.oil, by = c('region' = 'country'))

#=====
# PLOT
#=====

# first draft
ggplot(map.oil, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = bpd_2016))

# get stats
upper_mean_bpd <- df.oil %>% 
  filter(bpd_2016 > 822675) %>%
  summarize(mean(bpd_2016))
upper_mean_bpd

lower_mean_bpd <- df.oil %>% 
  filter(bpd_2016 < 822675) %>%
  summarize(mean(bpd_2016))
lower_mean_bpd

# final draft

breaks <- c(1000, 30000, 100000, 300000, 800000, 1500000, 3000000, 8000000)
ggplot(map.oil, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = log(bpd_2016))) +  
  scale_fill_viridis(option="viridis") + 
  guides(fill = guide_legend(reverse = T)) +
  labs(fill = 'log(bpd)', 
       title = 'Oil Production by Country',
       subtitle = 'Barrels per day (bpd), 2016',
       x = NULL,
       y = NULL) +
  theme(text = element_text(family = 'American Typewriter', color = '#999999'),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 14),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = '#333333'),
        plot.background = element_rect(fill = '#333333'),
        legend.position = c(.18,.36),
        legend.background = element_blank(),
        legend.key = element_blank()
  ) +
  annotate(geom = 'text',
           label = 'Source: U.S. Energy Information Administration\nhttps://en.wikipedia.org/wiki/List_of_countries_by_oil_production',
           x = 18, 
           y = -55,
           size = 2,
           family = 'Gill Sans',
           color = '#CCCCCC',
           hjust = 'left'
  )
  


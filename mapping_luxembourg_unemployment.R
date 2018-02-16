library(rvest)
library(dplyr)
library(tidyr)
library(ggthemes)
library(ggplot2)
library(viridis)
library(gganimate)
library(maps)
library(rgdal)
library(broom)
library(hrbrthemes)

##########
# GET DATA
##########

url <- "http://www.statistiques.public.lu/stat/TableViewer/tableViewHTML.aspx?ReportId=12950&IF_Language=eng&MainTheme=2&FldrName=3&RFPath=91"
page <- read_html(url)
# get table
df.unemployment <- page %>%
  html_node(".b2020-datatable") %>%
  html_table(fill = TRUE)
# inspect
df.unemployment

############
# CLEAN DATA
############

# use first row as colnames
colnames(df.unemployment) <- df.unemployment[1,]
# inspect
df.unemployment

# remove first 2 rows
df.unemployment <- df.unemployment[-c(1:2),]
# inspect
df.unemployment

# rename first two columns
colnames(df.unemployment)[1:2] <- c("division", "var")
# inspect
df.unemployment
str(df.unemployment)

# convert character columns to numeric
df.unemployment[, 3:18] <- sapply(df.unemployment[, 3:18], as.numeric)
# inspect
str(df.unemployment)

# Gather all columns except division & var
df.unemployment <- df.unemployment %>%  
  gather(key=year, value, -division, -var) %>%
  spread(var, value) %>% 
  tbl_df()
# inspect
df.unemployment

#  filter out canton data and remove "Grand Duchy of Luxembourg" rows 
df.unemployment <- df.unemployment %>%
  filter(!grepl("Canton", division), 
         division != "Grand Duchy of Luxembourg") 

# retain just the data I'll use
df.unemployment <- df.unemployment %>%
  select(commune = division, year, unemp_rate = `Unemployment rate (in %)`)
# inspect
df.unemployment

##############
# GET MAP DATA
##############

setwd('~/Downloads')
# get Luxembourg Shapefile at https://data.public.lu/en/datasets/limites-administratives-du-grand-duche-de-luxembourg/
sf.luxembourg <- readOGR("Limadmin_SHP/LIMADM_COMMUNES.shp")
# inspect
str(sf.luxembourg)
# convert to df
df.communes <- tidy(sf.luxembourg, region = "COMMUNE")
# inspect
df.communes

##############################
# JOIN MAP & UNEMPLOYMENT DATA
##############################
# change  “Haute-Sûre” to “Haute Sûre” in commune column 
df.unemployment$commune <- gsub("Haute-Sûre", "Haute Sûre", df.unemployment$commune)

# change “Redange” to “Redange-sur-Attert” in commune id column in shapefile
df.communes$id <- gsub("Redange", "Redange-sur-Attert", df.communes$id)

# join
df.unemployment <- left_join(df.communes, df.unemployment, by = c("id" = "commune"))
# convert to tibble
df.unemployment <- df.unemployment %>%
  tbl_df()  %>%
  filter(!is.na(year))
# inspect
df.unemployment

##########
# PLOT MAP
##########

# Unemployment in 2016
p <- df.unemployment %>%
  filter(year == 2016) %>%
  ggplot() + 
  geom_polygon(aes(x = long, y = lat, group = id, fill = unemp_rate))  +
  labs(title = "Unemployment rate in Luxembourg in 2016",
       y = NULL, x = NULL, fill = "Unemployment rate") +
  coord_fixed((10/13)) + 
  scale_fill_viridis(option = "inferno") +
  theme_ipsum_rc() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank()) 
ggsave(p, 'mapping_luxembourg_unemployment_2016.png')

# animate instead of faceting
p <- df.unemployment %>%
  ggplot() + 
  geom_polygon(data = df.unemployment,
               aes(x = long, y = lat, group = id, fill = unemp_rate,
                   frame = year)) +
  coord_fixed((10/13)) + 
  scale_fill_viridis(option = "inferno") +
  theme_ipsum_rc() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "#E5E9F0")) 

gganimate(p, "mapping_luxembourg_unemployment_by_year.gif", interval = 2)


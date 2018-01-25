library(tidyverse)
library(rvest)
library(countrycode)

data("countrycode_data")
countrycode_data <- select(countrycode_data, region = country.name.en, iso3c)

html.homicides <- read_html("https://en.wikipedia.org/wiki/List_of_countries_by_intentional_homicide_rate") 
df.homicides <- tbl_df(html.homicides %>%
  html_nodes("table") %>%
  .[[8]] %>%
  html_table(fill = TRUE)

# inspect 
df.homicides

page <- read_html("https://en.wikipedia.org/wiki/List_of_countries_by_intentional_homicide_rate") %>% 
  html_nodes(css = "#mw-content-text > div > table:nth-child(23)") %>% 
  html_table(fill = T) %>% 
  .[[1]] %>% 
  select(1:3) %>% 
  magrittr::set_colnames(c("region", "rate", "count")) %>% 
  slice(225:443) %>% 
  mutate(rate = as.numeric(rate),
         count = as.numeric(gsub(",", "", count)),
         region = ifelse(region == "United States", "United States of America", region))


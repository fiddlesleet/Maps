library(data.table)
library(gdata)
library(ggmap)

# download data here: http://www.napawineproject.com/project-notes/
filepath <- '[FILL THIS IN]'
vineyards <- read.xls(filepath, pattern = "Winery")

# inspect
str(vineyards)

# remove unnecessary columns
vineyards <- vineyards[, 1:8]
# inspect
str(vineyards)

# remove unnecessary rows
vineyards <- vineyards[1:1160, ]
str(vineyards)

# only need name, address, city, apps 
vineyards <- vineyards[, c(1:3,7)]
str(vineyards)

# remove private vineyards, which don't offer tastings, from list
vineyards <- vineyards %>%
  filter(App != "Private")
str(vineyards)

# remove vineyards that don't have addresses
vineyards <- vineyards %>%
  filter(Address != "")
str(vineyards)

# remove vineyards whose listed address is a PO box
vineyards <- vineyards[!grepl("P.O. Box", vineyards$Address),]


# create character vector of addresses
addresses <- with(vineyards, paste(Address, City, "CA", sep = ", "))
str(addresses)

# correct erroneous chr symbol
addresses[60] <- "5330 Silverado Trail, Napa, CA"
# confirm error has been fixed
a <- tbl_df(addresses) 
a

# geocode addresses
geocodes <- geocode(addresses)
str(geocodes)

# merge dataframes 
geocoded_vineyards <- cbind(geocodes, vineyards$App)
colnames(geocoded_vineyards) <- c("lon", "lat", "App")
geocoded_vineyards

# remove non yes/nos in Appointment column
geocoded_vineyards <- geocoded_vineyards %>%
  filter(App != "") %>%
  filter(App != "App") %>%
  filter(App != "N/Y") %>% 
  filter(App != "No/Yes")
geocoded_vineyards
str(geocoded_vineyards)

# get map of california
map.ca <- get_map(location = "Oakville, California", zoom = 11, maptype = "roadmap", source = "google")

# plot map 
basemap <- ggmap(map.ca)

basemap +
  geom_point(data = geocoded_vineyards,
           aes(x = lon, y = lat, color = App), alpha = .3) +
  geom_point(data = geocoded_vineyards, 
             aes(x = lon, y = lat, color = App), shape = 1) +
  labs(x = NULL, y = NULL) +
  labs(colour="Appointment\nRequired") + 
  labs(title = "Napa Valley Vineyards", subtitle = "Source: The Napa Wine Project") +
  theme(text = element_text(color = "#464646", family = "American Typewriter")) +
  theme(axis.text = element_blank()) + # remove axis labels
  theme(axis.ticks = element_blank()) + # remove axis tick marks 
  theme(plot.title = element_text(size = 24)) +
  theme(plot.subtitle = element_text(size = 10)) +
  theme(legend.key = element_rect(fill = "white")) # white background for key



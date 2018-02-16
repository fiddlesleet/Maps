library(tidyverse)
library(ggthemes)
library(rgdal)
library(viridis)
library(extrafont)
library(RColorBrewer)
devtools::install_github("ricardo-bion/ggtech", 
                         dependencies=TRUE)
library(ggtech)

##########
# GET DATA
##########

load(url("https://ikashnitsky.github.io/doc/misc/map-subplots/df-27-261-urb-rur.RData"))
load(url("https://ikashnitsky.github.io/doc/misc/map-subplots/spatial-27-261.RData"))

# convert shapefiles to df
df.sborders <- tidy(Sborders)
df.sn2 <- tidy(Sn2, region = "id")
df.sneighbors <- tidy(Sneighbors)

# join spatial & statistical data
df.europe <- left_join(df, df.sn2, by = "id")

##################
# MAKE MOSAIC PLOT
##################
# formatting
font <- "Roboto Condensed"
# palette
display.brewer.pal(n = 9, "YlGnBu")
pal <- brewer.pal(n = 9, "YlGnBu")[c(5:3)]

# from Stack Overflow
makeplot_mosaic <- function(data, x, y, ...) {
  xvar <- deparse(substitute(x))
  yvar <- deparse(substitute(y))
  mydata <- data[c(xvar, yvar)];
  mytable <- table(mydata);
  widths <- c(0, cumsum(apply(mytable, 1, sum)));
  heights <- apply(mytable, 1, function(x){c(0, cumsum(x/sum(x)))});
  alldata <- data.frame();
  allnames <- data.frame();
  for(i in 1:nrow(mytable)){
    for(j in 1:ncol(mytable)){
      alldata <- rbind(alldata, c(widths[i], 
                                  widths[i+1], 
                                  heights[j, i], 
                                  heights[j+1, i]));
    }
  }
  colnames(alldata) <- c("xmin", "xmax", "ymin", "ymax")
  
  alldata[[xvar]] <- rep(dimnames(mytable)[[1]], 
                         rep(ncol(mytable), nrow(mytable)));
  alldata[[yvar]] <- rep(dimnames(mytable)[[2]], nrow(mytable));
  
  ggplot(alldata, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)) + 
    geom_rect(color="white", aes_string(fill=yvar)) +
    xlab(paste(xvar, "(count)")) + 
    ylab(paste(yvar, "(proportion)"));
}

mosaic <- makeplot_mosaic(data = df %>% mutate(type = as.numeric(type)), 
                              x = subregion, y = type) +
  theme_void() +
  scale_fill_tech(theme="airbnb") + 
  scale_y_continuous(limits = c(0, 1.4))+
  annotate("text", x = c(27, 82.5, 186), y = 1.05, 
           label=c("EAST", "SOUTH", "WEST"), 
           size = 4, fontface = 2, 
           vjust = 0.5, hjust = 0,
           family = font) + 
  coord_flip() +
  theme(legend.position = "none")

#####################
# MAKE EUROPE BASEMAP
#####################

europe_basemap <- ggplot() +
  geom_polygon(data = df.sneighbors,
               aes(x = long, y = lat, group = group),
               fill = "#eeeeee", color = "#eeeeee") +
  coord_equal(ylim = c(1350000,5450000), xlim = c(2500000, 6600000)) + # make map square
  theme_map(base_family = font) + 
  theme(panel.border = element_rect(color = "black",
                                    size = .5,
                                    fill = NA),
        legend.position = c(1, 1),
        legend.justification = c(1, 1),
        legend.background = element_rect(colour = NA, fill = NA),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 11)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = NULL, y = NULL)

###########################
# MAKE EUROPE SUBREGION MAP
###########################

# format
display.brewer.pal(n = 9, "YlGnBu")
pal <- brewer.pal(n = 9, "YlGnBu")[c(5:3)]

map.europe_subregions <- europe_basemap +
  geom_polygon(data = df.europe,
               aes(x = long, y = lat, group = group, fill = subregion),
               color = NA) +
  scale_fill_tech(theme="etsy") + 
  theme(legend.position = "none")

#################
# MAP EUROPE DATA
#################

# format
display.brewer.pal(n = 9, "Greens")
green <- brewer.pal(n = 9, "Greens")[5]
display.brewer.pal(n = 9, "GnBu")
blue <- brewer.pal(n = 9, "GnBu")[6]
display.brewer.pal(n = 9, "BuPu")
purple <- brewer.pal(n = 9, "BuPu")[7]
pal <- c(green, blue, purple)
caption <- "Classification: De Beer, J., Van Der Gaag, N., & Van Der Erf, R. (2014). New classification of urban and rural NUTS 2 regions in Europe. NIDI Working Papers, 2014/3. Retrieved from http://www.nidi.nl/shared/content/output/papers/nidi-wp-2014-03.pdf
\nCredit: Ilya Kashnitsky"

europe_basemap +
  geom_polygon(data = df.europe,
               aes(x = long, y = lat, group = group, fill = type),
               color = "grey30", size=.1) +
  scale_fill_tech(theme="airbnb") + 
  geom_path(data = df.sborders, aes(x = long, y = lat, group = group), # add country borders 
            color = "grey20",
            size = .5) + 
  # add mosaic plot
  annotation_custom(grob = ggplotGrob(mosaic), 
                    # where
                    xmin = 2500000, xmax = 4000000, 
                    ymin = 4450000, ymax = 5450000) +
  # add regions plot
  annotation_custom(grob = ggplotGrob(map.europe_subregions), 
                    # where
                    xmin = 5400000, xmax = 6600000, 
                    ymin = 2950000, ymax = 4150000) +
  labs(title = "Urban / Rural classification of NUTS-2 regions of Europe\n",
       caption = paste(strwrap(caption, width = 95), collapse = '\n'))+
  theme(plot.title = element_text(size = 20),
        plot.caption = element_text(size = 12))

ggsave("mapping-urbanization-europe.png")
  






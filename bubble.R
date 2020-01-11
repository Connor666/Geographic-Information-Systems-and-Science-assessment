library(ggplot2)
library(ggmap)
library(maptools)
library(maps)
library(dplyr)
library(viridis)
library(mapproj)
library(readr)

China <- map_data("world") %>% filter(region=="China") #Region of China
myData <- read.csv('cw/ex.csv') # Read file
myBreaks <- c(0.1,0.3, 0.5,0.7, 1)

# Plot map
map <- 
  ggplot() +
  geom_polygon(data=China,aes(long, y = lat, group = group), fill="white", alpha=0.3) +
  geom_point(data=myData, aes(x=longitude, y = latitude, size=open.rate, color=open.rate, alpha=open.rate), shape=20, stroke=FALSE) +
  scale_size_continuous(name="Opening rate", trans="sqrt", range=c(1,12), breaks=myBreaks) +
  scale_alpha_continuous(name="Opening rate", trans="sqrt", range=c(0.1, 0.6), breaks=myBreaks) +
  scale_color_viridis(option="magma", trans="sqrt", breaks=myBreaks, name="Opening rate" ) + #Transformation "sqrt"
  theme_void()  + coord_map() +  xlim(70,149)+ ylim(18,55)+
  guides( colour = guide_legend()) +
  ggtitle("Window opening rate in China") +
  theme(
    legend.position = c(0.85, 0.8),
    text = element_text(color = "#f5f5f2"), # Choose colour
    plot.background = element_rect(fill = "#4e4d47", color = NA), 
    panel.background = element_rect(fill = "#4e4d47", color = NA), 
    legend.background = element_rect(fill = "#4e4d47", color = NA),
    plot.title = element_text(size= 16, hjust=0.1, color = "#f5f5f2", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
  )
plot(map)

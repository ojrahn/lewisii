######################################
############# Mapping data ###########
### By Mackenzie Urquhart-Cronish ####
############## 2019/10/10 ############
#####################################

library(ggsn) 
library(ggmap)
library(ggplot2)
library(ggrepel)
library(tidyverse)
library(viridis)

data <- read.csv("./data/02_Lewisii_surveys_2019.csv", na.strings = c("", "NA"))

data_HM <- read.csv("./data/04_lewisii_HM_recession_pathway.csv", na.strings = c("", "NA"))

# remove all rows that have an "NA" in survey_date column and all that include "Coleman" in Trip coloumn
data$Lat <- as.character(data$Lat)
data$Long <- as.character(data$Long)
data$Lat <- as.numeric(data$Lat)
data$Long <- as.numeric(data$Long)


data_1 <- data %>% 
  filter(survey_date != is.na(NA), 
         Trip != "Coleman_1") %>% 
          mutate(Long = Long * -1)

data_2 <- data_1 %>%
  filter(grepl("HM", Waypoints)) # filter cases containing HM

# register_google(key = "AIzaSyCiQ5MSpftPwpzAm9IcpMsQfLa_x3x4azg", write = TRUE)

myLocation <- c(-123.02334, 49.96126, -122.99035, 49.98610)
(-123.02334 + (-122.99035))/2
(49.96126 + (49.98610))/2


myMap <-
  get_googlemap(
    center = c(lon = -123.0068, lat = 49.969),
    source = "google",
    maptype = "satellite",
    zoom = 14
  )
ggmap(myMap)

# make datasets of year only, and then plot those lines. Need to write function
data_1928 <- data_HM %>% 
  group_by(year) %>% 
  filter(year == "1928")

data_1949 <- data_HM %>% 
  group_by(year) %>% 
  filter(year == "1949")


data_1977 <- data_HM %>% 
  group_by(year) %>% 
  filter(year == "1977")


data_2003 <- data_HM %>% 
  group_by(year) %>% 
  filter(year == "2003")


data_2016 <- data_HM %>% 
  group_by(year) %>% 
  filter(year == "2016")

ggmap(myMap) +
  geom_path(data = data_1928, 
            aes(color = year), size = 1.5, lineend = "round") + 
  geom_path(data = data_1949, 
            aes(color = year), size = 1.5, lineend = "round") + 
  geom_path(data = data_1977, 
            aes(color = year), size = 1.5, lineend = "round") + 
  geom_path(data = data_2003, 
            aes(color = year), size = 1.5, lineend = "round") + 
  geom_path(data = data_2016, 
            aes(color = year), size = 1.5, lineend = "round") + # figured out how to map recession lines!
  scale_color_viridis(option = "D") +
  geom_point(
    data = data_2,
    mapping = aes(x = Long, y = Lat),
    size = 2.5,
    color = "red2") +
  xlab("Longitude") + 
  ylab("Latitude")
 
# # code to label waypoints, may not be neccessary
#   labs(x = "Longitude", y = "Latitude") +
#   geom_label_repel(
#     data = data_2,
#     aes(x = Long, y = Lat, label = Waypoints),
#     size = 4,
#     vjust = 1,
#     hjust = -0.5,
#     angle = 60,
#     colour = "red"
#   ) 
#   
  
# Map of the PNW woth Mt. Baker, Mt. Rainier, and Garibalid PP mapped
# Rainer 46.89223 -121.67082
# Mt. Baker 48.72405 -121.83491
# Garibaldi 49.97635 -123.00183

PNW <- data.frame("mountain" = c("Mt. Rainier", "Mt. Baker", "Garibaldi Provincial Park"), "lat" = c(46.89223, 48.72405, 49.97635), lon = c(-121.67082, -121.83491, -123.00183) )
  
PNWmap <-
    get_googlemap(
      center = c(lon = -123.0068, lat = 48.43429),
      source = "google",
      maptype = "satellite",
      zoom = 7
    )
ggmap(PNWmap)
  
ggmap(PNWmap) +
    geom_point(
      data = PNW,
      mapping = aes(x = lon, y = lat),
      size = 4,
      color = "red2") +
    xlab("Longitude") + 
    ylab("Latitude") +
  geom_label_repel(
    data = PNW,
    aes(x = lon, y = lat, label = mountain),
    size = 6,
    vjust = 2,
    hjust = -0.5,
    angle = 60,
    colour = "red"
  ) 
# perhaps add shading where the cascade and south oast mountains are? 

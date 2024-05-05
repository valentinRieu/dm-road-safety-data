# Data visualization of our dataset

if(!require('tidyverse')) install.packages('tidyverse')
library(tidyverse)

if(!require('ggplot2')) install.packages('ggplot2')
library(ggplot2)

if(!require('ggthemes')) install.packages('ggthemes')
library(ggthemes)

if(!require('leaflet')) install.packages('leaflet')
library(leaflet)

if(!require('leaflet.extras')) install.packages('leaflet.extras')
library(leaflet.extras)

if(!require('corrplot')) install.packages('corrplot')
library(corrplot)

rm(list = ls())

data <- read_csv("data/rcs.csv")

data <- data %>%
  na.omit()

colours <- c()

for (i in 1:nrow(data)) {
  if (data$accident_severity[i] == 3) {
    colours <- c(colours, "green")
  } else if (data$accident_severity[i] == 2) {
    colours <- c(colours, "orange")
  } else {
    colours <- c(colours, "red")
  }
}

leaflet(data) %>%
  addTiles() %>%
  addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = ~number_of_casualties, color = colours) %>%
  addLegend("bottomright", colors = c("red", "orange", "green"), labels = c("Fatal", "Serious", "Slight"), title = "Severity")


data_numeric <- data %>%
  select_if(is.numeric) %>%
  dplyr::select(-c(accident_year, location_easting_osgr, police_force, location_northing_osgr, local_authority_district, longitude, latitude, first_road_number, second_road_number))


problems(data_numeric)

correlation_matrix <- cor(data_numeric)

postscript("plots/correlation_matrix.ps")
corrplot(correlation_matrix, method = "ellipse", type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

dev.off()



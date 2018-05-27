library(tidyverse)
library(leaflet)
library(geojsonio)
library(mapview)
library(sf)

data <- read_csv("APD-DATA-CLEAN.csv")
clean_data <- data %>% filter(!is.na(LONGITUDE))
austin_data <- filter(clean_data, -97.94 <= LONGITUDE, LATITUDE <=  30.52) %>% filter(LONGITUDE <= -97.56, 30.09 <= LATITUDE)

gf <- st_read("austin-council-wound.geojson")
plot(st_geometry(gf), col = sf.colors(10, categorical = TRUE), border = 'grey', axes = TRUE)
mapview(gf["district_id"], col.regions = sf.colors(10))
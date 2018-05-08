library(tidyverse)
library(reshape2)
library(leaflet)
library(jsonlite)
data <- read_csv("geodata.csv")
maxlat <- max(data$lat, na.rm=T)
maxlong <- max(data$long, na.rm=T)
minlat <- min(data$lat, na.rm=T)
minlong <- min(data$long, na.rm=T)

#register_google(key = "AIzaSyDoAf6cwt4aw_2iKxNN64C9tOeQ9ge8yMo")
data <- read_csv("geodata.csv")
clean_data <- data %>% filter(!is.na(long))
aus_data <- filter(clean_data, -97.94 <= long, lat <=  30.52) %>% filter(long <= -97.56, 30.09 <= lat)

geoJSON <- fromJSON("districts-processed-geoJSON.json")
geojson <- readLines("austin-council-processed.geojson", warn = FALSE) %>%
  paste(collapse = "\n") %>%
  fromJSON(simplifyVector = FALSE)

leaflet(aus_data) %>% setView(lng = -97.7341, lat = 30.2849, zoom = 10) %>%
  addTiles() %>% addGeoJSON(geojson) %>% addTiles() %>% addMarkers(clusterOptions = markerClusterOptions())
# leaflet(aus_data) %>% addTiles() %>% addMarkers(clusterOptions = markerClusterOptions())

# {
#   "type": "Feature",
#   "properties": {"district_id": "7", "council_representative": "Leslie Poole"},
#   "geometry": {
#     "type": "MultiPolygon",
#     "coordinates": [[
#       [-104.05, 48.99],
#       [-97.22,  48.98],
#       [-96.58,  45.94],
#       [-104.03, 45.94],
#       [-104.05, 48.99]
#       ]]
#   }
# }
# geodata <- read_csv("city-council.csv")
# substrRight <- function(x, n){
#   substr(x, nchar(x)-n+1, nchar(x))
# }
# rexp <- "^(\\w+)\\s?(.*)$"
# nexp <- "(-[0-9]{2}\\.[0-9]+)\\s([0-9]{2}\\.[0-9]+)"
# gdata <-  tibble(DISTRICT=geodata$COUNCIL_DISTRICT, SHAPE=sub(rexp,"\\1",geodata$the_geom), COORDS=sub(rexp,"\\2",geodata$the_geom))
# gdata <- mutate(gdata, locs = gsub("\\,", "]\\,", COORDS))
# gdata <- mutate(gdata, locs = gsub(nexp, "\\1, \\2", locs))
# gdata <- mutate(gdata, locs = gsub("\\(", "[", locs))
# gdata <- mutate(gdata, locs = gsub(")", "]", locs))
# gdata <- mutate(gdata, locs = gsub("-", "[-", locs))
# gdata <- mutate(gdata, locs = paste(locs, "]", sep = ""))
# d <- substrRight(gdata$locs[1], 6)
# 
# js <- gdata$locs[3]
# gdata <- mutate(gdata, locs = fromJSON(locs))
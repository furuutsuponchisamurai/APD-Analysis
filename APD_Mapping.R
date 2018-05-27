library(tidyverse)
library(reshape2)
library(leaflet)
library(jsonlite)

data <- read_csv("APD-DATA-CLEAN.csv")
clean_data <- data %>% filter(!is.na(LONGITUDE))
austin_data <- filter(clean_data, -97.94 <= LONGITUDE, LATITUDE <=  30.52) %>% filter(LONGITUDE <= -97.56, 30.09 <= LATITUDE)

# geoJSON <- fromJSON("districts-processed-geoJSON.json")
# geojson <- readLines("austin-council-processed.geojson", warn = FALSE) %>%
#   paste(collapse = "\n") %>%
#   fromJSON(simplifyVector = FALSE)

gg <- geojson_read("austin-council-wound.geojson", method="local")

leaflet(austin_data) %>% setView(lng = -97.7341, lat = 30.2849, zoom = 10) %>%
  addTiles() %>% addGeoJSON(gg, fillColor = topo.colors(10, alpha = NULL)) %>% addTiles() %>% addMarkers(clusterOptions = markerClusterOptions())

# Use below leaflet commands with leafletR
# sty <- styleCat(prop="district_id", val=c("1","2","3","4","5","6","7","8","9","10"),
#                 style.val=c('#8dd3c7','#ffffb3','#bebada','#fb8072','#80b1d3','#fdb462','#b3de69','#fccde5','#d9d9d9','#bc80bd'),
#                 leg="Council ID")
# map <- leaflet(data="austin-council-processed.geojson", title="District_id",
#                style=sty)

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
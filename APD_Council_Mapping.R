library(tidyverse)
library(leaflet)
library(geojsonio)
library(mapview)
library(tmap)
library(sf)
library(sp)
tmap_mode("view")

# Get data
data <- read_csv("APD-DATA-CLEAN.csv")
gf <- st_read("austin-council-wound.geojson")

# Clean/filter data
clean_data <- data %>% filter(!is.na(LONGITUDE))
austin_data <- filter(clean_data, -97.94 <= LONGITUDE, LATITUDE <=  30.52) %>% filter(LONGITUDE <= -97.56, 30.09 <= LATITUDE)
council_data <- rgdal::readOGR("austin-council-wound.geojson", "OGRGeoJSON")

# Check crime data for point in city council -> everything needs to have same projection
sp::coordinates(austin_data) <- ~LONGITUDE+LATITUDE
sp::proj4string(austin_data) <- sp::proj4string(council_data)
tmp_austin_data <- austin_data
d_ids <- as_tibble(sp::over(austin_data, council_data))
austin_data <- st_as_sf(austin_data)
austin_council_crimes <- bind_cols(austin_data, d_ids)
district_crimes <- as_tibble(austin_council_crimes$district_id) %>% rename(district_id = value) %>% group_by(district_id) %>% summarise(n = length(district_id))
ggf <- merge(gf, district_crimes)

# Mapping
plot(ggf["n"], border = 'black', axes = TRUE, key.pos=4)
m <- mapview(ggf["n"], col.regions = sf.colors(10))
tm_shape(ggf) + tm_fill("n", palette = sf.colors(5), auto.palette.mapping = FALSE, title = "APD Incidents")
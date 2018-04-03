#APD Cleaning
# Google API Key AIzaSyDoAf6cwt4aw_2iKxNN64C9tOeQ9ge8yMo
library(tidyverse)
library(magrittr)
library(nominatim)
library(ggmap)
library(maps)

apd = read_csv('APD_Data.csv')
apd <- mutate(apd, Date= as.Date(Date, format= "%m/%d/%y"))
apd <- mutate(apd, ADDRESS = ifelse(grepl("BLOCK", ADDRESS), str_c(str_replace(ADDRESS, " BLOCK", ""),", Austin, TX"), str_c(ADDRESS, ", Austin, TX")))

crime_categorizer <- function (crime) {
  if (grepl("ASSAULT", crime)) {
    return("ASSAULT")
  }
  else if (grepl("THEFT", crime)) {
    return("THEFT")
  }
  else if (grepl("BURGLARY", crime) | grepl("ROBBERY", crime)) {
    return("BURGLARY")
  }
  else {
    return(crime)
  }
}

clean_apd <- mutate(apd, crime_cat = ifelse(grepl("DWI", `Crime Type`),'DWI',
                                             ifelse(grepl("ASSAULT", `Crime Type`),'ASSAULT',
                                             ifelse(grepl("THEFT", `Crime Type`),'THEFT',
                                                    ifelse(grepl("BURGLARY", `Crime Type`) | grepl("ROBBERY", `Crime Type`),'BURGLARY',`Crime Type`
                                             )
                                             )
                                             )
                                             )
                     )

clean_distinct_crimes <- clean_apd %>% count(`crime_cat`) %>% arrange(desc(n))
clean_distinct_addresses <- clean_apd %>% count(`ADDRESS`) %>% arrange(desc(n)) %>% filter(!grepl("UNKNOWN", ADDRESS))

#Robberies
robberies <- filter(clean_apd, crime_cat == "BURGLARY")
ggplot(data = robberies, mapping = aes(x = Time)) + geom_histogram(binwidth = 7200)

#Geocoding?qmap
#geocode("4100 GUADALUPE ST, Austin, TX")
#locations <- osm_geocode(head(clean_distinct_addresses$ADDRESS, 10), key = getOption("OSM_API_KEY","Qug7p0udpNTluLDS2033ZTHY3ZLSyt4G"))
#locations <- osm_geocode(mutiple_crimes$ADDRESS, key = getOption("OSM_API_KEY","Qug7p0udpNTluLDS2033ZTHY3ZLSyt4G"))
#write_tsv(locations, "google_locations.tsv")

#define a function that will process googles server responses for us.
getGeoDetails <- function(address){   
  #use the gecode function to query google servers
  geo_reply = geocode(address, output='all', messaging=TRUE, override_limit=TRUE)
  #now extract the bits that we need from the returned list
  answer <- data.frame(lat=NA, long=NA, accuracy=NA, formatted_address=NA, address_type=NA, status=NA)
  answer$status <- geo_reply$status
  
  #if we are over the query limit - want to pause for an hour
  while(geo_reply$status == "OVER_QUERY_LIMIT"){
    print("OVER QUERY LIMIT - Pausing for 1 hour at:") 
    time <- Sys.time()
    print(as.character(time))
    Sys.sleep(60*60)
    geo_reply = geocode(address, output='all', messaging=TRUE, override_limit=TRUE)
    answer$status <- geo_reply$status
  }
  
  #return Na's if we didn't get a match:
  if (geo_reply$status != "OK"){
    return(answer)
  }   
  #else, extract what we need from the Google server reply into a dataframe:
  answer$lat <- geo_reply$results[[1]]$geometry$location$lat
  answer$long <- geo_reply$results[[1]]$geometry$location$lng   
  if (length(geo_reply$results[[1]]$types) > 0){
    answer$accuracy <- geo_reply$results[[1]]$types[[1]]
  }
  answer$address_type <- paste(geo_reply$results[[1]]$types, collapse=',')
  answer$formatted_address <- geo_reply$results[[1]]$formatted_address
  
  return(answer)
}

#initialise a dataframe to hold the results
geocoded <- data.frame()
# find out where to start in the address list (if the script was interrupted before):
startindex <- 1
#if a temp file exists - load it up and count the rows!
tempfilename <- paste0('crime_locations', '_temp_geocoded.rds')
if (file.exists(tempfilename)){
  print("Found temp file - resuming from index:")
  geocoded <- readRDS(tempfilename)
  startindex <- (nrow(geocoded) + 1)
  print(startindex)
}

# Start the geocoding process - address by address. geocode() function takes care of query speed limit.
addresses <- clean_distinct_addresses$ADDRESS
for (ii in seq(startindex, length(addresses))){
  print(paste("Working on index", ii, "of", length(addresses)))
  #query the google geocoder - this will pause here if we are over the limit.
  result = getGeoDetails(addresses[ii]) 
  print(result$status)     
  result$index <- ii
  #append the answer to the results file.
  geocoded <- rbind(geocoded, result)
  #save temporary results as we are going along
  saveRDS(geocoded, tempfilename)
}

geo_data <- as_tibble(geocoded)


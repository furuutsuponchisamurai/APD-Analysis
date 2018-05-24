 #APD Cleaning
library(tidyverse)
library(magrittr)
library(nominatim)
library(ggmap)
library(maps)

google = Sys.getenv("GOOGLE")
register_google(key = google)
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

#define a function that will process googles server responses for us.
getGeoData <- function(address){   
  #use the gecode function to query google servers
  geo_reply = geocode(address, output='all', messaging=TRUE, override_limit=TRUE)
  #now extract the bits that we need from the returned list
  answer <- tibble(lat=NA, long=NA, accuracy=NA, formatted_address=NA, address_type=NA, status=NA)
  answer$status <- geo_reply$status
  
  #if we are over the query limit - want to pause for an hour
  while(geo_reply$status == "OVER_QUERY_LIMIT"){
    print("OVER QUERY LIMIT - Pausing for 24 hour at:") 
    time <- Sys.time()
    print(as.character(time))
    Sys.sleep(60*60*60)
    geo_reply = geocode(address, output='all', messaging=TRUE, override_limit=TRUE)
    answer$status <- geo_reply$status
  }
  
  #return found values into tibble, rest NA:
  if (geo_reply$status != "OK"){
    return(answer)
  }   
  
  answer$lat <- geo_reply$results[[1]]$geometry$location$lat
  answer$long <- geo_reply$results[[1]]$geometry$location$lng   
  if (length(geo_reply$results[[1]]$types) > 0){
    answer$accuracy <- geo_reply$results[[1]]$types[[1]]
  }
  answer$address_type <- paste(geo_reply$results[[1]]$types, collapse=',')
  answer$formatted_address <- geo_reply$results[[1]]$formatted_address
  answer$input_address <- address
  
  return(answer)
}

#initialise a dataframe to hold the results
geocoded <- tibble()
# find out where to start in the address list (if the script was interrupted before):
startindex <- 1
#Add data in if a tempfile exists
tempfile <- "temp_crime_locations.rds"
if (file.exists(tempfile)){
  print("Found temp file - resuming from index:")
  geocoded <- readRDS(tempfile)
  startindex <- (nrow(geocoded) + 1)
  print(startindex)
}

# Start the geocoding process - address by address. geocode() function takes care of query speed limit.
addresses <- clean_distinct_addresses$ADDRESS
for (ii in seq(startindex, length(addresses))){
  print(paste("Working on index", ii, "of", length(addresses)))
  #query the google geocoder - this will pause here if we are over the limit.
  result = getGeoData(addresses[ii]) 
  print(result$status)     
  result$index <- ii
  #append the answer to the results file.
  geocoded <- rbind(geocoded, result)
  #save temporary results as we are going along
  saveRDS(geocoded, tempfile)
}

distinct_addresses <- clean_distinct_addresses %>%
  select(-n) %>% mutate(form_address = geodata$formatted_address, lat = geodata$lat, long = geodata$long)
# 
# for (i in seq(1, length(distinct_addresses$ADDRESS))){
#   if (is.na(distinct_addresses$form_address[i])){
#     print(paste("Working on index", i, "of", length(distinct_addresses$ADDRESS)))
#     result <- getGeoData(distinct_addresses$ADDRESS[i])
#     distinct_addresses$lat[i] <- result$lat
#     distinct_addresses$long[i] <- result$long
#     distinct_addresses$form_address[i] <- result$formatted_address
#     }
# }

cleaned_apd <- clean_apd %>%
  select(-LOCATION_TYPE, -`Location 1`, -LONGITUDE, -LATITUDE) %>% 
  mutate(LONGITUDE = distinct_addresses$long[match(ADDRESS, distinct_addresses$ADDRESS)],
         LATITUDE = distinct_addresses$lat[match(ADDRESS, distinct_addresses$ADDRESS)],
         FORMATTED_ADDRESS = distinct_addresses$form_address[match(ADDRESS, distinct_addresses$ADDRESS)])

write_csv(cleaned_apd, "APD-DATA-CLEAN.csv")
write_csv(geocoded, "geodata.csv")

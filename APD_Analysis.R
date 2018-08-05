# Analysis of crime data
library(tidyverse)

crime_data <- read_delim("council-crime-2016.tsv", "\t") %>% mutate(district_id = as.character(district_id))
top_15_incidents <- crime_data %>% count(crime_cat, sort = TRUE) %>% head(15)

# Filtering join for crime_data
filtered_crime <- crime_data %>% semi_join(top_15_incidents)
# visiualize crime counts for top 15
ggplot(data = filtered_crime) + geom_bar(mapping = aes(x = crime_cat))
ggplot(data = crime_data) + stat_count(mapping = aes(x = district_id))
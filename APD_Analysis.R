# Analysis of crime data
library(tidyverse)

crime_data <- read_delim("council-crime-2016.tsv", "\t")
top_15_incidents <- crime_data %>% count(crime_cat, sort = TRUE) %>% head(15)

# Filtering join for crime_data
filtered_crime <- crime_data %>% semi_join(top_15_incidents)
# visiualize crime counts for top 15
# ggplot(data = top_15_incidents) + geom_point(mapping = aes(x = crime_cat, y = n))
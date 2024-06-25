##Merged Map Datasets


library(here)
library(dplyr)
library(tidycensus)
library(tidyverse)
library(sf)
library(leaflet)
library(tigris)

virginia_2018_2022 = read.csv(here("Data/merged_data", "virginia_2018_2022.csv"), sep = ",", header = TRUE)
appalachia_2018_2022 = read.csv(here("Data/merged_data", "appalachia_2018_2022.csv"), sep = ",", header = TRUE)

#use tigris library to get county shapefiles
options(tigris_use_cache = TRUE)
appalachian_states <- c("AL", "GA", "KY", "MD", "MS", "NY", "NC", "OH", "PA", "SC", "TN", "VA", "WV")

all_counties <- list()

for (state in appalachian_states) {
  state_counties <- counties(state = state, cb = TRUE)
  all_counties[[state]] <- state_counties
}

# combine all counties into a single sf object
us_counties <- do.call(rbind, all_counties)
us_counties$GEOID = as.integer(us_counties$GEOID)

#merging with virginia
map_virginia_2018_2022 <- us_counties %>%
  filter(STATEFP == "51") %>%
  right_join(virginia_2018_2022, by = c("GEOID" = "FIPS"), multiple = "all")

#merging with appalachia
map_appalachia_2018_2022 <- us_counties %>%
  right_join(appalachia_2018_2022, by = c("GEOID" = "FIPS"), multiple = "all")


#Data export
#write.csv(map_appalachia_2018_2022, 
#file = here("Data/merged_data", "map_virginia_2018_2022.csv"),
#row.names = FALSE) 

#write.csv(map_virginia_2018_2022, 
#file = here("Data/merged_data", "map_appalachia_2018_2022.csv"),
#row.names = FALSE) 

library(sf)
library(deltamapr)
library(tidyverse)

dftest = df_latlong %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

nuts = df_nutrients_clean %>%
  select(Latitude, Longitude) %>%
  filter(!is.na(Latitude)) %>%
  distinct() %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

ggplot()+
  geom_sf(data = WW_Delta)+
  geom_sf(data = nuts)+
  coord_sf(ylim = c(37.5, 39), xlim = c(-122.5, -121))

#some in odd places, but none obviously on dry land. 

#add geographic locations
stations = read_csv("data/station_metadata.csv") %>%
  mutate(Longitude = as.numeric(str_trim(Longitude)), Latitude = as.numeric(str_trim(Latitude))) %>%
  filter(!is.na(Latitude), !is.na(Longitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)

missingstations = filter(stations, is.na(`Geographic Description`))

ggplot()+
  geom_sf(data = WW_Delta) +
  geom_sf(data = missingstations)+
  geom_sf_label(data = missingstations, aes(label = Station_Name))


ggplot()+
  geom_sf(data = WW_Delta) +
  geom_sf(data = missingstations)+
  geom_sf_label(data = missingstations, aes(label = Station_Name))+
  coord_sf(xlim = c(-121.8, -121), ylim = c(37.5, 38))

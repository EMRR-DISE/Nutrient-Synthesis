library(sf)
library(deltamapr)

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
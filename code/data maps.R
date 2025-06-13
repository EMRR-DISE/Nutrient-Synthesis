#maps of data

library(tidyverse)
library(discretewq)
library(zooper)
library(sf)
library(ggmap)
library(deltamapr)
library(cder)
library(dataRetrieval)
library(usmap)


#It might actually make more sense to do a heatmap by motnh, year, region.

Regions = deltamapr::R_EDSM_Subregions_Mahardja_FLOAT
ggplot()+
  geom_sf(data = WW_Delta)+
  geom_sf(data = Regions, aes(fill = Region), alpha = 0.5)

#yeah, let's use these subregions for now

#start with zooplankton
zoops = Zoopsynther(Data_type = "Community", Size_class = "Meso", Years = c(2010:2024))
zoops2 = select(zoops, SampleID, Latitude, Longitude, Source, Date, Station) %>%
  distinct() %>%
  filter(!is.na(Latitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = F) %>%
  st_transform(crs = st_crs(Regions)) %>%
  st_join(Regions) %>%
  st_drop_geometry() %>%
  mutate(Year = year(Date), Month = month(Date))

zoopssampls = zoops2 %>%
  group_by(Year, SubRegion) %>%
  summarize(N = n()) %>%
  filter(N !=0)

ggplot(zoopssampls, aes(x = Year, y = SubRegion, fill = N))+ geom_tile()+
  scale_fill_viridis_c()

#now clams

#GRTS sites (from betsy, with grazing rate help from jan), as well as longterm sites, but just may and october
clams = read_csv("data/short_term_density_ms.csv")
clams2 = select(clams, Year, StationCode, Latitude, Longitude, Month, Season, Survey) %>%
  distinct()%>%
  filter(!is.na(Latitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = F) %>%
  st_transform(crs = st_crs(Regions)) %>%
  st_join(Regions) %>%
  st_drop_geometry() %>%
  filter(Survey == "GRTS")


#now the normal EMP sites
clamsEMP =  read_csv("https://pasta.lternet.edu/package/data/eml/edi/1036/5/5855f038ec2899f759db9ee826d0092a")
clamsEMP2 = select(clamsEMP, Station, Month, Year, Latitude, Longitude, Date) %>%
  distinct()%>%
  filter(!is.na(Latitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = F) %>%
  st_transform(crs = st_crs(Regions)) %>%
  st_join(Regions) %>%
  st_drop_geometry() %>%
  mutate(Year = year(Date), Month = month(Date))

clamsampls = bind_rows(clamsEMP2, clams2) %>%
  group_by(Year, SubRegion) %>%
  summarize(N = n()) %>%
  filter(N !=0, Year >2009)

ggplot(clamsampls, aes(x = Year, y = SubRegion, fill = N))+ geom_tile()+
  scale_fill_viridis_c()

### Map of nutrient stations
nuts = read_csv("Data/Discrete_Stations_Consolidated.csv")
nuts2 = mutate(nuts, Longitude = as.numeric(Longitude), Latitude = as.numeric(Latitude)) %>%
  filter(!is.na(Longitude), !is.na(Latitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

#more stream data 
streams = st_read("C:/Users/rhartman/OneDrive - California Department of Water Resources/Office meetins and programs/mentorship/data/MajorCalRivers/MajorCalifRivers.shp")
cali = us_map("state", include = "CA")

ggplot()+
  geom_sf(data = WW_Watershed, color = "blue")+
  geom_sf(data = streams, color = "blue")+
  geom_sf(data = nuts2)+
  geom_sf(data = cali, alpha =0)

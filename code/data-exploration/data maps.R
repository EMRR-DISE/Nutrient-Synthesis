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
library(patchwork)
library(here)

#It might actually make more sense to do a heatmap by motnh, year, region.

Regions = deltamapr::R_EDSM_Subregions_Mahardja_FLOAT
ggplot()+
  geom_sf(data = WW_Delta)+
  geom_sf(data = Regions, aes(fill = SubRegion), alpha = 0.5)

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
  summarize(Zoops = n()) %>%
  filter(Zoops !=0)

ggplot(zoopssampls, aes(x = Year, y = SubRegion, fill = Zoops))+ geom_tile()+
  scale_fill_viridis_c()

#now clams

#GRTS sites (from betsy, with grazing rate help from jan), as well as longterm sites, but just may and october
clams = read_csv(here("data/raw/short_term_density_ms.csv"))
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
  summarize(Clams = n()) %>%
  filter(Clams !=0, Year >2009)

ggplot(clamsampls, aes(x = Year, y = SubRegion, fill = Clams))+ geom_tile()+
  scale_fill_viridis_c()

#map the regular stations
clamsEMP3 = select(clamsEMP, Station, Month, Year, Latitude, Longitude, Date) %>%
  distinct()%>%
  filter(!is.na(Latitude), Year >2008) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = F) 

ggplot()+
  geom_sf(data = WW_Delta)+
  geom_sf(data = Regions, aes(fill = SubRegion), alpha = 0.5)+
  geom_sf(data = clamsEMP3)+
  scale_fill_discrete(guide = NULL)



### Map of nutrient stations
nuts = read_csv(here("data/raw/Discrete_Stations_Consolidated.csv"))
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


#chlorophyll samples
chl = wq(Sources = c("EMP", "USGS_CAWSC", "USGS_SFBS", "NCRO"), Start_year = 2010, End_year = 2024)


chl2 = select(chl, Source, Station, Month, Year, Latitude, Longitude, Date) %>%
  distinct()%>%
  filter(!is.na(Latitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = F) %>%
  st_transform(crs = st_crs(Regions)) %>%
  st_join(Regions) %>%
  st_drop_geometry() %>%
  mutate(Year = year(Date), Month = month(Date))

chlsampls = bind_rows(chl2, clams2) %>%
  group_by(Year, SubRegion) %>%
  summarize(Chl = n()) %>%
  filter(Chl !=0, Year >2009)

#now put all the samples togetehr

allsamples = left_join(zoopssampls, clamsampls) %>%
  left_join(chlsampls) %>%
  left_join(Regions) %>%
  pivot_longer(cols = c(Zoops, Chl, Clams), names_to = "Type", values_to = "NSamples") %>%
  ungroup() %>%
  st_as_sf()

p1= ggplot(filter(allsamples, Year < 2017))+
  geom_sf(data = WW_Delta)+
  geom_sf(data = filter(allsamples, Year <2017), aes(fill = NSamples), alpha = 0.5) +
  facet_grid(Year~ Type)+xlab(NULL)+ylab(NULL)+ scale_fill_viridis_c(limits = c(0, 350), guide = "none")+
  coord_sf(xlim = c(-122.5, -121.5), ylim = c(37.7, 38.6), crs = 4326)

p2= ggplot(filter(allsamples, Year >= 2017))+
  geom_sf(data = WW_Delta)+
  geom_sf(data = filter(allsamples, Year >= 2017), aes(fill = NSamples), alpha = 0.5) +
  facet_grid(Year~ Type)+xlab(NULL)+ylab(NULL)+ scale_fill_viridis_c(limits = c(0, 350))+
  coord_sf(xlim = c(-122.5, -121.5), ylim = c(37.7, 38.6), crs = 4326)

p1+p2

ggsave(here("results/plots/samplemap.png"), width = 12, height = 12, device = "png")

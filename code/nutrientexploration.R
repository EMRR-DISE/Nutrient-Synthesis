#exploritory code and graphs
#Rosemary Hartman
#2024-8-14

library(tidyverse)
library(discretewq)
library(sf)
library(deltamapr)

#pull all the nutrient data that is currently available in the discretewq package

nuts = wq(Sources = c("DOP", "EMP", "FMWT", "NCRO", "USBR", "USGS_CAWSC", "USGS_SFBS", "YBFMP"))

#filter so it's just ones with nutrients

nutsx = filter(nuts, !is.na(DissNitrateNitrite)) %>%
  filter(!is.na(Longitude)) %>%
  mutate(Month2 = month(Date, abbr = T, label = T)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

#Now pull in recent MWQI data from WDL

MWQI = read_csv("data/WQDataReport.csv")
MWQIx = filter(MWQI, Analyte == "Dissolved Nitrate + Nitrite")

#station gps coordinates
stations = read_csv("data/WQstationsWDL.csv") %>%
  select(station_number, station_id, latitude, longitude, sample_count)

MWQIx2 = left_join(MWQIx, stations, by = c("StationNumber" = "station_number")) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  mutate(Source = "MWQI")

ggplot(filter(nutsx, Year %in% c(2019, 2020, 2021), Source != "DOP"))+
  geom_sf(data = WW_Delta, fill = "lightcyan", color = "lightcyan3")+
  geom_sf(aes(shape = Source, color = Source)) +
  scale_shape_manual(values = c(15, 16, 17, 18,19), labels = c("DWR - EMP","DWR-MWQI", "DWR- NCRO", "USGS - CAWSC", "USGS - SFBS"))+
  scale_color_manual(values = c("darkgreen", "salmon", "plum2", "gold", "blue"), labels = c("DWR - EMP","DWR-MWQI",  "DWR- NCRO", "USGS - CAWSC", "USGS - SFBS"))+
  geom_sf(data = MWQIx2, aes(shape = "MWQI", color = "MWQI"))+
  coord_sf(xlim = c(-122.3, -121.4), ylim = c(37.8, 38.5))+
  facet_wrap(~Month2)+
  theme_bw()


#how many samples per month and year?
nutsum = group_by(nutsx, Source, Month, Year) %>%
  summarize(N = n()) %>%
  filter(Year >2010)

ggplot(nutsum, aes(x = Year+(1-Month)/12, y = N, color = Source))+ geom_line(size =.75)+
  scale_color_manual(values = c("skyblue", "darkgreen", "salmon3", "plum3", "gold2"), 
                     labels = c("DOP", "DWR - EMP", "DWR- NCRO", "USGS - CAWSC", "USGS - SFBS"))+
  theme_bw()+
  ylab("Number of samples")+
  xlab("Month and Year")
  
  

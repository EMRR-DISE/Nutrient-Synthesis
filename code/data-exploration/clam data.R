#let's look at clam data

#we published everything through 2021 with the drought project
#https://portal.edirepository.org/nis/mapbrowse?packageid=edi.1653.1

library(tidyverse)
library(sf)
library(deltamapr)

EMPclams = read_csv("https://pasta.lternet.edu/package/data/eml/edi/1653/1/407336fbf9a2005780935acc5f223e34") 
GRTSclams = read_csv("https://pasta.lternet.edu/package/data/eml/edi/1653/1/8a1292d209f5ed2a7f1de1f140b0b837") 
clamregressions = read_csv("https://pasta.lternet.edu/package/data/eml/edi/1653/1/0f11bd41ec1b41be92e502a9fa47209e") 

#meterCubedPerMeterSquaredPerDay
TotalFiltration = group_by(EMPclams, Station, Date, Latitude, Longitude) %>%
  summarize(Filtration = sum(Filtration_Rate), Turnover = sum(Turnover_Rate)) %>%
  mutate(Year = year(Date), Month = month(Date), Longitude = case_when(Longitude >0 ~ Longitude*-1,
                                                                       TRUE ~ Longitude))

ggplot(TotalFiltration, aes(x = Station, y = Filtration))+ geom_boxplot()

ggplot(TotalFiltration, aes(x = as.factor(year(Date)), y = Filtration))+ geom_boxplot()

#limit to 2010-present, and get rid of stations in San Pablo Bay

TotalFilt2 = filter(TotalFiltration, year(Date)>2009, Longitude > -122.2)


ggplot(TotalFilt2, aes(x = Station, y = Filtration))+ geom_boxplot()

ggplot(TotalFilt2, aes(x = as.factor(year(Date)), y = Filtration))+ geom_boxplot()


ggplot(TotalFilt2, aes(x = as.factor(year(Date)), y = log(Filtration+1)))+ geom_boxplot()

#what does this look like regionally?
#R_EDSM_Regions_1718P1
TotalFilt_regions = st_as_sf(TotalFilt2, coords = c("Longitude", "Latitude"), crs = 4326, remove = F) %>%
  st_transform(crs = st_crs(R_EDSM_Regions_1718P1)) %>%
  st_join(R_EDSM_Regions_1718P1) %>%
  st_drop_geometry()


ggplot(TotalFilt_regions, aes(x = as.factor(year(Date)), y = log(Filtration+1)))+ geom_boxplot()+
  facet_wrap(~Region)

ggplot(TotalFilt_regions, aes(x = as.factor(year(Date)), y = log(Filtration+1)))+ geom_point()+
  facet_wrap(~Region)

ggplot(TotalFilt_regions, aes(x = as.factor(Month), y = log(Filtration+1)))+ geom_point()+
  facet_wrap(~Region)
#some differences between years, bigger differences bewteen regions


test = filter(TotalFilt_regions, Region == "North" & year(Date) == 2017)

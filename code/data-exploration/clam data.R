#let's look at clam data

#we published everything through 2021 with the drought project
#https://portal.edirepository.org/nis/mapbrowse?packageid=edi.1653.1

library(tidyverse)
library(sf)
library(deltamapr)
library(readxl)

EMPclams = read_csv("https://pasta.lternet.edu/package/data/eml/edi/1653/1/407336fbf9a2005780935acc5f223e34") 
GRTSclams = read_csv("https://pasta.lternet.edu/package/data/eml/edi/1653/1/8a1292d209f5ed2a7f1de1f140b0b837") 
clamregressions = read_csv("https://pasta.lternet.edu/package/data/eml/edi/1653/1/0f11bd41ec1b41be92e502a9fa47209e") 

#get additional Suisun Marsh data

#get additional North Delta data
t3 = read_excel("data/raw/T3_Delta2015_BioRecGR_final.xlsx") %>%
  mutate(Station = as.character(Station))%>%
  rename(Month = month, Year = year)
t5 = read_excel("data/raw/T5_Delta2016_BioRecGR_final.xlsx")%>%
  mutate(Station = as.character(Station), Year = 2016)
t7 = read_excel("data/raw/T7_Delta2017_BioRecGR_final.xlsx")%>%
  mutate(Station = as.character(Station), Year = 2017) %>%
  rename(Lat = lat, Long = long, Month = month, Clam = clam)
t9 = read_excel("data/raw/T9_Delta2018_BioRecGR_Final.xlsx")%>%
  mutate(Station = as.character(Station), Year = 2018)

USGSclams = bind_rows(t3, t5, t7, t9) %>%
  rename(Filtration_Rate = GR, Turnover_Rate = GRTO, Latitude = Lat, Longitude = Long)%>%
  mutate(Date = ymd(paste(Year, Month, "15", sep = "-"))) %>%
  group_by(Station, Date, Latitude, Longitude) %>%
  summarize(Filtration = sum(Filtration_Rate, na.rm =T), Turnover = sum(Turnover_Rate))
#I think GR is the same as filtration rate, basically

#meterCubedPerMeterSquaredPerDay
TotalFiltration = rename(GRTSclams, Station = SiteID) %>%
  mutate(Date = ymd(paste(Year, Month, "15", sep = "-")), Filtration_Rate = Turnover_Rate*Depth) %>%
  bind_rows(EMPclams) %>%
  group_by(Station, Date, Latitude, Longitude, Depth) %>%
  summarize(Filtration = sum(Filtration_Rate, na.rm =T), Turnover = sum(Turnover_Rate, na.rm =T)) %>%
  mutate(Year = year(Date), Month = month(Date), Longitude = case_when(Longitude >0 ~ Longitude*-1,
                                                                       TRUE ~ Longitude)) %>%
  bind_rows(USGSclams)

#ggplot(filter(TotalFiltration, !is.na(Filtration)), aes(x = Station, y = Filtration))+ geom_boxplot()

ggplot(TotalFiltration, aes(x = as.factor(year(Date)), y = Filtration))+ geom_boxplot()

#limit to 2010-present, and get rid of stations in San Pablo Bay

TotalFilt2 = filter(TotalFiltration, year(Date)>2009, Longitude > -122.2)


#ggplot(TotalFilt2, aes(x = Station, y = Turnover))+ geom_boxplot()

ggplot(TotalFilt2, aes(x = as.factor(year(Date)), y = Turnover))+ geom_boxplot()

#get teh better vesion of the regions



# Load Delta EDSM shapefile and only keep SubRegions east of Carquinez Straight
sf_delta <- st_read("data/spatial/delta_subregions.shp")

#what does this look like regionally?
TotalFilt_regions = st_as_sf(TotalFilt2, coords = c("Longitude", "Latitude"), crs = 4326, remove = F) %>%
  st_transform(crs = st_crs(R_EDSM_Regions_1718P1)) %>%
  st_join(sf_delta) %>%
  st_drop_geometry()


ggplot(TotalFilt_regions, aes(x = as.factor(year(Date)), y = log(Turnover+1)))+ geom_boxplot()+
  facet_wrap(~Region)



ggplot(TotalFilt_regions, aes(x = as.factor(year(Date)), y = log(Filtration+1)))+ geom_boxplot()+
  facet_wrap(~Region)

ggplot(TotalFilt_regions, aes(x = as.factor(year(Date)), y = log(Turnover+1)))+ geom_point()+
  facet_wrap(~Region)

ggplot(TotalFilt_regions, aes(x = as.factor(Month), y = log(Turnover+1)))+ geom_point()+
  facet_wrap(~Region)
#some differences between years, bigger differences bewteen regions


test = filter(TotalFilt_regions, Region == "North" & year(Date) == 2017)

ggplot()+
  geom_sf( data = WW_Delta)+
  geom_sf(data = sf_delta, aes(fill = SubRegion), alpha = 0.2)+
  scale_fill_discrete(guide = NULL)+
  coord_sf(ylim = c(37.8, 38.6), xlim = c(-122.2, -121.3))+
  theme_bw()
  

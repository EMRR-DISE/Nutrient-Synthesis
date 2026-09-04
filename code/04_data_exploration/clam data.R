#let's look at clam data

#we published everything through 2021 with the drought project
#https://portal.edirepository.org/nis/mapbrowse?packageid=edi.1653.1

library(tidyverse)
library(sf)
library(deltamapr)
library(readxl)

#Previous grazing rate data from the drought synthesis package
EMPclams = read_csv("https://pasta.lternet.edu/package/data/eml/edi/1653/1/407336fbf9a2005780935acc5f223e34")
GRTSclams = read_csv("https://pasta.lternet.edu/package/data/eml/edi/1653/1/8a1292d209f5ed2a7f1de1f140b0b837")
clamregressions = read_csv("https://pasta.lternet.edu/package/data/eml/edi/1653/1/0f11bd41ec1b41be92e502a9fa47209e")

#get additional Suisun Marsh data from the SMSCG data package on EDI
#rename and standardize format
siusunclams = read_csv("data/raw/SMSCG_clam_EDI_2018_2021_corr.csv") %>%
  rename(Latitude = North_decimal_degrees, Longitude = West_decimal_degrees,
         CorbiculaBiomass = Corbicula_AFDM_g_per_m2,
         PotamocorbulaBiomass = Potamocorbula_AFDM_g_per_m2,
         Filtration = Total_filtration_rate_m3_per_m2_per_day,
         Turnover = Total_grazing_turnover_per_day) %>%
  select(Year, Month, Station, Date, Latitude, Longitude, CorbiculaBiomass, PotamocorbulaBiomass,
         Filtration, Turnover) %>%
  mutate(Month = month(Date))

#get additional North Delta data from usgs, standardize format
#https://www.sciencebase.gov/catalog/item/5e9e225b82cefae35a106f5e
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

#put each year's file together
USGSclams = bind_rows(t3, t5, t7, t9) %>%
  rename(Filtration_Rate = GR, Turnover_Rate = GRTO, Latitude = Lat, Longitude = Long)%>%
  mutate(Date = ymd(paste(Year, Month, "15", sep = "-"))) %>%
  group_by(Station, Date, Latitude, Longitude) %>%
  summarize(Filtration = sum(Filtration_Rate, na.rm =T), Turnover = sum(Turnover_Rate),
            CorbiculaBiomass = Biomass[which(Clam == "CF")], PotamocorbulaBiomass = Biomass[which(Clam == "PA")])
#I think GR is the same as filtration rate, basically. We only have turnover rate for 2015 for some odd reason

#Put the three datasets together
TotalFiltration = rename(GRTSclams, Station = SiteID) %>%
  mutate(Date = ymd(paste(Year, Month, "15", sep = "-")), Filtration_Rate = Turnover_Rate*Depth) %>%
  bind_rows(EMPclams) %>%
  group_by(Station, Date, Latitude, Longitude, Depth) %>%
  summarize(Filtration = sum(Filtration_Rate, na.rm =T), Turnover = sum(Turnover_Rate, na.rm =T),
            CorbiculaBiomass = sum(Biomass[which(Clam == "CF")]),
            PotamocorbulaBiomass = sum(Biomass[which(Clam == "PA")])) %>%
  mutate(Year = year(Date), Month = month(Date), Longitude = case_when(Longitude >0 ~ Longitude*-1,
                                                                       TRUE ~ Longitude)) %>%
  bind_rows(USGSclams) %>%
  bind_rows(siusunclams)

#ggplot(filter(TotalFiltration, !is.na(Filtration)), aes(x = Station, y = Filtration))+ geom_boxplot()

ggplot(TotalFiltration, aes(x = as.factor(year(Date)), y = Filtration))+ geom_boxplot()

#limit to 2010-present, and get rid of stations in San Pablo Bay

TotalFilt2 = filter(TotalFiltration, year(Date)>2009, Longitude > -122.2)


#ggplot(TotalFilt2, aes(x = Station, y = Turnover))+ geom_boxplot()

ggplot(TotalFilt2, aes(x = as.factor(year(Date)), y = Turnover))+ geom_boxplot()


# Load Delta EDSM shapefile and only keep SubRegions east of Carquinez Straight
sf_delta <- st_read("data/spatial/delta_subregions.shp")

#Add the regions to the dataset
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

ggplot()+
  geom_sf( data = WW_Delta)+
  geom_sf(data = sf_delta, aes(fill = SubRegion), alpha = 0.2)+
  scale_fill_discrete(guide = NULL)+
  coord_sf(ylim = c(37.8, 38.6), xlim = c(-122.2, -121.3))+
  theme_bw()

#regional, monthly data for nutrient model
Ave_filt_regions = TotalFilt_regions %>%
  group_by(Month, Year, Region) %>%
  summarize(Clam_Turnover = mean(Turnover, na.rm =T),
            Clam_Filtration = mean(Filtration, na.rm =T),
            CorbiculaBiomass = mean(CorbiculaBiomass, na.rm =T),
            PotamocorbulaBiomass = mean(PotamocorbulaBiomass, na.rm =T)) %>%
  filter(!is.na(Region))

#now the seasonal version
SeasonalAve_filt_regions = TotalFilt_regions %>%
  mutate(YearAdj = case_when(Month ==12 ~ Year+1,
                           TRUE ~ Year),
         Season = case_when(Month %in% c(1,2, 12) ~ "Winter",
                            Month %in% c(3,4,5) ~ "Spring",
                            Month %in% c(6,7,8) ~ "Summer",
                            Month %in% c(9,10,11) ~ "Fall") ) %>%
  group_by(Season, YearAdj, Region) %>%
  summarize(Clam_Turnover = mean(Turnover, na.rm =T),
            Clam_Filtration = mean(Filtration, na.rm =T),
            CorbiculaBiomass = mean(CorbiculaBiomass, na.rm =T),
            PotamocorbulaBiomass = mean(PotamocorbulaBiomass, na.rm =T)) %>%
  filter(!is.na(Region))

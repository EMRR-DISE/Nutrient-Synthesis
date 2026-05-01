#exploring zooplankton data to see which taxa might be most important

library(tidyverse)
library(zooper)
library(sf)

zoops = Zoopsynther(Data_type = "Community", Sources = c("EMP", "FMWT", "STN", "20mm", "DOP"),
                    Years = c(2010:2024)) 

zoops = mutate(zoops, Month = month(Date))

#what are the most commont taxa?

zoopCommon = filter(zoops, !Undersampled) %>%
  group_by(Taxname, Lifestage, Taxlifestage, Month, SizeClass) %>%
  summarize(CPUE = sum(CPUE))

ggplot(zoopCommon, aes(x = Month, y = CPUE, fill = Taxlifestage)) + geom_area(position = "fill") +
  facet_wrap(~SizeClass)

#now get rid of the rarer stuff

zoopstot = group_by(zoopCommon, SizeClass, Month) %>%
  mutate(tot = sum(CPUE)) %>%
  ungroup() %>%
  mutate(percent = CPUE/tot,
         Taxon = case_when(percent<0.05 ~ "Other",
                           TRUE ~ Taxlifestage)) %>%
  group_by(SizeClass, Month, Taxon) %>%
  summarize(CPUE = sum(CPUE))

ggplot(zoopstot, aes(x = Month, y = CPUE, fill = Taxon)) + geom_area(position = "fill") +
  facet_wrap(~SizeClass) + scale_fill_manual(values = c("orange", "skyblue", "cyan", "darkgreen",
                                                        "purple", "pink", "black", "yellow", "red3",
                                                        "peru", "yellowgreen", "goldenrod",
                                                        "white", "grey", "deeppink", 'seagreen', "gold",
                                                      "blue", "salmon", "darkblue", "peachpuff", "slategray",
                                                      "sienna3", "cyan3", "lightgreen", "maroon"))

#OK! That's by month. 
#now go back and add grazing rates and add it all up.

#lookup = zoops %>%
# filter(SizeClass != "Macro") %>%
#  select(Taxname, Lifestage, Taxlifestage) %>%
#  distinct()
#write.csv(lookup, "data/zooplookup.csv")

lookup = read_csv("data/zooplookup.csv")

zoops_grazing = left_join(zoops, lookup) %>%
  filter(!is.na(GrazingRate), !is.na(FunctionalGroup)) %>%
  mutate(BPUE = CPUE*Biomass_ug, Grazing = CPUE*GrazingRate) %>%
  group_by(SampleID, Latitude, Longitude, Date, Source,
           SizeClass, FunctionalGroup) %>%
  summarize(BPUE = sum(BPUE, na.rm = T), Grazing = sum(Grazing, na.rm =T))

#now add regions, calculate mean across regions and months
Regions = st_read("data/spatial/delta_subregions.shp")

zoops_regions = zoops_grazing %>%
  filter(!is.na(Latitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(crs = st_crs(Regions)) %>%
  st_join(Regions) %>%
  st_drop_geometry() %>%
  filter(!is.na(Region)) 

#grazing rate is in mL/day
#biomass is in ugC/m3

zoops_monthly = zoops_regions %>%
  mutate(Month = month(Date), Year = year(Date)) %>%
  group_by(Region, Month, Year, FunctionalGroup) %>%
  summarize(BPUE = mean(BPUE), Grazing = mean(Grazing))

write_rds(zoops_monthly, "data/processed/zoops_monthly.rds")
write_csv(zoops_monthly, "data/processed/zoops_monthly.csv")

#now seasonal

zoops_seasonal = zoops_regions %>%
  mutate(Month = month(Date), Year = year(Date), 
         YearAdj = case_when(Month ==12 ~ Year+1, TRUE ~ Year),
         Season = case_when(Month %in% c(12,1,2) ~ "Winter",
                            Month %in% c(3,4,5) ~ "Spring",
                            Month %in% c(6,7,8) ~ "Summer",
                            Month %in% c(9,10,11) ~ "Fall")) %>%
  group_by(Region, Season, YearAdj, FunctionalGroup) %>%
  summarize(BPUE = mean(BPUE), Grazing = mean(Grazing))

write_rds(zoops_seasonal, "data/processed/zoops_seasonal.rds")
write_csv(zoops_seasonal, "data/processed/zoops_seasonal.csv")

#a few quick plots to check things

ggplot(zoops_monthly, aes(x = Year, y = Grazing, color = FunctionalGroup)) +
  geom_smooth()+ facet_grid(Month~Region)

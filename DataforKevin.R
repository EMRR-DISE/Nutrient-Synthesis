#organized nutrient dataset for KEvin and the Delta Modeling group
#Rosemary Hartman
#2024-10-21

library(tidyverse)
library(discretewq)
library(deltamapr)
library(sf)
library(here)


WQ = wq(Sources = c("EMP", "NCRO", "20mm", "FMWT", "STN", "USGS_CAWSC", "USGS_SFBS"),
        Start_year = 2000, End_year = 2023)

Micro = filter(WQ, !is.na(Microcystis))
Nuts = filter(WQ, !is.na(DissNitrateNitrite))

Both = filter(WQ, !is.na(Microcystis) & !is.na(DissNitrateNitrite))


subregions = R_EDSM_Subregions_Mahardja

WQsf = filter(WQ, !is.na(Longitude)) %>%
  st_as_sf(coords = c("Longitude","Latitude"), crs = 4326) %>%
    st_transform(crs = st_crs(subregions)) %>%
  st_join(subregions) %>%
  mutate(SubRegion = case_when(is.na(SubRegion) ~ "Really Far South",
                        TRUE ~ SubRegion))


ggplot()+
  geom_sf(data = WW_Delta)+
  geom_sf(data = WQsf, aes(color = SubRegion))


#monthly means

WQmonthly = WQsf %>%
  st_drop_geometry() %>%
  mutate(Month = month(Date), Year = year(Date)) %>%
  group_by(SubRegion, Month, Year) %>%
  summarize(Microcystis = mean(Microcystis, na.rm = T), 
            Secchi = mean(Secchi, na.rm = T),
            DissNitrateNitrite = mean(DissNitrateNitrite, na.rm =T),
            DissAmmonia = mean(DissAmmonia, na.rm = T),
            pH = mean(pH, na.rm =T),
            Chl = mean(Chlorophyll, na.rm =T),
            Conductivity = mean(Conductivity, na.rm =T),
            Temperature = mean(Temperature, na.rm =T),
            DissolvedOxygen = mean(DissolvedOxygen, na.rm =T),
             TurbidityNTU = mean(TurbidityNTU, na.rm =T),
            TKN = mean(TKN, na.rm =T),
            Salinity = mean(Salinity, na.rm =T),
            DissOrthophos = mean(DissOrthophos, na.rm =T),
            TotPhos = mean(TotPhos, na.rm =T)) %>%
  mutate(across(c(Microcystis:TotPhos), ~ifelse(is.nan(.), NA, .)))

WQmonthlysub = filter(WQmonthly, !is.na(Microcystis), !is.na(DissNitrateNitrite))


#what does this look like?

ggplot(WQmonthlysub, aes(x = DissNitrateNitrite, y = Microcystis))+
  geom_point()

write.csv(WQmonthlysub, here("data/raw/discretewq_monthlymean.csv"))

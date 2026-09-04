#figure out what is going on with the tidal parr data

library(tidyverse)
library(readxl)
library(here)

fp_raw <- here("data/raw")

stations = read_csv(file.path(fp_raw, "Bryte Internal Station List.csv")) %>%
  rename(Station = station.wq.bryte.internal)

TidalParr1 = read_excel(file.path(fp_raw, "Tidal Parr 2020_WQ_Bryte.xlsx")) %>%
  mutate(Station = str_trim(str_remove(`Station Number`, "\\(.*\\)")),
         CollectionDate = mdy_hm(`Collection Date`))
TidalParr2 = read_csv(file.path(fp_raw, "WQ Data Tidal Parr 2018_2019.csv")) %>%
  mutate(`Rpt Limit` = as.character(`Rpt Limit`), Station = `Station Name`,
CollectionDate = mdy_hm(`Collection Date`))

WDLtidalparr = read_excel(file.path(fp_raw, "ALLTidalParr_download02192025.xlsx")) %>%
  mutate(CollectionDate = mdy_hm(`Collection Date`))

Test = bind_rows(TidalParr1, TidalParr2) %>%
  select(`Station`,`Sample Code`, `CollectionDate`) %>%
  distinct() 

Test2 = bind_rows(TidalParr1, TidalParr2) %>%
  select(`Station`, `CollectionDate`) %>%
  distinct() %>%
  group_by(CollectionDate) %>%
  summarize(N = n()) %>%
  filter(N>1)

test3 = filter(bind_rows(TidalParr1, TidalParr2), CollectionDate %in% Test2$CollectionDate)

TidalParrstations = bind_rows(TidalParr1, TidalParr2) %>%
    select(`Station`, `Sample Code`, `CollectionDate`) %>%
  distinct()  %>%
  full_join(WDLtidalparr, by = c("CollectionDate", "Sample Code"))

foo3 = unique(WDLtidalparr$`CollectionDate`)
brett = c(unique(TidalParr1$`CollectionDate`), unique(TidalParr2$`CollectionDate`))

notthere = filter(WDLtidalparr, !`CollectionDate` %in% brett)
there = filter(TidalParrstations, !`CollectionDate` %in% unique(WDLtidalparr$`CollectionDate`))
#ok, just misisng the equipment blanks and maybe two samples

TidalParrFinal = TidalParrstations %>%
  select(-`Station Number`, -`Long Station Name`, -`Short Station Name`) %>%
  rename(`Station Number` = Station)

write.csv(TidalParrFinal, here("data/intermediate/TidalParrFinal.csv"))

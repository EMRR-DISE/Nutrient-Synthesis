# DWR Drought Synthesis
# Purpose: Retrieve Delta Inflow from DAYFLOW model for water years 1997-2024
# and save copy in data/external for continued processing
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

library(readr)
library(tibble)
library(purrr)
library(here)

# Download data from CNRA data portal: <https://data.cnra.ca.gov/dataset/dayflow>
dayflow_1997_2023 <- read_csv(
  "https://data.cnra.ca.gov/dataset/06ee2016-b138-47d7-9e85-f46fae674536/resource/21c377fe-53b8-4bd6-9e1f-2025221be095/download/dayflow-results-1997-2023.csv"
)
dayflow_2024 <- read_csv(
  "https://data.cnra.ca.gov/dataset/06ee2016-b138-47d7-9e85-f46fae674536/resource/6a7cb172-fb16-480d-9f4f-0322548fee83/download/dayflowcalculations2024.csv"
)

# Save local copies of data
lst(dayflow_1997_2023, dayflow_2024) %>%
  iwalk(\(x, idx) {
    saveRDS(x, file = here("data/external", paste0(idx, ".rds")))
  })

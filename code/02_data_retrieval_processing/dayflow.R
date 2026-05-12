# DWR Drought Synthesis
# Purpose: Retrieve Delta Inflow from DAYFLOW model for water years 1997-2024
# and process it for integration with other data for our analyses
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

get_dayflow <- function() {
  # Download data from CNRA data portal: <https://data.cnra.ca.gov/dataset/dayflow>
  dayflow_1997_2023 <- readr::read_csv(
    "https://data.cnra.ca.gov/dataset/06ee2016-b138-47d7-9e85-f46fae674536/resource/21c377fe-53b8-4bd6-9e1f-2025221be095/download/dayflow-results-1997-2023.csv"
  )
  dayflow_2024 <- readr::read_csv(
    "https://data.cnra.ca.gov/dataset/06ee2016-b138-47d7-9e85-f46fae674536/resource/6a7cb172-fb16-480d-9f4f-0322548fee83/download/dayflowcalculations2024.csv"
  )

  # Clean up and combine data
  df_dayflow <- dayflow_1997_2023 |>
    dplyr::mutate(Date = lubridate::mdy(Date)) |>
    dplyr::bind_rows(dayflow_2024) |>
    dplyr::select(Date, SAC, SJR, TOT) |>
    dplyr::mutate(
      Year = lubridate::year(Date),
      Month = lubridate::month(Date)
    ) |>
    # Remove data prior to 2009 and after 2024
    dplyr::filter(Year %in% 2009:2024)

  return(df_dayflow)
}

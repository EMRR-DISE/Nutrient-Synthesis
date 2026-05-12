# DWR Nutrient Synthesis
# Purpose: Process vegetation index data collected by NCRO for integration with other data
# for our analyses
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

# NOTE: Requires functions from "code/01_functions/global_funcs.R"

get_veg_index <- function() {
  # Import data:
  # Vegetation Index data
  df_veg_index <- readr::read_csv(
    here::here("data/raw/wqes_veg.csv"),
    col_select = c(StationCode, FldDate, FldObsVegSub, FldObsVegSurf)
  )

  # Station Info
  df_veg_index_stations <- readr::read_csv(
    here::here("data/raw/wqes_veg_stations.csv")
  )

  # Prepare data:
  df_veg_index_stations_c <- df_veg_index_stations |>
    dplyr::distinct(
      StationCode,
      Latitude = `Latitude (WGS84)`,
      Longitude = `Longitude (WGS84)`
    )

  df_veg_index_c <- df_veg_index |>
    dplyr::mutate(
      StationCode,
      Date = lubridate::date(FldDate),
      Month = lubridate::month(Date),
      Year = lubridate::year(Date),
      VegIndex_Subm = FldObsVegSub,
      VegIndex_Surf = FldObsVegSurf,
      .keep = "none"
    ) |>
    # Remove records were all values are missing, remove data collected after 2024
    dplyr::filter(
      !dplyr::if_all(tidyselect::starts_with("VegIndex"), is.na),
      Year <= 2024
    ) |>
    # Convert index levels to numeric values
    dplyr::mutate(
      dplyr::across(
        tidyselect::starts_with("VegIndex"),
        \(x) {
          dplyr::recode_values(
            x,
            "Not Visible" ~ 0,
            "Low" ~ 1,
            c("medium", "Medium") ~ 2,
            "High" ~ 3,
            "Extreme" ~ 4
          )
        }
      )
    ) |>
    # Assign regions to the stations
    dplyr::left_join(
      df_veg_index_stations_c,
      by = dplyr::join_by(StationCode)
    ) |>
    assign_regions() |>
    # Remove a few duplicated records
    dplyr::distinct()

  return(df_veg_index_c)
}

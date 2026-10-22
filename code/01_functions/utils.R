# DWR Nutrient Synthesis
# Purpose: General utility functions
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

# Assign regions and remove any data outside of our regions of interest
# df_data requires Latitude and Longitude columns
# Default CRS of station coordinates is 4326 (WGS 84 geographic coordinate reference system)
# Removes Latitude and Longitude columns after assigning regions
assign_regions <- function(df_data, crs = 4326) {
  # Load regions shapefile
  sf_delta <-
    sf::read_sf(here::here("data/spatial/delta_subregions.shp")) |>
    dplyr::select(-SubRegion)

  df_data |>
    sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) |>
    sf::st_transform(crs = sf::st_crs(sf_delta)) |>
    sf::st_join(sf_delta, join = sf::st_intersects) |>
    # Remove any data outside our regions of interest
    dplyr::filter(!is.na(Region)) |>
    sf::st_drop_geometry()
}

# Assign adjusted calendar year from a year and numeric month. An adjusted calendar year is
# defined as December-November, with December of the previous calendar year included with
# the following year
assign_year_adj <- function(month, year) {
  ifelse(month == 12, year + 1, year)
}

# Assign season as a factor from a numeric month
assign_season <- function(month) {
  factor(
    dplyr::recode_values(
      month,
      3:5 ~ "Spring",
      6:8 ~ "Summer",
      9:11 ~ "Fall",
      c(12, 1, 2) ~ "Winter"
    ),
    levels = c("Winter", "Spring", "Summer", "Fall")
  )
}

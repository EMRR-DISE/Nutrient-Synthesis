# DWR Nutrient Synthesis
# Purpose: Process spatial data used in project and save copy in data/spatial for continued use
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

library(here)
library(stringr)
library(readr)
library(dplyr)
library(sf)

# Install devtools if its not installed already
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# Make sure we are using `deltamapr` version 1.0.1, commit fe34697b3d1aaa2945bbfc647582a19e251abf67
deltamapr_ref_target <- "fe34697"
if (requireNamespace("deltamapr", quietly = TRUE)) {
  deltamapr_src <- devtools::package_info(
    "deltamapr",
    dependencies = FALSE
  )$source

  deltamapr_ref <- stringr::str_sub(
    deltamapr_src,
    start = stringr::str_locate(deltamapr_src, "@")[1] + 1,
    end = stringr::str_locate(deltamapr_src, "@")[1] + 7
  )

  if (deltamapr_ref != deltamapr_ref_target) {
    devtools::install_github(
      "InteragencyEcologicalProgram/deltamapr",
      ref = deltamapr_ref_target
    )
  }
} else {
  devtools::install_github(
    "InteragencyEcologicalProgram/deltamapr",
    ref = deltamapr_ref_target
  )
}

library(deltamapr)

# Import region assignments
df_regions <- read_csv(here("data/raw/region_assignments.csv"))

# Load Delta EDSM shapefile and only keep SubRegions east of Carquinez Straight
sf_delta <- R_EDSM_Subregions_Mahardja_FLOAT %>%
  filter(
    !SubRegion %in%
      c(
        "Carquinez Strait",
        "Lower Napa River",
        "San Francisco Bay",
        "San Pablo Bay",
        "South Bay",
        "Upper Napa River"
      )
  ) %>%
  select(SubRegion) %>%
  # Add region assignments to the SubRegion shapefile
  left_join(df_regions, by = join_by(SubRegion)) %>%
  relocate(Region, .after = SubRegion)

# Export Delta SubRegions/Regions shapefile
sf_delta %>% write_sf(here("data/spatial/delta_subregions.shp"))

# DWR Drought Synthesis
# Purpose: Retrieve North Delta clam data collected by USGS for years 2015-2018
# and save copy in data/external for continued processing
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

library(sbtools)
library(rlang)
library(purrr)
library(stringr)
library(here)

# Function to download specified data entities from a Science Base item and save files to data/external
get_scibase_data <- function(item_id, entity_regex) {
  # Compile all data entities for specified Science Base item ID
  df_sb_files <- item_list_files(item_id)
  inform(c(
    "i" = paste0(
      "Data entities for ",
      item_id,
      " include:\n",
      paste(df_sb_files$fname, collapse = "\n"),
      "\n"
    )
  ))

  # Subset to desired data entities
  sb_ent_sub <- str_subset(df_sb_files$fname, entity_regex)
  inform(c(
    "i" = paste0(
      "Downloading data entities:\n",
      paste(sb_ent_sub, collapse = "\n"),
      "\n"
    )
  ))

  # Proceed with downloading desired data entities from Science Base item
  map(
    sb_ent_sub,
    \(x) {
      item_file_download(
        sb_id = item_id,
        names = x,
        destinations = here("data/external", x),
        overwrite_file = TRUE
      )
    }
  )

  inform(c("v" = "All files successfully downloaded to 'data/external'"))
}

# Download data to data/external
# 2015-2018 published data: https://www.sciencebase.gov/catalog/item/5e9e225b82cefae35a106f5e
get_scibase_data(
  item_id = "5e9e225b82cefae35a106f5e",
  entity_regex = "BioRecGR"
)

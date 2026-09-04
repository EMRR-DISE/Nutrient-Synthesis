# Global functions for the DWR Nutrient Synthesis

# Get data entity names for specified EDI ID
get_edi_data_entities <- function(edi_id) {
  df_data_ent <- EDIutils::read_data_entity_names(edi_id)
  rlang::inform(c(
    "i" = paste0(
      "Data entities for ",
      edi_id,
      " include:\n",
      paste(df_data_ent$entityName, collapse = "\n"),
      "\n"
    )
  ))
  return(df_data_ent$entityName)
}

# Download specified data entities from an EDI package and save raw bytes files to a
# temporary directory
get_edi_data <- function(edi_id, entity_names) {
  df_data_ent <- EDIutils::read_data_entity_names(edi_id)
  df_data_ent_filt <- dplyr::filter(df_data_ent, entityName %in% entity_names)

  # Provide message on which data entities will be downloaded
  rlang::inform(c(
    "i" = paste0(
      "Downloading data entities:\n",
      paste(df_data_ent_filt$entityName, collapse = "\n"),
      "\n"
    )
  ))

  ls_data_raw <-
    purrr::map(df_data_ent_filt$entityId, \(x) {
      EDIutils::read_data_entity(edi_id, entityId = x)
    }) |>
    rlang::set_names(df_data_ent_filt$entityName)

  for (i in 1:length(ls_data_raw)) {
    file_raw <- file.path(tempdir(), glue::glue("{names(ls_data_raw)[i]}.bin"))
    con <- file(file_raw, "wb")
    writeBin(ls_data_raw[[i]], con)
    close(con)
  }

  rlang::inform(c(
    "v" = "All files successfully downloaded to temporary directory"
  ))
}

# Download specified data entities from a Science Base item and save files to a
# temporary directory
get_scibase_data <- function(item_id, entity_regex) {
  # Compile all data entities for specified Science Base item ID
  df_sb_files <- sbtools::item_list_files(item_id)
  rlang::inform(c(
    "i" = paste0(
      "Data entities for ",
      item_id,
      " include:\n",
      paste(df_sb_files$fname, collapse = "\n"),
      "\n"
    )
  ))

  # Subset to desired data entities
  sb_ent_sub <- stringr::str_subset(df_sb_files$fname, entity_regex)
  rlang::inform(c(
    "i" = paste0(
      "Downloading data entities:\n",
      paste(sb_ent_sub, collapse = "\n"),
      "\n"
    )
  ))

  # Proceed with downloading desired data entities from Science Base item
  purrr::walk(
    sb_ent_sub,
    \(x) {
      sbtools::item_file_download(
        sb_id = item_id,
        names = x,
        destinations = file.path(tempdir(), x),
        overwrite_file = TRUE
      )
    }
  )

  rlang::inform(c(
    "v" = "All files successfully downloaded to temporary directory"
  ))
}

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

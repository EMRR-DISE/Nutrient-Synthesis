# DWR Nutrient Synthesis
# Purpose: Retrieve clam data and process it for integration with other data for our analyses
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

# NOTE: Requires functions from "code/01_functions/global_funcs.R"

get_clam_data <- function() {
  # Download data ------------------------------------------------------------------------------

  # Clam grazing rate data from the drought synthesis EDI package
  edi_id_drt <- "edi.1653.1"
  edi_data_ent_drt <- get_edi_data_entities(edi_id_drt)
  regex_drt_clams <- "_clams|\\s{1}Clam\\s{1}"
  edi_data_ent_drt_clams <- stringr::str_subset(
    edi_data_ent_drt,
    regex_drt_clams
  )
  get_edi_data(edi_id_drt, edi_data_ent_drt_clams)

  # Additional Suisun Marsh clam data from the SMSCG data package on EDI
  edi_id_smscg <- "edi.876.8"
  edi_data_ent_smscg <- get_edi_data_entities(edi_id_smscg)
  edi_data_ent_smscg_clams <- stringr::str_subset(
    edi_data_ent_smscg,
    "smscg_clams"
  )
  get_edi_data(edi_id_smscg, edi_data_ent_smscg_clams)

  # North Delta clam data collected by USGS for years 2015-2018 on sciencebase
  # 2015-2018 published data: https://www.sciencebase.gov/catalog/item/5e9e225b82cefae35a106f5e
  regex_usgs_clams <- "BioRecGR"
  get_scibase_data(
    item_id = "5e9e225b82cefae35a106f5e",
    entity_regex = regex_usgs_clams
  )

  # Import data --------------------------------------------------------------------------------

  temp_files <- list.files(tempdir(), full.names = TRUE)

  # Clam data from EDI
  edi_clams <- c(edi_data_ent_drt_clams, edi_data_ent_smscg_clams)

  ls_edi_clams <-
    purrr::map(edi_clams, \(x) stringr::str_subset(temp_files, x)) |>
    purrr::map(readr::read_csv) |>
    rlang::set_names(
      stringr::str_to_lower(stringr::str_replace_all(edi_clams, "\\s", "_"))
    )

  # North Delta clam data collected by USGS
  ndf_usgs_clams <-
    tibble::tibble(
      fp = stringr::str_subset(temp_files, regex_usgs_clams),
      year = purrr::map_int(
        fp,
        \(x) as.numeric(stringr::str_extract(basename(x), "(?<=Delta)\\d{4}"))
      ),
      skip_row_num = dplyr::if_else(year == 2015, 0, 1),
      df_data = purrr::map2(
        fp,
        skip_row_num,
        \(x, y) readxl::read_excel(path = x, skip = y)
      )
    )

  # Prepare data -------------------------------------------------------------------------------

  # Clam grazing rate data from the drought synthesis EDI package
  df_drt_clams <- ls_edi_clams$grts_clams |>
    dplyr::rename(Station = SiteID) |>
    dplyr::mutate(
      Date = lubridate::ymd(paste(Year, Month, "15", sep = "-")),
      Filtration_Rate = Turnover_Rate * Depth
    ) |>
    dplyr::bind_rows(ls_edi_clams$emp_clam_data) |>
    dplyr::group_by(Station, Date, Latitude, Longitude, Depth) |>
    dplyr::summarize(
      Clam_Filtration = sum(Filtration_Rate, na.rm = TRUE),
      Clam_Turnover = sum(Turnover_Rate, na.rm = TRUE),
      CorbiculaBiomass = sum(Biomass[which(Clam == "CF")]),
      PotamocorbulaBiomass = sum(Biomass[which(Clam == "PA")]),
      .groups = "drop_last"
    ) |>
    # Average values collected at different depths but at same station and date
    dplyr::summarize(
      dplyr::across(
        c(tidyselect::starts_with("Clam_"), tidyselect::ends_with("Biomass")),
        \(x) dplyr::na_if(mean(x, na.rm = TRUE), NaN)
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      Year = lubridate::year(Date),
      Month = lubridate::month(Date),
      Longitude = dplyr::if_else(Longitude > 0, Longitude * -1, Longitude)
    )

  # Suisun Marsh clam data from the SMSCG data package on EDI
  df_smscg_clams <- ls_edi_clams$smscg_clams |>
    dplyr::mutate(
      Year,
      Month = lubridate::month(Date),
      Station,
      Date,
      Latitude = North_decimal_degrees,
      Longitude = West_decimal_degrees,
      CorbiculaBiomass = Corbicula_AFDM_g_per_m2,
      PotamocorbulaBiomass = Potamocorbula_AFDM_g_per_m2,
      Clam_Filtration = Total_filtration_rate_m3_per_m2_per_day,
      Clam_Turnover = Total_grazing_turnover_per_day,
      .keep = "none"
    )

  # North Delta clam data collected by USGS
  df_usgs_clams <- ndf_usgs_clams |>
    dplyr::select(year, df_data) |>
    tibble::deframe() |>
    purrr::map(\(x) dplyr::rename_with(x, stringr::str_to_title)) |>
    purrr::map_at(
      "2015",
      \(x) {
        dplyr::mutate(x, Station = as.character(Station)) |>
          dplyr::rename(Year_data = Year)
      }
    ) |>
    purrr::list_rbind(names_to = "Year_fp") |>
    dplyr::mutate(
      Year = dplyr::if_else(!is.na(Year_data), Year_data, as.numeric(Year_fp)),
      Date = lubridate::ymd(paste(Year, Month, "15", sep = "-")),
      Filtration_Rate = Gr,
      Turnover_Rate = Grto,
      Latitude = Lat,
      Longitude = Long
    ) |>
    dplyr::group_by(Station, Date, Latitude, Longitude)

  # Summarize Filtration and Turnover rates separately from the CF and PA biomass
  df_usgs_clams_rates <- df_usgs_clams |>
    dplyr::summarize(
      Clam_Filtration = sum(Filtration_Rate, na.rm = T),
      Clam_Turnover = sum(Turnover_Rate),
      .groups = "drop"
    )

  df_usgs_clams_biomass <- df_usgs_clams |>
    dplyr::filter(Clam %in% c("CF", "PA")) |>
    dplyr::group_by(Clam, .add = TRUE) |>
    dplyr::summarize(Biomass = sum(Biomass), .groups = "drop") |>
    tidyr::pivot_wider(names_from = Clam, values_from = Biomass) |>
    dplyr::rename(CorbiculaBiomass = CF, PotamocorbulaBiomass = PA)

  df_usgs_clams_c <-
    dplyr::full_join(
      df_usgs_clams_rates,
      df_usgs_clams_biomass,
      by = dplyr::join_by(Station, Date, Latitude, Longitude)
    ) |>
    dplyr::mutate(Year = lubridate::year(Date), Month = lubridate::month(Date))

  # Combine all clams datasets and finish preparing data
  df_clams_all <-
    dplyr::bind_rows(df_drt_clams, df_smscg_clams, df_usgs_clams_c) |>
    dplyr::filter(Year %in% 2009:2024) |>
    assign_regions()

  return(df_clams_all)
}
